-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- ============================================================================
-- Authors:					Patrick Lehmann
-- 
-- Package:					This VHDL package declares new physical types and their
--									conversion functions.
--
-- Description:
-- ------------------------------------
--		For detailed documentation see below.
--	
--		NAMING CONVENTION:
--			t - time
--			p - period
--			d - delay
--			f - frequency
--			br - baud rate
--			vec - vector
--			
--		ATTENTION:
--			This package is not supported by Xilinx Synthese Tools prior to 14.7!
--			
--			It was successfully tested with:
--				- Xilinx Synthesis Tool (XST) 14.7 and Xilinx ISE Simulator (iSim) 14.7
--				- Quartus II 13.1
--				- QuestaSim 10.0d
--				- GHDL 0.31
--
--			Tool chains with known issues:
--				- Xilinx Vivado	Synthesis 2014.4
--
--			Untested tool chains
--				- Xilinx Vivado Simulator (xSim) 2014.4
--		
-- License:
-- ============================================================================
-- Copyright 2007-2015 Technische Universitaet Dresden - Germany,
--										 Chair for VLSI-Design, Diagnostics and Architecture
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--		http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-- ============================================================================

library IEEE;
use			IEEE.math_real.all;

library PoC;
use			PoC.config.all;
use			PoC.utils.all;
use			PoC.strings.all;


package physical is
	
	type FREQ is range 0 to INTEGER'high units
		Hz;
		kHz = 1000 Hz;
		MHz = 1000 kHz;
		GHz = 1000 MHz;
--	THz = 1000 GHz;
	end units;

	type BAUD is range 0 to INTEGER'high units
		Bd;
		kBd = 1000 Bd;
		MBd = 1000 kBd;
		GBd = 1000 MBd;
	end units;

	type MEMORY is range 0 to INTEGER'high units
		Byte;
		KiB = 1024 Byte;
		MiB = 1024 KiB;
		GiB = 1024 MiB;
--	TiB = 1024 GiB;
	end units;
	
	--
	type		T_TIMEVEC						is array(NATURAL range <>) of TIME;
	type		T_FREQVEC						is array(NATURAL range <>) of FREQ;
	type		T_BAUDVEC						is array(NATURAL range <>) of BAUD;
	type		T_MEMVEC						is array(NATURAL range <>) of MEMORY;
	
	-- TODO
	constant C_PHYSICAL_REPORT_TIMING_DEVIATION		: BOOLEAN		:= TRUE;
	
	-- conversion functions
	function to_time(f : FREQ)	return TIME;
	function to_freq(p : TIME)	return FREQ;
	function to_freq(br : BAUD)	return FREQ;
	function to_baud(str : STRING)	return BAUD;

	-- if-then-else
	function ite(cond : BOOLEAN; value1 : TIME;	value2 : TIME)			return TIME;
	function ite(cond : BOOLEAN; value1 : FREQ;	value2 : FREQ)			return FREQ;
	function ite(cond : BOOLEAN; value1 : BAUD;	value2 : BAUD)			return BAUD;
	function ite(cond : BOOLEAN; value1 : MEMORY;	value2 : MEMORY)	return MEMORY;
	
	-- min/ max for 2 arguments
	function min(arg1 : TIME; arg2 : TIME) return TIME;						-- Calculates: min(arg1, arg2) for times
	function min(arg1 : FREQ; arg2 : FREQ) return FREQ;						-- Calculates: min(arg1, arg2) for frequencies
	function min(arg1 : BAUD; arg2 : BAUD) return BAUD;						-- Calculates: min(arg1, arg2) for symbols per second
	function min(arg1 : MEMORY; arg2 : MEMORY) return MEMORY;			-- Calculates: min(arg1, arg2) for memory
	
	function max(arg1 : TIME; arg2 : TIME) return TIME;						-- Calculates: max(arg1, arg2) for times
	function max(arg1 : FREQ; arg2 : FREQ) return FREQ;						-- Calculates: max(arg1, arg2) for frequencies
	function max(arg1 : BAUD; arg2 : BAUD) return BAUD;						-- Calculates: max(arg1, arg2) for symbols per second
	function max(arg1 : MEMORY; arg2 : MEMORY) return MEMORY;			-- Calculates: max(arg1, arg2) for memory
	
	-- min/max/sum as vector aggregation
	function min(vec : T_TIMEVEC)	return TIME;										-- Calculates: min(vec) for a time vector
	function min(vec : T_FREQVEC)	return FREQ;										-- Calculates: min(vec) for a frequency vector
	function min(vec : T_BAUDVEC)	return BAUD;										-- Calculates: min(vec) for a baud vector
	function min(vec : T_MEMVEC)	return MEMORY;									-- Calculates: min(vec) for a memory vector
	
	function max(vec : T_TIMEVEC)	return TIME;										-- Calculates: max(vec) for a time vector
	function max(vec : T_FREQVEC)	return FREQ;										-- Calculates: max(vec) for a frequency vector
	function max(vec : T_BAUDVEC)	return BAUD;										-- Calculates: max(vec) for a baud vector
	function max(vec : T_MEMVEC)	return MEMORY;									-- Calculates: max(vec) for a memory vector
	
	-- QUESTION: some sum functions are not meaningful -> orthogonal function/type system
	function sum(vec : T_TIMEVEC)	return TIME;										-- Calculates: sum(vec) for a time vector
	function sum(vec : T_FREQVEC)	return FREQ;										-- Calculates: sum(vec) for a frequency vector
	function sum(vec : T_BAUDVEC)	return BAUD;										-- Calculates: sum(vec) for a baud vector
	function sum(vec : T_MEMVEC)	return MEMORY;									-- Calculates: sum(vec) for a memory vector
	
	-- convert standard types (NATURAL, REAL) to time (TIME)
	function fs2Time(t_fs : NATURAL)		return TIME;
	function ps2Time(t_ps : NATURAL)		return TIME;
	function ns2Time(t_ns : NATURAL)		return TIME;
	function us2Time(t_us : NATURAL)		return TIME;
	function ms2Time(t_ms : NATURAL)		return TIME;
	function sec2Time(t_sec : NATURAL)	return TIME;
	
	function fs2Time(t_fs : REAL)				return TIME;
	function ps2Time(t_ps : REAL)				return TIME;
	function ns2Time(t_ns : REAL)				return TIME;
	function us2Time(t_us : REAL)				return TIME;
	function ms2Time(t_ms : REAL)				return TIME;
	function sec2Time(t_sec : REAL)			return TIME;
	
	-- convert standard types (NATURAL, REAL) to period (TIME)
	function Hz2Time(f_Hz : NATURAL)		return TIME;
	function kHz2Time(f_kHz : NATURAL)	return TIME;
	function MHz2Time(f_MHz : NATURAL)	return TIME;
	function GHz2Time(f_GHz : NATURAL)	return TIME;
--	function THz2Time(f_THz : NATURAL)	return TIME;

	function Hz2Time(f_Hz : REAL)				return TIME;
	function kHz2Time(f_kHz : REAL) 		return TIME;
	function MHz2Time(f_MHz : REAL) 		return TIME;
	function GHz2Time(f_GHz : REAL) 		return TIME;
--	function THz2Time(f_THz : REAL) 		return TIME;
	
	-- convert standard types (NATURAL, REAL) to frequency (FREQ)
	function Hz2Freq(f_Hz : NATURAL)		return FREQ;
	function kHz2Freq(f_kHz : NATURAL)	return FREQ;
	function MHz2Freq(f_MHz : NATURAL)	return FREQ;
	function GHz2Freq(f_GHz : NATURAL)	return FREQ;
--	function THz2Freq(f_THz : NATURAL)	return FREQ;
	
	function Hz2Freq(f_Hz : REAL)				return FREQ;
	function kHz2Freq(f_kHz : REAL)			return FREQ;
	function MHz2Freq(f_MHz : REAL)			return FREQ;
	function GHz2Freq(f_GHz : REAL)			return FREQ;
--	function THz2Freq(f_THz : REAL)			return FREQ;
	
	-- convert physical types to standard type (REAL)
	function to_real(t : TIME;			scale : TIME)		return REAL;
	function to_real(f : FREQ;			scale : FREQ)		return REAL;
	function to_real(br : BAUD;			scale : BAUD)		return REAL;
	function to_real(mem : MEMORY;	scale : MEMORY)	return REAL;
	
	-- convert physical types to standard type (INTEGER)
	function to_int(t : TIME;			scale : TIME;		RoundingStyle : T_ROUNDING_STYLE := ROUND_TO_NEAREST)	return INTEGER;
	function to_int(f : FREQ;			scale : FREQ;		RoundingStyle : T_ROUNDING_STYLE := ROUND_TO_NEAREST)	return INTEGER;
	function to_int(br : BAUD;		scale : BAUD;		RoundingStyle : T_ROUNDING_STYLE := ROUND_TO_NEAREST)	return INTEGER;
	function to_int(mem : MEMORY;	scale : MEMORY;	RoundingStyle : T_ROUNDING_STYLE := ROUND_UP)					return INTEGER;
	
	-- calculate needed counter cycles to achieve a given 1. timing/delay and 2. frequency/period
	function TimingToCycles(Timing : TIME; Clock_Period			: TIME; RoundingStyle : T_ROUNDING_STYLE := ROUND_UP) return NATURAL;
	function TimingToCycles(Timing : TIME; Clock_Frequency	: FREQ; RoundingStyle : T_ROUNDING_STYLE := ROUND_UP) return NATURAL;
	
	function CyclesToDelay(Cycles : NATURAL; Clock_Period			: TIME) return TIME;
	function CyclesToDelay(Cycles : NATURAL; Clock_Frequency	: FREQ) return TIME;
	
	-- convert and format physical types to STRING
	function to_string(t : TIME; precision : NATURAL)			return STRING;
	function to_string(f : FREQ; precision : NATURAL)			return STRING;
	function to_string(br : BAUD; precision : NATURAL)		return STRING;
	function to_string(mem : MEMORY; precision : NATURAL)	return STRING;
end physical;


package body physical is

	-- iSim 14.7 does not support fs in simulation (fs values are converted to 0 ps)
	function MinimalTimeResolutionInSimulation return TIME is
	begin
		if		(1 fs > 0 sec) then	return 1 fs;
		elsif	(1 ps > 0 sec) then	return 1 ps;
		elsif	(1 ns > 0 sec) then	return 1 ns;
		elsif	(1 us > 0 sec) then	return 1 us;
		elsif	(1 ms > 0 sec) then	return 1 ms;
		else											return 1 sec;
		end if;
	end function;

	-- real division for physical types
	-- ===========================================================================
	function div(a : TIME; b : TIME) return REAL is
		constant MTRIS	: TIME		:= MinimalTimeResolutionInSimulation;
	begin
		if	(a < 1 us) then
			return real(a / MTRIS) / real(b / MTRIS);
		elsif (a < 1 ms) then
			return real(a / (1000 * MTRIS)) / real(b / MTRIS) * 1000.0;
		elsif (a < 1 sec) then
			return real(a / (1000000 * MTRIS)) / real(b / MTRIS) * 1000000.0;
		else
			return real(a / (1000000000 * MTRIS)) / real(b / MTRIS) * 1000000000.0;
		end if;
	end function;
	
	function div(a : FREQ; b : FREQ) return REAL is
	begin
		return real(a / 1 Hz) / real(b / 1 Hz);
	end function;
	
	function div(a : BAUD; b : BAUD) return REAL is
	begin
		return real(a / 1 Bd) / real(b / 1 Bd);
	end function;
	
	function div(a : MEMORY; b : MEMORY) return REAL is
	begin
		return real(a / 1 Byte) / real(b / 1 Byte);
	end function;

	-- conversion functions
	-- ===========================================================================
	function to_time(f : FREQ) return TIME is
		variable res : TIME;
	begin
		if		(f < 1 kHz) then res := div(1  Hz, f) * 1 sec;
		elsif (f < 1 MHz) then res := div(1 kHz, f) * 1 ms;
		elsif (f < 1 GHz) then res := div(1 MHz, f) * 1 us;
--	elsif (f < 1 THz) then res := div(1 GHz, f) * 1 ns;
		else										 res := div(1 GHz, f) * 1 ns;
--	else										 res := div(1 THz, f) * 1 ps;
		end if;

		if (POC_VERBOSE = TRUE) then
			report "to_time: f= " & to_string(f, 3) & "  return " & to_string(res, 3) severity note;
		end if;
		return res;
	end function;

	function to_freq(p : TIME) return FREQ is
		variable res : FREQ;
	begin
--	if		(p < 1 ps)	then res := div(1 fs, p) * 1 THz;
		if		(p < 1 ns)	then res := div(1 ps, p) * 1 GHz;
--	elsif (p < 1 ns)	then res := div(1 ps, p) * 1 GHz;
		elsif (p < 1 us)	then res := div(1 ns, p) * 1 MHz;
		elsif (p < 1 ms)	then res := div(1 us, p) * 1 kHz;
		elsif (p < 1 sec) then res := div(1 ms, p) * 1  Hz;
		else report "to_freq: input period exceeds output frequency scale." severity failure;
		end if;
		if (POC_VERBOSE = TRUE) then
			report "to_freq: p= " & to_string(p, 3) & "  return " & to_string(res, 3) severity note;
		end if;
		return res;
	end function;
	
	function to_freq(br : BAUD) return FREQ is
		variable res : FREQ;
	begin
		if		(br < 1 kBd) then res := div(br, 1 Bd)	* 1  Hz;
		elsif	(br < 1 MBd) then res := div(br, 1 kBd) * 1 kHz;
		elsif	(br < 1 GBd) then res := div(br, 1 MBd) * 1 MHz;
		else											res := div(br, 1 GBd) * 1 GHz;
		end if;

		if (POC_VERBOSE = TRUE) then
			report "to_freq: br= " & to_string(br, 3) & "  return " & to_string(res, 3) severity note;
		end if;
		return res;
	end function;
	
	function to_baud(str : STRING) return BAUD is
		variable pos		: INTEGER;
		variable int		: NATURAL;
		variable base		: POSITIVE;
		variable frac		: NATURAL;
		variable digits	: NATURAL;
	begin
		pos			:= str'low;
		int			:= 0;
		frac		:= 0;
		digits	:= 0;
		-- read integer part
		for i in pos to str'high loop
			if (chr_isDigit(str(i)) = TRUE) then		int := int * 10 + to_digit_dec(str(i));
			elsif (str(i) = '.') then								pos	:= -i;	exit;
			elsif (str(i) = ' ') then								pos	:= i;		exit;
			else																		pos := 0;		exit;
			end if;
		end loop;
		-- read fractional part
		if ((pos < 0) and (-pos < str'high)) then
			for i in -pos+1 to str'high loop
				if ((frac = 0) and (str(i) = '0')) then	next;
				elsif (chr_isDigit(str(i)) = TRUE) then	frac	:= frac * 10 + to_digit_dec(str(i));
				elsif (str(i) = ' ') then								digits	:= i + pos - 1;	pos	:= i;	exit;
				else																														pos	:= 0;	exit;
				end if;
			end loop;
		end if;
		-- abort if format is unknown
		if (pos = 0) then report "to_baud: Unknown format" severity FAILURE;	end if;
		-- parse unit
		pos := pos + 1;
		if ((pos + 1 = str'high) and (str(pos to pos + 1) = "Bd")) then
																		return int * 1 Bd;
		elsif (pos + 2 = str'high) then
			if (str(pos to pos + 2) = "kBd") then
				if (frac = 0) then					return (int * 1 kBd);
				elsif (digits <= 3) then		return (int * 1 kBd) + (frac * 10**(3 - digits) * 1 Bd);
				else												return (int * 1 kBd) + (frac / 10**(digits - 3) * 100 Bd);
				end if;
			elsif (str(pos to pos + 2) = "MBd") then
				if (frac = 0) then					return (int * 1 kBd);
				elsif (digits <= 3) then		return (int * 1 MBd) + (frac * 10**(3 - digits) * 1 kBd);
				elsif (digits <= 6) then		return (int * 1 MBd) + (frac * 10**(6 - digits) * 1 Bd);
				else												return (int * 1 MBd) + (frac / 10**(digits - 6) * 100000 Bd);
				end if;
			elsif (str(pos to pos + 2) = "GBd") then
				if (frac = 0) then					return (int * 1 kBd);
				elsif (digits <= 3) then		return (int * 1 GBd) + (frac * 10**(3 - digits) * 1 MBd);
				elsif (digits <= 6) then		return (int * 1 GBd) + (frac * 10**(6 - digits) * 1 kBd);
				elsif (digits <= 9) then		return (int * 1 GBd) + (frac * 10**(9 - digits) * 1 Bd);
				else												return (int * 1 GBd) + (frac / 10**(digits - 9) * 100000000 Bd);
				end if;
			else
				report "to_baud: Unknown unit." severity FAILURE;
			end if;
		else
			report "to_baud: Unknown format" severity FAILURE;
		end if;
	end function;
	
	-- if-then-else
	-- ===========================================================================
	function ite(cond : BOOLEAN; value1 : TIME;	value2 : TIME) return TIME is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;
	
	function ite(cond : BOOLEAN; value1 : FREQ;	value2 : FREQ) return FREQ is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;
	
	function ite(cond : BOOLEAN; value1 : BAUD;	value2 : BAUD) return BAUD is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;
	
	function ite(cond : BOOLEAN; value1 : MEMORY;	value2 : MEMORY) return MEMORY is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;
	
	-- min/ max for 2 arguments
	-- ===========================================================================
	-- Calculates: min(arg1, arg2) for times
	function min(arg1 : TIME; arg2 : TIME) return TIME is
	begin
		if (arg1 < arg2) then return arg1; end if;
		return arg2;
	end function;
	
	-- Calculates: min(arg1, arg2) for frequencies
	function min(arg1 : FREQ; arg2 : FREQ) return FREQ is
	begin
		if (arg1 < arg2) then return arg1; end if;
		return arg2;
	end function;
	
	-- Calculates: min(arg1, arg2) for symbols per second
	function min(arg1 : BAUD; arg2 : BAUD) return BAUD is
	begin
		if (arg1 < arg2) then return arg1; end if;
		return arg2;
	end function;
	
	-- Calculates: min(arg1, arg2) for memory
	function min(arg1 : MEMORY; arg2 : MEMORY) return MEMORY is
	begin
		if (arg1 < arg2) then return arg1; end if;
		return arg2;
	end function;
	
	-- Calculates: max(arg1, arg2) for times
	function max(arg1 : TIME; arg2 : TIME) return TIME is
	begin
		if (arg1 > arg2) then return arg1; end if;
		return arg2;
	end function;

	-- Calculates: max(arg1, arg2) for frequencies
	function max(arg1 : FREQ; arg2 : FREQ) return FREQ is
	begin
		if (arg1 > arg2) then return arg1; end if;
		return arg2;
	end function;

	-- Calculates: max(arg1, arg2) for symbols per second
	function max(arg1 : BAUD; arg2 : BAUD) return BAUD is
	begin
		if (arg1 > arg2) then return arg1; end if;
		return arg2;
	end function;

	-- Calculates: max(arg1, arg2) for memory
	function max(arg1 : MEMORY; arg2 : MEMORY) return MEMORY is
	begin
		if (arg1 > arg2) then return arg1; end if;
		return arg2;
	end function;
	
	-- min/max/sum as vector aggregation
	-- ===========================================================================
	-- Calculates: min(vec) for a time vector
	function min(vec : T_TIMEVEC)	return TIME is
		variable  res : TIME := TIME'high;
	begin
		for i in vec'range loop
			if (vec(i) < res) then
				res := vec(i);
			end if;
		end loop;
		return  res;
	end;
	
	-- Calculates: min(vec) for a frequency vector
	function min(vec : T_FREQVEC)	return FREQ is
		variable  res : FREQ := FREQ'high;
	begin
		for i in vec'range loop
			if (vec(i) < res) then
				res := vec(i);
			end if;
		end loop;
		return  res;
	end;
	
	-- Calculates: min(vec) for a baud vector
	function min(vec : T_BAUDVEC)	return BAUD is
		variable  res : BAUD := BAUD'high;
	begin
		for i in vec'range loop
			if (vec(i) < res) then
				res := vec(i);
			end if;
		end loop;
		return  res;
	end;
	
	-- Calculates: min(vec) for a memory vector
	function min(vec : T_MEMVEC)	return MEMORY is
		variable  res : MEMORY := MEMORY'high;
	begin
		for i in vec'range loop
			if (vec(i) < res) then
				res := vec(i);
			end if;
		end loop;
		return  res;
	end;
	
	-- Calculates: max(vec) for a time vector
	function max(vec : T_TIMEVEC)	return TIME is
		variable  res : TIME := TIME'low;
	begin
		for i in vec'range loop
			if (vec(i) > res) then
				res := vec(i);
			end if;
		end loop;
		return  res;
	end;
	
	-- Calculates: max(vec) for a frequency vector
	function max(vec : T_FREQVEC)	return FREQ is
		variable  res : FREQ := FREQ'low;
	begin
		for i in vec'range loop
			if (vec(i) > res) then
				res := vec(i);
			end if;
		end loop;
		return  res;
	end;
	
	-- Calculates: max(vec) for a baud vector
	function max(vec : T_BAUDVEC)	return BAUD is
		variable  res : BAUD := BAUD'low;
	begin
		for i in vec'range loop
			if (vec(i) > res) then
				res := vec(i);
			end if;
		end loop;
		return  res;
	end;
	
	-- Calculates: max(vec) for a memory vector
	function max(vec : T_MEMVEC)	return MEMORY is
		variable  res : MEMORY := MEMORY'low;
	begin
		for i in vec'range loop
			if (vec(i) > res) then
				res := vec(i);
			end if;
		end loop;
		return  res;
	end;
	
	-- Calculates: sum(vec) for a time vector
	function sum(vec : T_TIMEVEC)	return TIME is
		variable  res : TIME := 0 fs;
	begin
		for i in vec'range loop
			res	:= res + vec(i);
		end loop;
		return  res;
	end;
	
	-- Calculates: sum(vec) for a frequency vector
	function sum(vec : T_FREQVEC)	return FREQ is
		variable  res : FREQ := 0 Hz;
	begin
		for i in vec'range loop
			res	:= res + vec(i);
		end loop;
		return  res;
	end;
	
	-- Calculates: sum(vec) for a baud vector
	function sum(vec : T_BAUDVEC)	return BAUD is
		variable  res : BAUD := 0 Bd;
	begin
		for i in vec'range loop
			res	:= res + vec(i);
		end loop;
		return  res;
	end;
	
	-- Calculates: sum(vec) for a memory vector
	function sum(vec : T_MEMVEC)	return MEMORY is
		variable  res : MEMORY := 0 Byte;
	begin
		for i in vec'range loop
			res	:= res + vec(i);
		end loop;
		return  res;
	end;
	
	-- convert standard types (NATURAL, REAL) to time (TIME)
	-- ===========================================================================
	function fs2Time(t_fs : NATURAL) return TIME is
	begin
		return t_fs * 1 fs;
	end function;
	
	function ps2Time(t_ps : NATURAL) return TIME is
	begin
		return t_ps * 1 ps;
	end function;
	
	function ns2Time(t_ns : NATURAL) return TIME is
	begin
		return t_ns * 1 ns;
	end function;
	
	function us2Time(t_us : NATURAL) return TIME is
	begin
		return t_us * 1 us;
	end function;
	
	function ms2Time(t_ms : NATURAL) return TIME is
	begin
		return t_ms * 1 ms;
	end function;
	
	function sec2Time(t_sec : NATURAL) return TIME is
	begin
		return t_sec * 1 sec;
	end function;
	
	function fs2Time(t_fs : REAL) return TIME is
	begin
		return t_fs * 1 fs;
	end function;
	
	function ps2Time(t_ps : REAL) return TIME is
	begin
		return t_ps * 1 ps;
	end function;
	
	function ns2Time(t_ns : REAL) return TIME is
	begin
		return t_ns * 1 ns;
	end function;
	
	function us2Time(t_us : REAL) return TIME is
	begin
		return t_us * 1 us;
	end function;
	
	function ms2Time(t_ms : REAL) return TIME is
	begin
		return t_ms * 1 ms;
	end function;
	
	function sec2Time(t_sec : REAL) return TIME is
	begin
		return t_sec * 1 sec;
	end function;
	
	-- convert standard types (NATURAL, REAL) to period (TIME)
	-- ===========================================================================
	function Hz2Time(f_Hz : NATURAL) return TIME is
	begin
		return 1 sec / f_Hz;
	end function;
	
	function kHz2Time(f_kHz : NATURAL) return TIME is
	begin
		return 1 ms / f_kHz;
	end function;
	
	function MHz2Time(f_MHz : NATURAL) return TIME
	 is
	begin
		return 1 us / f_MHz;
	end function;
	
	function GHz2Time(f_GHz : NATURAL) return TIME is
	begin
		return 1 ns / f_GHz;
	end function;
	
--	function THz2Time(f_THz : NATURAL) return TIME is
--	begin
--		return 1 ps / f_THz;
--	end function;

	
	function Hz2Time(f_Hz : REAL) return TIME is
	begin
		return 1 sec / f_Hz;
	end function;
	
	function kHz2Time(f_kHz : REAL) return TIME is
	begin
		return 1 ms / f_kHz;
	end function;
	
	function MHz2Time(f_MHz : REAL) return TIME is
	begin
		return 1 us / f_MHz;
	end function;
	
	function GHz2Time(f_GHz : REAL) return TIME is
	begin
		return 1 ns / f_GHz;
	end function;
	
--	function THz2Time(f_THz : REAL) return TIME is
--	begin
--		return 1 ps / f_THz;
--	end function;
	
	-- convert standard types (NATURAL, REAL) to frequency (FREQ)
	-- ===========================================================================
	function Hz2Freq(f_Hz : NATURAL) return FREQ is
	begin
		return f_Hz * 1 Hz;
	end function;
	
	function kHz2Freq(f_kHz : NATURAL) return FREQ is
	begin
		return f_kHz * 1 kHz;
	end function;
	
	function MHz2Freq(f_MHz : NATURAL) return FREQ is
	begin
		return f_MHz * 1 MHz;
	end function;
	
	function GHz2Freq(f_GHz : NATURAL) return FREQ is
	begin
		return f_GHz * 1 GHz;
	end function;
	
--	function THz2Freq(f_THz : NATURAL) return FREQ is
--	begin
--		return f_THz * 1 THz;
--	end function;
	
	function Hz2Freq(f_Hz : REAL) return FREQ is
	begin
		return f_Hz * 1 Hz;
	end function;
	
	function kHz2Freq(f_kHz : REAL )return FREQ is
	begin
		return f_kHz * 1 kHz;
	end function;
	
	function MHz2Freq(f_MHz : REAL )return FREQ is
	begin
		return f_MHz * 1 MHz;
	end function;
	
	function GHz2Freq(f_GHz : REAL )return FREQ is
	begin
		return f_GHz * 1 GHz;
	end function;
	
--	function THz2Freq(f_THz : REAL )return FREQ is
--	begin
--		return f_THz * 1 THz;
--	end function;
	
	-- convert physical types to standard type (REAL)
	-- ===========================================================================
	function to_real(t : TIME; scale : TIME) return REAL is
	begin
		if		(scale = 1	fs) then	return div(t, 1	 fs);
		elsif	(scale = 1	ps) then	return div(t, 1	 ps);
		elsif	(scale = 1	ns) then	return div(t, 1	 ns);
		elsif	(scale = 1	us) then	return div(t, 1	 us);
		elsif	(scale = 1	ms) then	return div(t, 1	 ms);
		elsif	(scale = 1 sec) then	return div(t, 1 sec);
		else	report "to_real: scale must have a value of '1 <unit>'" severity failure;
		end if;
	end;

	function to_real(f : FREQ; scale : FREQ) return REAL is
	begin
		if		(scale = 1	Hz) then	return div(f, 1	 Hz);
		elsif	(scale = 1 kHz) then	return div(f, 1 kHz);
		elsif	(scale = 1 MHz) then	return div(f, 1 MHz);
		elsif	(scale = 1 GHz) then	return div(f, 1 GHz);
--	elsif	(scale = 1 THz) then	return div(f, 1 THz);
		else	report "to_real: scale must have a value of '1 <unit>'" severity failure;
		end if;
	end;

	function to_real(br : BAUD; scale : BAUD) return REAL is
	begin
		if		(scale = 1	Bd) then	return div(br, 1	Bd);
		elsif	(scale = 1 kBd) then	return div(br, 1 kBd);
		elsif	(scale = 1 MBd) then	return div(br, 1 MBd);
		elsif	(scale = 1 GBd) then	return div(br, 1 GBd);
		else	report "to_real: scale must have a value of '1 <unit>'" severity failure;
		end if;
	end;
	
	function to_real(mem : MEMORY; scale : MEMORY) return REAL is
	begin
		if		(scale = 1 Byte)	then	return div(mem, 1	Byte);
		elsif	(scale = 1 KiB)		then	return div(mem, 1 KiB);
		elsif	(scale = 1 MiB)		then	return div(mem, 1 MiB);
		elsif	(scale = 1 GiB)		then	return div(mem, 1 GiB);
--	elsif	(scale = 1 TiB)		then	return div(mem, 1 TiB);
		else	report "to_real: scale must have a value of '1 <unit>'" severity failure;
		end if;
	end;
	
	-- convert physical types to standard type (INTEGER)
	-- ===========================================================================
	function to_int(t : TIME; scale : TIME; RoundingStyle : T_ROUNDING_STYLE := ROUND_TO_NEAREST) return INTEGER is
	begin
		case RoundingStyle is
			when ROUND_UP =>					return integer(ceil(to_real(t, scale)));
			when ROUND_DOWN =>				return integer(floor(to_real(t, scale)));
			when ROUND_TO_NEAREST =>	return integer(round(to_real(t, scale)));
			when others =>						null;
		end case;
		report "to_int: unsupported RoundingStyle: " & T_ROUNDING_STYLE'image(RoundingStyle) severity failure;
	end;

	function to_int(f : FREQ; scale : FREQ; RoundingStyle : T_ROUNDING_STYLE := ROUND_TO_NEAREST) return INTEGER is
	begin
		case RoundingStyle is
			when ROUND_UP =>					return integer(ceil(to_real(f, scale)));
			when ROUND_DOWN =>				return integer(floor(to_real(f, scale)));
			when ROUND_TO_NEAREST =>	return integer(round(to_real(f, scale)));
			when others =>						null;
		end case;
		report "to_int: unsupported RoundingStyle: " & T_ROUNDING_STYLE'image(RoundingStyle) severity failure;
	end;

	function to_int(br : BAUD; scale : BAUD; RoundingStyle : T_ROUNDING_STYLE := ROUND_TO_NEAREST) return INTEGER is
	begin
		case RoundingStyle is
			when ROUND_UP =>					return integer(ceil(to_real(br, scale)));
			when ROUND_DOWN =>				return integer(floor(to_real(br, scale)));
			when ROUND_TO_NEAREST =>	return integer(round(to_real(br, scale)));
			when others =>						null;
		end case;
		report "to_int: unsupported RoundingStyle: " & T_ROUNDING_STYLE'image(RoundingStyle) severity failure;
	end;
	
	function to_int(mem : MEMORY; scale : MEMORY; RoundingStyle : T_ROUNDING_STYLE := ROUND_UP) return INTEGER is
	begin
		case RoundingStyle is
			when ROUND_UP =>					return integer(ceil(to_real(mem, scale)));
			when ROUND_DOWN =>				return integer(floor(to_real(mem, scale)));
			when ROUND_TO_NEAREST =>	return integer(round(to_real(mem, scale)));
			when others =>						null;
		end case;
		report "to_int: unsupported RoundingStyle: " & T_ROUNDING_STYLE'image(RoundingStyle) severity failure;
	end;
	
	-- calculate needed counter cycles to achieve a given 1. timing/delay and 2. frequency/period
	-- ===========================================================================
	--	@param Timing					A given timing or delay, which should be achived
	--	@param Clock_Period		The period of the circuits clock
	--	@RoundingStyle				Default = round to nearest; other choises: ROUND_UP, ROUND_DOWN
	function TimingToCycles(Timing : TIME; Clock_Period : TIME; RoundingStyle : T_ROUNDING_STYLE := ROUND_UP) return NATURAL is
		variable res_real	: REAL;
		variable res_nat	: NATURAL;
		variable res_time	: TIME;
		variable res_dev	: REAL;
	begin
		res_real := div(Timing, Clock_Period);	
		case RoundingStyle is
			when ROUND_TO_NEAREST =>	res_nat := natural(round(res_real));
			when ROUND_UP =>					res_nat := natural(ceil(res_real));
			when ROUND_DOWN =>				res_nat := natural(floor(res_real));
			when others =>	report "RoundingStyle '" & T_ROUNDING_STYLE'image(RoundingStyle) & "' not supported." severity failure;
		end case;
		res_time	:= CyclesToDelay(res_nat, Clock_Period);
		res_dev		:= (1.0 - div(res_time, Timing)) * 100.0;
		
		if (POC_VERBOSE = TRUE) then
			report "TimingToCycles: " & 	CR &
						 "  Timing: " &					to_string(Timing, 3) & CR &
						 "  Clock_Period: " &		to_string(Clock_Period, 3) & CR &
						 "  RoundingStyle: " &	str_substr(T_ROUNDING_STYLE'image(RoundingStyle), 7) & CR &
						 "  res_real = " &			str_format(res_real, 3) & CR &
						 "  => " &							INTEGER'image(res_nat)
			severity note;
		end if;
			
--		if (C_PHYSICAL_REPORT_TIMING_DEVIATION = TRUE) then
--			report "TimingToCycles (timing deviation report): " & CR &
--						 "  timing to achieve: " & to_string(Timing) & CR &
--						 "  calculated cycles: " & INTEGER'image(res_nat) & " cy" & CR &
--						 "  resulting timing:  " & to_string(res_time) & CR &
--						 "  deviation:         " & to_string(Timing - res_time) & " (" & str_format(res_dev, 2) & "%)"
--			severity note;
--		end if;
		
		return res_nat;
	end;
	
	function TimingToCycles(Timing : TIME; Clock_Frequency	: FREQ; RoundingStyle : T_ROUNDING_STYLE := ROUND_UP) return NATURAL is
	begin
		return TimingToCycles(Timing, to_time(Clock_Frequency), RoundingStyle);
	end function;

	function CyclesToDelay(Cycles : NATURAL; Clock_Period : TIME) return TIME is
	begin
		return Clock_Period * Cycles;
	end function;

	function CyclesToDelay(Cycles : NATURAL; Clock_Frequency : FREQ) return TIME is
	begin
		return CyclesToDelay(Cycles, to_time(Clock_Frequency));
	end function;
	
	-- convert and format physical types to STRING
	function to_string(t : TIME; precision : NATURAL) return STRING is
		variable unit		: STRING(1 to 3)	:= (others => C_POC_NUL);
		variable value	: REAL;
	begin
		if (t < 1 ps) then
			unit(1 to 2)	:= "fs";
			value					:= to_real(t, 1 fs);
		elsif (t < 1 ns) then
			unit(1 to 2)	:= "ps";
			value					:= to_real(t, 1 ps);
		elsif (t < 1 us) then
			unit(1 to 2)	:= "ns";
			value					:= to_real(t, 1 ns);
		elsif (t < 1 ms) then
			unit(1 to 2)	:= "us";
			value					:= to_real(t, 1 us);
		elsif (t < 1 sec) then
			unit(1 to 2)	:= "ms";
			value					:= to_real(t, 1 ms);
		else
			unit					:= "sec";
			value					:= to_real(t, 1 sec);
		end if;

		return str_format(value, precision) & " " & str_trim(unit);
	end function;
		
	function to_string(f : FREQ; precision : NATURAL) return STRING is
		variable unit		: STRING(1 to 3)	:= (others => C_POC_NUL);
		variable value	: REAL;
	begin
		if (f < 1 kHz) then
			unit(1 to 2)	:= "Hz";
			value					:= to_real(f, 1 Hz);
		elsif (f < 1 MHz) then
			unit					:= "kHz";
			value					:= to_real(f, 1 kHz);
		elsif (f < 1 GHz) then
			unit					:= "MHz";
			value					:= to_real(f, 1 MHz);
		else	--if (f < 1 THz) then
			unit					:= "GHz";
			value					:= to_real(f, 1 GHz);
--	else
--		unit					:= "THz";
--		value					:= to_real(f, 1 THz);
		end if;

		return str_format(value, precision) & " " & str_trim(unit);
	end function;
		
	function to_string(br : BAUD; precision : NATURAL) return STRING is
		variable unit		: STRING(1 to 3)	:= (others => C_POC_NUL);
		variable value	: REAL;
	begin
		if (br < 1 kBd) then
			unit(1 to 2)	:= "Bd";
			value					:= to_real(br, 1 Bd);
		elsif (br < 1 MBd) then
			unit					:= "kBd";
			value					:= to_real(br, 1 kBd);
		elsif (br < 1 GBd) then
			unit					:= "MBd";
			value					:= to_real(br, 1 MBd);
		else
			unit					:= "GBd";
			value					:= to_real(br, 1 GBd);
		end if;

		return str_format(value, precision) & " " & str_trim(unit);
	end function;
		
	function to_string(mem : MEMORY; precision : NATURAL) return STRING is
		variable unit		: STRING(1 to 3)	:= (others => C_POC_NUL);
		variable value	: REAL;
	begin
		if (mem < 1 KiB) then
			unit(1)				:= 'B';
			value					:= to_real(mem, 1 Byte);
		elsif (mem < 1 MiB) then
			unit					:= "KiB";
			value					:= to_real(mem, 1 KiB);
		elsif (mem < 1 GiB) then
			unit					:= "MiB";
			value					:= to_real(mem, 1 MiB);
		else	--if (mem < 1 TiB) then
			unit					:= "GiB";
			value					:= to_real(mem, 1 GiB);
--	else
--		unit					:= "TiB";
--		value					:= to_real(mem, 1 TiB);
		end if;

		return str_format(value, precision) & " " & str_trim(unit);
	end function;
	
end package body;
