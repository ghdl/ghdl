-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- =============================================================================
-- Authors:					Patrick Lehmann
-- 									Martin Zabel
--									Thomas B. Preusser
--
-- Package:					This VHDL package declares new physical types and their
--									conversion functions.
--
-- Description:
-- -------------------------------------
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
-- =============================================================================
-- Copyright 2007-2015 Technische Universitaet Dresden - Germany,
--										 Chair of VLSI-Design, Diagnostics and Architecture
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
-- =============================================================================

library IEEE;
use			IEEE.math_real.all;

library PoC;
use			PoC.config.all;
use			PoC.utils.all;
use			PoC.strings.all;


package physical is

	type FREQ is range 0 to integer'high units
		Hz;
		kHz = 1000 Hz;
		MHz = 1000 kHz;
		GHz = 1000 MHz;
	end units;

	type BAUD is range 0 to integer'high units
		Bd;
		kBd = 1000 Bd;
		MBd = 1000 kBd;
		GBd = 1000 MBd;
	end units;

	type MEMORY is range 0 to integer'high units
		Byte;
		KiB = 1024 Byte;
		MiB = 1024 KiB;
		GiB = 1024 MiB;
	end units;

	-- vector data types
	type		T_TIMEVEC						is array(natural range <>) of time;
	type		T_FREQVEC						is array(natural range <>) of FREQ;
	type		T_BAUDVEC						is array(natural range <>) of BAUD;
	type		T_MEMVEC						is array(natural range <>) of MEMORY;

	-- if true: TimingToCycles reports difference between expected and actual result
	constant C_PHYSICAL_REPORT_TIMING_DEVIATION		: boolean		:= TRUE;

	-- conversion functions
	function to_time(f : FREQ)	return time;
	function to_freq(p : time)	return FREQ;
	function to_freq(br : BAUD)	return FREQ;
	function to_baud(str : string)	return BAUD;

	-- inter-type arithmetic
	function div(a : time; b : time) return real;
	function div(a : FREQ; b : FREQ) return real;

	function "/"(x : real; t : time) return FREQ;
	function "/"(x : real; f : FREQ) return time;
	function "*"(t : time; f : FREQ) return real;
	function "*"(f : FREQ; t : time) return real;

	-- if-then-else
	function ite(cond : boolean; value1 : time;	value2 : time)			return time;
	function ite(cond : boolean; value1 : FREQ;	value2 : FREQ)			return FREQ;
	function ite(cond : boolean; value1 : BAUD;	value2 : BAUD)			return BAUD;
	function ite(cond : boolean; value1 : MEMORY;	value2 : MEMORY)	return MEMORY;

	-- min/ max for 2 arguments
	function tmin(arg1 : time; arg2 : time) return time;						-- Calculates: min(arg1, arg2) for times
	function fmin(arg1 : FREQ; arg2 : FREQ) return FREQ;						-- Calculates: min(arg1, arg2) for frequencies
	function bmin(arg1 : BAUD; arg2 : BAUD) return BAUD;						-- Calculates: min(arg1, arg2) for symbols per second
	function mmin(arg1 : MEMORY; arg2 : MEMORY) return MEMORY;			-- Calculates: min(arg1, arg2) for memory

	function tmax(arg1 : time; arg2 : time) return time;						-- Calculates: max(arg1, arg2) for times
	function fmax(arg1 : FREQ; arg2 : FREQ) return FREQ;						-- Calculates: max(arg1, arg2) for frequencies
	function bmax(arg1 : BAUD; arg2 : BAUD) return BAUD;						-- Calculates: max(arg1, arg2) for symbols per second
	function mmax(arg1 : MEMORY; arg2 : MEMORY) return MEMORY;			-- Calculates: max(arg1, arg2) for memory

	-- min/max/sum as vector aggregation
	function tmin(vec : T_TIMEVEC)	return time;										-- Calculates: min(vec) for a time vector
	function fmin(vec : T_FREQVEC)	return FREQ;										-- Calculates: min(vec) for a frequency vector
	function bmin(vec : T_BAUDVEC)	return BAUD;										-- Calculates: min(vec) for a baud vector
	function mmin(vec : T_MEMVEC)	return MEMORY;									-- Calculates: min(vec) for a memory vector

	function tmax(vec : T_TIMEVEC)	return time;										-- Calculates: max(vec) for a time vector
	function fmax(vec : T_FREQVEC)	return FREQ;										-- Calculates: max(vec) for a frequency vector
	function bmax(vec : T_BAUDVEC)	return BAUD;										-- Calculates: max(vec) for a baud vector
	function mmax(vec : T_MEMVEC)	return MEMORY;									-- Calculates: max(vec) for a memory vector

	function tsum(vec : T_TIMEVEC)	return time;										-- Calculates: sum(vec) for a time vector
	function fsum(vec : T_FREQVEC)	return FREQ;										-- Calculates: sum(vec) for a frequency vector
	function bsum(vec : T_BAUDVEC)	return BAUD;										-- Calculates: sum(vec) for a baud vector
	function msum(vec : T_MEMVEC)	return MEMORY;									-- Calculates: sum(vec) for a memory vector

	-- convert standard types (NATURAL, REAL) to time (TIME)
	function fs2Time(t_fs : integer)		return time;
	function ps2Time(t_ps : integer)		return time;
	function ns2Time(t_ns : integer)		return time;
	function us2Time(t_us : integer)		return time;
	function ms2Time(t_ms : integer)		return time;
	function sec2Time(t_sec : integer)	return time;

	function fs2Time(t_fs : REAL)				return time;
	function ps2Time(t_ps : REAL)				return time;
	function ns2Time(t_ns : REAL)				return time;
	function us2Time(t_us : REAL)				return time;
	function ms2Time(t_ms : REAL)				return time;
	function sec2Time(t_sec : REAL)			return time;

	-- convert standard types (NATURAL, REAL) to period (TIME)
	function Hz2Time(f_Hz : natural)		return time;
	function kHz2Time(f_kHz : natural)	return time;
	function MHz2Time(f_MHz : natural)	return time;
	function GHz2Time(f_GHz : natural)	return time;

	function Hz2Time(f_Hz : REAL)				return time;
	function kHz2Time(f_kHz : REAL) 		return time;
	function MHz2Time(f_MHz : REAL) 		return time;
	function GHz2Time(f_GHz : REAL) 		return time;

	-- convert standard types (NATURAL, REAL) to frequency (FREQ)
	function Hz2Freq(f_Hz : natural)		return FREQ;
	function kHz2Freq(f_kHz : natural)	return FREQ;
	function MHz2Freq(f_MHz : natural)	return FREQ;
	function GHz2Freq(f_GHz : natural)	return FREQ;

	function Hz2Freq(f_Hz : REAL)				return FREQ;
	function kHz2Freq(f_kHz : REAL)			return FREQ;
	function MHz2Freq(f_MHz : REAL)			return FREQ;
	function GHz2Freq(f_GHz : REAL)			return FREQ;

	-- convert physical types to standard type (REAL)
	function to_real(t : time;			scale : time)		return REAL;
	function to_real(f : FREQ;			scale : FREQ)		return REAL;
	function to_real(br : BAUD;			scale : BAUD)		return REAL;
	function to_real(mem : MEMORY;	scale : MEMORY)	return REAL;

	-- convert physical types to standard type (INTEGER)
	function to_int(t : time;			scale : time;		RoundingStyle : T_ROUNDING_STYLE := ROUND_TO_NEAREST)	return integer;
	function to_int(f : FREQ;			scale : FREQ;		RoundingStyle : T_ROUNDING_STYLE := ROUND_TO_NEAREST)	return integer;
	function to_int(br : BAUD;		scale : BAUD;		RoundingStyle : T_ROUNDING_STYLE := ROUND_TO_NEAREST)	return integer;
	function to_int(mem : MEMORY;	scale : MEMORY;	RoundingStyle : T_ROUNDING_STYLE := ROUND_UP)					return integer;

	-- calculate needed counter cycles to achieve a given 1. timing/delay and 2. frequency/period
	function TimingToCycles(Timing : time; Clock_Period			: time; RoundingStyle : T_ROUNDING_STYLE := ROUND_UP) return natural;
	function TimingToCycles(Timing : time; Clock_Frequency	: FREQ; RoundingStyle : T_ROUNDING_STYLE := ROUND_UP) return natural;

	function CyclesToDelay(Cycles : natural; Clock_Period			: time) return time;
	function CyclesToDelay(Cycles : natural; Clock_Frequency	: FREQ) return time;

	-- convert and format physical types to STRING
	function to_string(t : time; precision : natural)			return string;
	function to_string(f : FREQ; precision : natural)			return string;
	function to_string(br : BAUD; precision : natural)		return string;
	function to_string(mem : MEMORY; precision : natural)	return string;
end package;


package body physical is

	-- WORKAROUND: for simulators with a "Minimal Time Resolution" > 1 fs
	--	Version:	all
	--	Vendors:	all
	--	Issue:
	--		Some simulators use a lower minimal time resolution (MTR) than the VHDL
	--		standard (LRM) defines (1 fs). Usually, the MTR is set to 1 ps or 1 ns.
	--		Most simulators allow the user to specify a higher MTR -> check the
	--		simulator documentation.
	--	Solution:
	--		The currently set MTR can be calculated in VHDL. Using the correct MTR
	--		can prevent cleared intermediate values and division by zero errors.
	--	Examples:
	--		Mentor Graphics QuestaSim/ModelSim (vSim): default MTR = ? ??
	--		Xilinx ISE Simulator (iSim):               default MTR = 1 ps
	--		Xilinx Vivado Simulator (xSim):            default MTR = 1 ps
	function MinimalTimeResolutionInSimulation return time is
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
	function div(a : time; b : time) return REAL is
		constant MTRIS	: time		:= MinimalTimeResolutionInSimulation;
		variable a_real : real;
		variable b_real : real;
	begin
		-- WORKAROUND: for Altera Quartus
		--	Version:	all
		--	Issue:
		--		Results of TIME arithmetic must be in 32-bit integer range, because
		--		the internally used 64-bit integer for type TIME can not be
		--		represented in VHDL.
		--	Solution:
		--		Pre- and post-scale all values to stay in the integer range.
	  if    a < 1 us  then
			a_real  := real(a / MTRIS);
		elsif a < 1 ms  then
			a_real  := real(a / (1000 * MTRIS)) * 1000.0;
		elsif a < 1 sec then
			a_real  := real(a / (1000000 * MTRIS)) * 1000000.0;
		else
			a_real  := real(a / (1000000000 * MTRIS)) * 1000000000.0;
		end if;

	  if    b < 1 us  then
			b_real  := real(b / MTRIS);
		elsif b < 1 ms  then
			b_real  := real(b / (1000 * MTRIS)) * 1000.0;
		elsif b < 1 sec then
			b_real  := real(b / (1000000 * MTRIS)) * 1000000.0;
		else
			b_real  := real(b / (1000000000 * MTRIS)) * 1000000000.0;
		end if;

		return a_real / b_real;
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
	function to_time(f : FREQ) return time is
		variable res : time;
	begin
		res := div(1000 MHz, f) * 1 ns;
		if POC_VERBOSE then
			report "to_time: f= " & to_string(f, 3) & "  return " & to_string(res, 3) severity note;
		end if;
		return res;
	end function;

	function to_freq(p : time) return FREQ is
		variable res : FREQ;
	begin
		if (p <= 1 sec) then res := div(1 sec, p) * 1  Hz;
		else report "to_freq: input period exceeds output frequency scale." severity failure;
		end if;
		if POC_VERBOSE then
			report "to_freq: p= " & to_string(p, 3) & "  return " & to_string(res, 3) severity note;
		end if;
		return res;
	end function;

	function to_freq(br : BAUD) return FREQ is
		variable res : FREQ;
	begin
		res := (br / 1 Bd)	* 1  Hz;
		if POC_VERBOSE then
			report "to_freq: br= " & to_string(br, 3) & "  return " & to_string(res, 3) severity note;
		end if;
		return res;
	end function;

	function to_baud(str : string) return BAUD is
		variable pos		: integer;
		variable int		: natural;
		variable base		: positive;
		variable frac		: natural;
		variable digits	: natural;
	begin
		pos			:= str'low;
		int			:= 0;
		frac		:= 0;
		digits	:= 0;
		-- read integer part
		for i in pos to str'high loop
			if chr_isDigit(str(i)) then		int := int * 10 + to_digit_dec(str(i));
			elsif (str(i) = '.') then								pos	:= -i;	exit;
			elsif (str(i) = ' ') then								pos	:= i;		exit;
			else																		pos := 0;		exit;
			end if;
		end loop;
		-- read fractional part
		if ((pos < 0) and (-pos < str'high)) then
			for i in -pos+1 to str'high loop
				if ((frac = 0) and (str(i) = '0')) then	next;
				elsif chr_isDigit(str(i)) then	frac	:= frac * 10 + to_digit_dec(str(i));
				elsif (str(i) = ' ') then								digits	:= i + pos - 1;	pos	:= i;	exit;
				else																														pos	:= 0;	exit;
				end if;
			end loop;
		end if;
		-- abort if format is unknown
		if pos = 0 then report "to_baud: Unknown format" severity FAILURE;	end if;
		-- parse unit
		pos := pos + 1;
		if ((pos + 1 = str'high) and (str(pos to pos + 1) = "Bd")) then
																		return int * 1 Bd;
		elsif (pos + 2 = str'high) then
			if (str(pos to pos + 2) = "kBd") then
				if frac = 0 then					return (int * 1 kBd);
				elsif (digits <= 3) then		return (int * 1 kBd) + (frac * 10**(3 - digits) * 1 Bd);
				else												return (int * 1 kBd) + (frac / 10**(digits - 3) * 100 Bd);
				end if;
			elsif (str(pos to pos + 2) = "MBd") then
				if frac = 0 then					return (int * 1 kBd);
				elsif (digits <= 3) then		return (int * 1 MBd) + (frac * 10**(3 - digits) * 1 kBd);
				elsif (digits <= 6) then		return (int * 1 MBd) + (frac * 10**(6 - digits) * 1 Bd);
				else												return (int * 1 MBd) + (frac / 10**(digits - 6) * 100000 Bd);
				end if;
			elsif (str(pos to pos + 2) = "GBd") then
				if frac = 0 then					return (int * 1 kBd);
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
		return 0 Bd;
	end function;

	-- inter-type arithmetic
	-- ===========================================================================
	function "/"(x : real; t : time) return FREQ is
	begin
		return  x*div(1 ms, t) * 1 kHz;
	end function;
	function "/"(x : real; f : FREQ) return time is
	begin
		return  x*div(1 kHz, f) * 1 ms;
	end function;
	function "*"(t : time; f : FREQ) return real is
	begin
		return  div(t, 1.0/f);
	end function;
	function "*"(f : FREQ; t : time) return real is
	begin
		return  div(f, 1.0/t);
	end function;

	-- if-then-else
	-- ===========================================================================
	function ite(cond : boolean; value1 : time;	value2 : time) return time is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;

	function ite(cond : boolean; value1 : FREQ;	value2 : FREQ) return FREQ is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;

	function ite(cond : boolean; value1 : BAUD;	value2 : BAUD) return BAUD is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;

	function ite(cond : boolean; value1 : MEMORY;	value2 : MEMORY) return MEMORY is
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
	function tmin(arg1 : time; arg2 : time) return time is
	begin
		if arg1 < arg2 then return arg1; end if;
		return arg2;
	end function;

	-- Calculates: min(arg1, arg2) for frequencies
	function fmin(arg1 : FREQ; arg2 : FREQ) return FREQ is
	begin
		if arg1 < arg2 then return arg1; end if;
		return arg2;
	end function;

	-- Calculates: min(arg1, arg2) for symbols per second
	function bmin(arg1 : BAUD; arg2 : BAUD) return BAUD is
	begin
		if arg1 < arg2 then return arg1; end if;
		return arg2;
	end function;

	-- Calculates: min(arg1, arg2) for memory
	function mmin(arg1 : MEMORY; arg2 : MEMORY) return MEMORY is
	begin
		if arg1 < arg2 then return arg1; end if;
		return arg2;
	end function;

	-- Calculates: max(arg1, arg2) for times
	function tmax(arg1 : time; arg2 : time) return time is
	begin
		if arg1 > arg2 then return arg1; end if;
		return arg2;
	end function;

	-- Calculates: max(arg1, arg2) for frequencies
	function fmax(arg1 : FREQ; arg2 : FREQ) return FREQ is
	begin
		if arg1 > arg2 then return arg1; end if;
		return arg2;
	end function;

	-- Calculates: max(arg1, arg2) for symbols per second
	function bmax(arg1 : BAUD; arg2 : BAUD) return BAUD is
	begin
		if arg1 > arg2 then return arg1; end if;
		return arg2;
	end function;

	-- Calculates: max(arg1, arg2) for memory
	function mmax(arg1 : MEMORY; arg2 : MEMORY) return MEMORY is
	begin
		if arg1 > arg2 then return arg1; end if;
		return arg2;
	end function;

	-- min/max/sum as vector aggregation
	-- ===========================================================================
	-- Calculates: min(vec) for a time vector
	function tmin(vec : T_TIMEVEC)	return time is
		variable  res : time := time'high;
	begin
		for i in vec'range loop
			if vec(i) < res then
				res := vec(i);
			end if;
		end loop;
		return  res;
	end;

	-- Calculates: min(vec) for a frequency vector
	function fmin(vec : T_FREQVEC)	return FREQ is
		variable  res : FREQ := FREQ'high;
	begin
		for i in vec'range loop
			if (integer(FREQ'pos(vec(i))) < integer(FREQ'pos(res))) then -- Quartus workaround
				res := vec(i);
			end if;
		end loop;
		return  res;
	end;

	-- Calculates: min(vec) for a baud vector
	function bmin(vec : T_BAUDVEC)	return BAUD is
		variable  res : BAUD := BAUD'high;
	begin
		for i in vec'range loop
			if (integer(BAUD'pos(vec(i))) < integer(BAUD'pos(res))) then -- Quartus workaround
				res := vec(i);
			end if;
		end loop;
		return  res;
	end;

	-- Calculates: min(vec) for a memory vector
	function mmin(vec : T_MEMVEC)	return MEMORY is
		variable  res : MEMORY := MEMORY'high;
	begin
		for i in vec'range loop
			if (integer(MEMORY'pos(vec(i))) < integer(MEMORY'pos(res))) then -- Quartus workaround
				res := vec(i);
			end if;
		end loop;
		return  res;
	end;

	-- Calculates: max(vec) for a time vector
	function tmax(vec : T_TIMEVEC)	return time is
		variable  res : time := time'low;
	begin
		for i in vec'range loop
			if vec(i) > res then
				res := vec(i);
			end if;
		end loop;
		return  res;
	end;

	-- Calculates: max(vec) for a frequency vector
	function fmax(vec : T_FREQVEC)	return FREQ is
		variable  res : FREQ := FREQ'low;
	begin
		for i in vec'range loop
			if (integer(FREQ'pos(vec(i))) > integer(FREQ'pos(res))) then -- Quartus workaround
				res := vec(i);
			end if;
		end loop;
		return  res;
	end;

	-- Calculates: max(vec) for a baud vector
	function bmax(vec : T_BAUDVEC)	return BAUD is
		variable  res : BAUD := BAUD'low;
	begin
		for i in vec'range loop
			if (integer(BAUD'pos(vec(i))) > integer(BAUD'pos(res))) then -- Quartus workaround
				res := vec(i);
			end if;
		end loop;
		return  res;
	end;

	-- Calculates: max(vec) for a memory vector
	function mmax(vec : T_MEMVEC)	return MEMORY is
		variable  res : MEMORY := MEMORY'low;
	begin
		for i in vec'range loop
			if (integer(MEMORY'pos(vec(i))) > integer(MEMORY'pos(res))) then -- Quartus workaround
				res := vec(i);
			end if;
		end loop;
		return  res;
	end;

	-- Calculates: sum(vec) for a time vector
	function tsum(vec : T_TIMEVEC)	return time is
		variable  res : time := 0 fs;
	begin
		for i in vec'range loop
			res	:= res + vec(i);
		end loop;
		return  res;
	end;

	-- Calculates: sum(vec) for a frequency vector
	function fsum(vec : T_FREQVEC)	return FREQ is
		variable  res : FREQ := 0 Hz;
	begin
		for i in vec'range loop
			res	:= res + vec(i);
		end loop;
		return  res;
	end;

	-- Calculates: sum(vec) for a baud vector
	function bsum(vec : T_BAUDVEC)	return BAUD is
		variable  res : BAUD := 0 Bd;
	begin
		for i in vec'range loop
			res	:= res + vec(i);
		end loop;
		return  res;
	end;

	-- Calculates: sum(vec) for a memory vector
	function msum(vec : T_MEMVEC)	return MEMORY is
		variable  res : MEMORY := 0 Byte;
	begin
		for i in vec'range loop
			res	:= res + vec(i);
		end loop;
		return  res;
	end;

	-- convert standard types (NATURAL, REAL) to time (TIME)
	-- ===========================================================================
	function fs2Time(t_fs : integer) return time is
	begin
		return t_fs * 1 fs;
	end function;

	function ps2Time(t_ps : integer) return time is
	begin
		return t_ps * 1 ps;
	end function;

	function ns2Time(t_ns : integer) return time is
	begin
		return t_ns * 1 ns;
	end function;

	function us2Time(t_us : integer) return time is
	begin
		return t_us * 1 us;
	end function;

	function ms2Time(t_ms : integer) return time is
	begin
		return t_ms * 1 ms;
	end function;

	function sec2Time(t_sec : integer) return time is
	begin
		return t_sec * 1 sec;
	end function;

	function fs2Time(t_fs : REAL) return time is
	begin
		return t_fs * 1 fs;
	end function;

	function ps2Time(t_ps : REAL) return time is
	begin
		return t_ps * 1 ps;
	end function;

	function ns2Time(t_ns : REAL) return time is
	begin
		return t_ns * 1 ns;
	end function;

	function us2Time(t_us : REAL) return time is
	begin
		return t_us * 1 us;
	end function;

	function ms2Time(t_ms : REAL) return time is
	begin
		return t_ms * 1 ms;
	end function;

	function sec2Time(t_sec : REAL) return time is
	begin
		return t_sec * 1 sec;
	end function;

	-- convert standard types (NATURAL, REAL) to period (TIME)
	-- ===========================================================================
	function Hz2Time(f_Hz : natural) return time is
	begin
		return 1 sec / f_Hz;
	end function;

	function kHz2Time(f_kHz : natural) return time is
	begin
		return 1 ms / f_kHz;
	end function;

	function MHz2Time(f_MHz : natural) return time
	 is
	begin
		return 1 us / f_MHz;
	end function;

	function GHz2Time(f_GHz : natural) return time is
	begin
		return 1 ns / f_GHz;
	end function;

	function Hz2Time(f_Hz : REAL) return time is
	begin
		return 1 sec / f_Hz;
	end function;

	function kHz2Time(f_kHz : REAL) return time is
	begin
		return 1 ms / f_kHz;
	end function;

	function MHz2Time(f_MHz : REAL) return time is
	begin
		return 1 us / f_MHz;
	end function;

	function GHz2Time(f_GHz : REAL) return time is
	begin
		return 1 ns / f_GHz;
	end function;

	-- convert standard types (NATURAL, REAL) to frequency (FREQ)
	-- ===========================================================================
	function Hz2Freq(f_Hz : natural) return FREQ is
	begin
		return f_Hz * 1 Hz;
	end function;

	function kHz2Freq(f_kHz : natural) return FREQ is
	begin
		return f_kHz * 1 kHz;
	end function;

	function MHz2Freq(f_MHz : natural) return FREQ is
	begin
		return f_MHz * 1 MHz;
	end function;

	function GHz2Freq(f_GHz : natural) return FREQ is
	begin
		return f_GHz * 1 GHz;
	end function;

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

	-- convert physical types to standard type (REAL)
	-- ===========================================================================
	function to_real(t : time; scale : time) return REAL is
	begin
		if		(scale = 1	fs) then	return div(t, 1	 fs);
		elsif	(scale = 1	ps) then	return div(t, 1	 ps);
		elsif	(scale = 1	ns) then	return div(t, 1	 ns);
		elsif	(scale = 1	us) then	return div(t, 1	 us);
		elsif	(scale = 1	ms) then	return div(t, 1	 ms);
		elsif	(scale = 1 sec) then	return div(t, 1 sec);
		else	report "to_real: scale must have a value of '1 <unit>'" severity failure;
		return 0.0;
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
		return 0.0;
	end;

	function to_real(br : BAUD; scale : BAUD) return REAL is
	begin
		if		(scale = 1	Bd) then	return div(br, 1	Bd);
		elsif	(scale = 1 kBd) then	return div(br, 1 kBd);
		elsif	(scale = 1 MBd) then	return div(br, 1 MBd);
		elsif	(scale = 1 GBd) then	return div(br, 1 GBd);
		else	report "to_real: scale must have a value of '1 <unit>'" severity failure;
		end if;
		return 0.0;
	end;

	function to_real(mem : MEMORY; scale : MEMORY) return REAL is
	begin
		if		(scale = 1 Byte)	then	return div(mem, 1	Byte);
		elsif	(scale = 1 KiB)		then	return div(mem, 1 KiB);
		elsif	(scale = 1 MiB)		then	return div(mem, 1 MiB);
		elsif	(scale = 1 GiB)		then	return div(mem, 1 GiB);
		else	report "to_real: scale must have a value of '1 <unit>'" severity failure;
		end if;
		return 0.0;
	end;

	-- convert physical types to standard type (INTEGER)
	-- ===========================================================================
	function to_int(t : time; scale : time; RoundingStyle : T_ROUNDING_STYLE := ROUND_TO_NEAREST) return integer is
	begin
		case RoundingStyle is
			when ROUND_UP =>					return integer(ceil(to_real(t, scale)));
			when ROUND_DOWN =>				return integer(floor(to_real(t, scale)));
			when ROUND_TO_NEAREST =>	return integer(round(to_real(t, scale)));
			when others =>						null;
		end case;
		report "to_int: unsupported RoundingStyle: " & T_ROUNDING_STYLE'image(RoundingStyle) severity failure;
		return 0;
	end;

	function to_int(f : FREQ; scale : FREQ; RoundingStyle : T_ROUNDING_STYLE := ROUND_TO_NEAREST) return integer is
	begin
		case RoundingStyle is
			when ROUND_UP =>					return integer(ceil(to_real(f, scale)));
			when ROUND_DOWN =>				return integer(floor(to_real(f, scale)));
			when ROUND_TO_NEAREST =>	return integer(round(to_real(f, scale)));
			when others =>						null;
		end case;
		report "to_int: unsupported RoundingStyle: " & T_ROUNDING_STYLE'image(RoundingStyle) severity failure;
		return 0;
	end;

	function to_int(br : BAUD; scale : BAUD; RoundingStyle : T_ROUNDING_STYLE := ROUND_TO_NEAREST) return integer is
	begin
		case RoundingStyle is
			when ROUND_UP =>					return integer(ceil(to_real(br, scale)));
			when ROUND_DOWN =>				return integer(floor(to_real(br, scale)));
			when ROUND_TO_NEAREST =>	return integer(round(to_real(br, scale)));
			when others =>						null;
		end case;
		report "to_int: unsupported RoundingStyle: " & T_ROUNDING_STYLE'image(RoundingStyle) severity failure;
		return 0;
	end;

	function to_int(mem : MEMORY; scale : MEMORY; RoundingStyle : T_ROUNDING_STYLE := ROUND_UP) return integer is
	begin
		case RoundingStyle is
			when ROUND_UP =>					return integer(ceil(to_real(mem, scale)));
			when ROUND_DOWN =>				return integer(floor(to_real(mem, scale)));
			when ROUND_TO_NEAREST =>	return integer(round(to_real(mem, scale)));
			when others =>						null;
		end case;
		report "to_int: unsupported RoundingStyle: " & T_ROUNDING_STYLE'image(RoundingStyle) severity failure;
		return 0;
	end;

	-- calculate needed counter cycles to achieve a given 1. timing/delay and 2. frequency/period
	-- ===========================================================================
	--	@param Timing					A given timing or delay, which should be achieved
	--	@param Clock_Period		The period of the circuits clock
	--	@RoundingStyle				Default = ROUND_UP; other choises: ROUND_UP, ROUND_DOWN, ROUND_TO_NEAREST
	function TimingToCycles(Timing : time; Clock_Period : time; RoundingStyle : T_ROUNDING_STYLE := ROUND_UP) return natural is
		variable res_real	: REAL;
		variable res_nat	: natural;
		variable res_time	: time;
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
		res_dev		:= (div(res_time, Timing) - 1.0) * 100.0;

		if POC_VERBOSE then
			report "TimingToCycles: " & 	LF &
						 "  Timing: " &					to_string(Timing, 3) & LF &
						 "  Clock_Period: " &		to_string(Clock_Period, 3) & LF &
						 "  RoundingStyle: " &	str_substr(T_ROUNDING_STYLE'image(RoundingStyle), 7) & LF &
						 "  res_real = " &			str_format(res_real, 3) & LF &
						 "  => " &							integer'image(res_nat)
			severity note;
		end if;

		if C_PHYSICAL_REPORT_TIMING_DEVIATION then
			report "TimingToCycles (timing deviation report): " & LF &
						 "  timing to achieve: " & to_string(Timing, 3) & LF &
						 "  calculated cycles: " & integer'image(res_nat) & " cy" & LF &
						 "  resulting timing:  " & to_string(res_time, 3) & LF &
						 "  deviation:         " & to_string(res_time - Timing, 3) & " (" & str_format(res_dev, 2) & "%)"
			severity note;
		end if;

		return res_nat;
	end;

	function TimingToCycles(Timing : time; Clock_Frequency	: FREQ; RoundingStyle : T_ROUNDING_STYLE := ROUND_UP) return natural is
	begin
		return TimingToCycles(Timing, to_time(Clock_Frequency), RoundingStyle);
	end function;

	function CyclesToDelay(Cycles : natural; Clock_Period : time) return time is
	begin
		return Clock_Period * Cycles;
	end function;

	function CyclesToDelay(Cycles : natural; Clock_Frequency : FREQ) return time is
	begin
		return CyclesToDelay(Cycles, to_time(Clock_Frequency));
	end function;

	-- convert and format physical types to STRING
	function to_string(t : time; precision : natural) return string is
		variable tt     : time;
		variable unit		: string(1 to 3)	:= (others => C_POC_NUL);
		variable value	: REAL;
	begin
		tt := abs t;
		if (tt < 1 ps) then
			unit(1 to 2)	:= "fs";
			value					:= to_real(tt, 1 fs);
		elsif (tt < 1 ns) then
			unit(1 to 2)	:= "ps";
			value					:= to_real(tt, 1 ps);
		elsif (tt < 1 us) then
			unit(1 to 2)	:= "ns";
			value					:= to_real(tt, 1 ns);
		elsif (tt < 1 ms) then
			unit(1 to 2)	:= "us";
			value					:= to_real(tt, 1 us);
		elsif (tt < 1 sec) then
			unit(1 to 2)	:= "ms";
			value					:= to_real(tt, 1 ms);
		else
			unit					:= "sec";
			value					:= to_real(tt, 1 sec);
		end if;

		return ite(t >= 0 fs, str_format(value, precision) & " " & str_trim(unit),
							      '-' & str_format(value, precision) & " " & str_trim(unit));
	end function;

	function to_string(f : FREQ; precision : natural) return string is
		variable unit		: string(1 to 3)	:= (others => C_POC_NUL);
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
		else
			unit					:= "GHz";
			value					:= to_real(f, 1 GHz);
		end if;

		return str_format(value, precision) & " " & str_trim(unit);
	end function;

	function to_string(br : BAUD; precision : natural) return string is
		variable unit		: string(1 to 3)	:= (others => C_POC_NUL);
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

	function to_string(mem : MEMORY; precision : natural) return string is
		variable unit		: string(1 to 3)	:= (others => C_POC_NUL);
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
		else
			unit					:= "GiB";
			value					:= to_real(mem, 1 GiB);
		end if;

		return str_format(value, precision) & " " & str_trim(unit);
	end function;

end package body;
