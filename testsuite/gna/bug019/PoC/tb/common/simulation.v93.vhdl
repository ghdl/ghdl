-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- =============================================================================
-- Testbench:				Simulation constants, functions and utilities.
-- 
-- Authors:					Patrick Lehmann
--									Thomas B. Preusser
-- 
-- Description:
-- ------------------------------------
--		TODO
--
-- License:
-- =============================================================================
-- Copyright 2007-2014 Technische Universitaet Dresden - Germany
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
-- =============================================================================

library IEEE;
use			IEEE.STD_LOGIC_1164.all;

library PoC;
use			PoC.vectors.all;
use			PoC.strings.all;
use			PoC.physical.all;


package simulation is
	-- predefined constants to ease testvector concatenation
	constant U8								: T_SLV_8							:= (others => 'U');
	constant U16							: T_SLV_16						:= (others => 'U');
	constant U24							: T_SLV_24						:= (others => 'U');
	constant U32							: T_SLV_32						:= (others => 'U');

	constant D8								: T_SLV_8							:= (others => '-');
	constant D16							: T_SLV_16						:= (others => '-');
	constant D24							: T_SLV_24						:= (others => '-');
	constant D32							: T_SLV_32						:= (others => '-');

	-- Testbench Status Management
	-- ===========================================================================
	-- The testbench is marked as failed. If a message is provided, it is
	-- reported as an error.
	procedure tbFail(msg : in string := "");

	-- If the passed condition has evaluated false, the testbench is marked
	-- as failed. In this case, the optional message will be reported as an
	-- error if one was provided.
	procedure tbAssert(cond : in boolean; msg : in string := "");

	-- Prints out the overall testbench result as defined by the automated
	-- testbench process. Unless tbFail() or tbAssert() with a false condition
	-- have been called before, a successful completion will be reported, a
	-- failure otherwise.
	procedure tbPrintResult;

	-- clock generation
	-- ===========================================================================
	subtype T_DutyCycle is REAL range 0.0 to 1.0;
	
	procedure simStop;
	impure function simIsStopped return BOOLEAN;
	procedure simGenerateClock(signal Clock : out STD_LOGIC; constant Frequency : in FREQ; constant DutyCycle : T_DutyCycle := 0.5);
	procedure simGenerateClock(signal Clock : out STD_LOGIC; constant Period : in TIME; constant DutyCycle : T_DutyCycle := 0.5);
	
	-- waveform generation
	-- ===========================================================================
	type T_SIM_WAVEFORM_TUPLE_SL is record
		Delay		: TIME;
		Value		: STD_LOGIC;
	end record;
	
	type T_SIM_WAVEFORM_TUPLE_SLV_8 is record
		Delay		: TIME;
		Value		: T_SLV_8;
	end record;
	
	type T_SIM_WAVEFORM_TUPLE_SLV_16 is record
		Delay		: TIME;
		Value		: T_SLV_16;
	end record;
	
	type T_SIM_WAVEFORM_TUPLE_SLV_24 is record
		Delay		: TIME;
		Value		: T_SLV_24;
	end record;
	
	type T_SIM_WAVEFORM_TUPLE_SLV_32 is record
		Delay		: TIME;
		Value		: T_SLV_32;
	end record;
	
	type T_SIM_WAVEFORM_TUPLE_SLV_48 is record
		Delay		: TIME;
		Value		: T_SLV_48;
	end record;
	
	type T_SIM_WAVEFORM_TUPLE_SLV_64 is record
		Delay		: TIME;
		Value		: T_SLV_64;
	end record;
	
	type T_SIM_WAVEFORM_SL			is array(NATURAL range <>) of T_SIM_WAVEFORM_TUPLE_SL;
	type T_SIM_WAVEFORM_SLV_8		is array(NATURAL range <>) of T_SIM_WAVEFORM_TUPLE_SLV_8;
	type T_SIM_WAVEFORM_SLV_16	is array(NATURAL range <>) of T_SIM_WAVEFORM_TUPLE_SLV_16;
	type T_SIM_WAVEFORM_SLV_24	is array(NATURAL range <>) of T_SIM_WAVEFORM_TUPLE_SLV_24;
	type T_SIM_WAVEFORM_SLV_32	is array(NATURAL range <>) of T_SIM_WAVEFORM_TUPLE_SLV_32;
	type T_SIM_WAVEFORM_SLV_48	is array(NATURAL range <>) of T_SIM_WAVEFORM_TUPLE_SLV_48;
	type T_SIM_WAVEFORM_SLV_64	is array(NATURAL range <>) of T_SIM_WAVEFORM_TUPLE_SLV_64;
	
	procedure simGenerateWaveform(signal Wave : out BOOLEAN;		Waveform: T_TIMEVEC;							InitialValue : BOOLEAN);
	procedure simGenerateWaveform(signal Wave : out STD_LOGIC;	Waveform: T_TIMEVEC;							InitialValue : STD_LOGIC := '0');
	procedure simGenerateWaveform(signal Wave : out STD_LOGIC;	Waveform: T_SIM_WAVEFORM_SL;			InitialValue : STD_LOGIC := '0');
	procedure simGenerateWaveform(signal Wave : out T_SLV_8;		Waveform: T_SIM_WAVEFORM_SLV_8;		InitialValue : T_SLV_8);
	procedure simGenerateWaveform(signal Wave : out T_SLV_16;		Waveform: T_SIM_WAVEFORM_SLV_16;	InitialValue : T_SLV_16);
	procedure simGenerateWaveform(signal Wave : out T_SLV_24;		Waveform: T_SIM_WAVEFORM_SLV_24;	InitialValue : T_SLV_24);
	procedure simGenerateWaveform(signal Wave : out T_SLV_32;		Waveform: T_SIM_WAVEFORM_SLV_32;	InitialValue : T_SLV_32);
	procedure simGenerateWaveform(signal Wave : out T_SLV_48;		Waveform: T_SIM_WAVEFORM_SLV_48;	InitialValue : T_SLV_48);
	procedure simGenerateWaveform(signal Wave : out T_SLV_64;		Waveform: T_SIM_WAVEFORM_SLV_64;	InitialValue : T_SLV_64);
	
	function simGenerateWaveform_Reset(constant Pause : TIME := 0 ns; ResetPulse : TIME := 10 ns) return T_TIMEVEC;
	
	-- TODO: integrate VCD simulation functions and procedures from sim_value_change_dump.vhdl here
	
	-- checksum functions
	-- ===========================================================================
	-- TODO: move checksum functions here
end;


use	std.TextIO.all;

package body simulation is

	-- Testbench Status Management
	-- ===========================================================================
	-- Internal state variable to log a failure condition for final reporting.
	-- Once de-asserted, this variable will never return to a value of true.
	shared variable pass					: boolean		:= true;
	shared variable simStopped		: BOOLEAN		:= FALSE;

	procedure tbFail(msg : in string := "") is
	begin
		if (str_length(msg) > 0) then
			report str_trim(msg) severity error;
		end if;
		pass := false;
	end;

	procedure tbAssert(cond : in boolean; msg : in string := "") is
	begin
		if not cond then
			tbFail(msg);
		end if;
	end;

	procedure tbPrintResult is
		variable l : line;
	begin
		write(l, string'("SIMULATION RESULT = "));
		if pass then
			write(l, string'("PASSED"));
		else
			write(l, string'("FAILED"));
		end if;
		writeline(output, l);
	end procedure;

	-- clock generation
	procedure simStop is
	begin
		simStopped	:= TRUE;
	end procedure;
	
	impure function simIsStopped return BOOLEAN is
	begin
		return simStopped;
	end function;
	
	procedure simGenerateClock(signal Clock : out STD_LOGIC; constant Frequency : in FREQ; constant DutyCycle : T_DutyCycle := 0.5) is
		constant Period : TIME := to_time(Frequency);
	begin
		simGenerateClock(Clock, Period, DutyCycle);
	end procedure;
	
	procedure simGenerateClock(signal Clock : out STD_LOGIC; constant Period : in TIME; constant DutyCycle : T_DutyCycle := 0.5) is
		constant TIME_HIGH	: TIME := Period * DutyCycle;
		constant TIME_LOW		: TIME := Period - TIME_HIGH;
	begin
		Clock		<= '0';

		while (not simStopped) loop
			wait for TIME_LOW;
			Clock		<= '1';
			wait for TIME_HIGH;
			Clock		<= '0';
		end loop;
	end procedure;
	
	-- waveform generation
	procedure simGenerateWaveform(signal Wave : out BOOLEAN; Waveform : T_TIMEVEC; InitialValue : BOOLEAN) is
		variable State : BOOLEAN := InitialValue;
	begin
		Wave <= State;
		for i in Waveform'range loop
			wait for Waveform(i);
			State := not State;
			Wave		<= State;
		end loop;
	end procedure;
	
	procedure simGenerateWaveform(signal Wave : out STD_LOGIC; Waveform: T_TIMEVEC; InitialValue : STD_LOGIC := '0') is
		variable State : STD_LOGIC := InitialValue;
	begin
		Wave <= State;
		for i in Waveform'range loop
			wait for Waveform(i);
			State := not State;
			Wave		<= State;
		end loop;
	end procedure;

	procedure simGenerateWaveform(signal Wave : out STD_LOGIC; Waveform: T_SIM_WAVEFORM_SL; InitialValue : STD_LOGIC := '0') is
	begin
		Wave <= InitialValue;
		for i in Waveform'range loop
			wait for Waveform(i).Delay;
			Wave		<= Waveform(i).Value;
		end loop;
	end procedure;
	
	procedure simGenerateWaveform(signal Wave : out T_SLV_8; Waveform: T_SIM_WAVEFORM_SLV_8; InitialValue : T_SLV_8) is
	begin
		Wave <= InitialValue;
		for i in Waveform'range loop
			wait for Waveform(i).Delay;
			Wave		<= Waveform(i).Value;
		end loop;
	end procedure;
	
	procedure simGenerateWaveform(signal Wave : out T_SLV_16; Waveform: T_SIM_WAVEFORM_SLV_16; InitialValue : T_SLV_16) is
	begin
		Wave <= InitialValue;
		for i in Waveform'range loop
			wait for Waveform(i).Delay;
			Wave		<= Waveform(i).Value;
		end loop;
	end procedure;
	
	procedure simGenerateWaveform(signal Wave : out T_SLV_24; Waveform: T_SIM_WAVEFORM_SLV_24; InitialValue : T_SLV_24) is
	begin
		Wave <= InitialValue;
		for i in Waveform'range loop
			wait for Waveform(i).Delay;
			Wave		<= Waveform(i).Value;
		end loop;
	end procedure;
	
	procedure simGenerateWaveform(signal Wave : out T_SLV_32; Waveform: T_SIM_WAVEFORM_SLV_32; InitialValue : T_SLV_32) is
	begin
		Wave <= InitialValue;
		for i in Waveform'range loop
			wait for Waveform(i).Delay;
			Wave		<= Waveform(i).Value;
		end loop;
	end procedure;
	
	procedure simGenerateWaveform(signal Wave : out T_SLV_48; Waveform: T_SIM_WAVEFORM_SLV_48; InitialValue : T_SLV_48) is
	begin
		Wave <= InitialValue;
		for i in Waveform'range loop
			wait for Waveform(i).Delay;
			Wave		<= Waveform(i).Value;
		end loop;
	end procedure;
	
	procedure simGenerateWaveform(signal Wave : out T_SLV_64; Waveform: T_SIM_WAVEFORM_SLV_64; InitialValue : T_SLV_64) is
	begin
		Wave <= InitialValue;
		for i in Waveform'range loop
			wait for Waveform(i).Delay;
			Wave		<= Waveform(i).Value;
		end loop;
	end procedure;
	
	function simGenerateWaveform_Reset(constant Pause : TIME := 0 ns; ResetPulse : TIME := 10 ns) return T_TIMEVEC is
	begin
		return (0 => Pause, 1 => ResetPulse);
	end function;
	
	-- checksum functions
	-- ===========================================================================
	-- TODO: move checksum functions here
end package body;
