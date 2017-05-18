-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- =============================================================================
-- Authors:					Patrick Lehmann
-- 									Martin Zabel
--
-- Package:					Simulation constants, functions and utilities.
--
-- Description:
-- -------------------------------------
-- .. TODO:: No documentation available.
--
-- License:
-- =============================================================================
-- Copyright 2007-2016 Technische Universitaet Dresden - Germany
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
use			IEEE.std_logic_1164.all;
use			IEEE.numeric_std.all;
use			IEEE.math_real.all;

library PoC;
use			PoC.utils.all;
-- use			PoC.strings.all;
use			PoC.vectors.all;
use			PoC.physical.all;

use			PoC.sim_types.all;
-- use			PoC.sim_random.all;
use			PoC.simulation.all;


package waveform is
	-- clock generation
	-- ===========================================================================
	procedure simGenerateClock(
		signal	 Clock			: out	std_logic;
		constant Frequency	: in	FREQ;
		constant Phase			: in	T_PHASE			:=	0 deg;
		constant DutyCycle	: in	T_DUTYCYCLE	:= 50 percent;
		constant Wander			: in	T_WANDER		:=	0 permil
	);
	procedure simGenerateClock(
		constant TestID			: in	T_SIM_TEST_ID;
		signal	 Clock			: out	std_logic;
		constant Frequency	: in	FREQ;
		constant Phase			: in	T_PHASE			:=	0 deg;
		constant DutyCycle	: in	T_DUTYCYCLE	:= 50 percent;
		constant Wander			: in	T_WANDER		:=	0 permil
	);
	procedure simGenerateClock(
		signal	 Clock			: out	std_logic;
		constant Period			: in	time;
		constant Phase			: in	T_PHASE			:=	0 deg;
		constant DutyCycle	: in	T_DUTYCYCLE	:= 50 percent;
		constant Wander			: in	T_WANDER		:=	0 permil
	);
	procedure simGenerateClock(
		constant TestID			: in	T_SIM_TEST_ID;
		signal	 Clock			: out	std_logic;
		constant Period			: in	time;
		constant Phase			: in	T_PHASE			:=	0 deg;
		constant DutyCycle	: in	T_DUTYCYCLE	:= 50 percent;
		constant Wander			: in	T_WANDER		:=	0 permil
	);

	procedure simWaitUntilRisingEdge(signal Clock : in std_logic; constant Times : in positive);
	procedure simWaitUntilRisingEdge(constant TestID : in T_SIM_TEST_ID; signal Clock : in std_logic; constant Times : in positive);
	procedure simWaitUntilFallingEdge(signal Clock : in std_logic; constant Times : in positive);
	procedure simWaitUntilFallingEdge(constant TestID : in T_SIM_TEST_ID; signal Clock : in std_logic; constant Times : in positive);

	procedure simGenerateClock2(constant TestID : in T_SIM_TEST_ID; signal Clock : out std_logic; signal Debug : out REAL; constant Period : in time);

	-- waveform description
	-- ===========================================================================
	type T_SIM_WAVEFORM_TUPLE_SL is record
		Delay		: time;
		Value		: std_logic;
	end record;

	type T_SIM_WAVEFORM_TUPLE_SLV_8 is record
		Delay		: time;
		Value		: T_SLV_8;
	end record;

	type T_SIM_WAVEFORM_TUPLE_SLV_16 is record
		Delay		: time;
		Value		: T_SLV_16;
	end record;

	type T_SIM_WAVEFORM_TUPLE_SLV_24 is record
		Delay		: time;
		Value		: T_SLV_24;
	end record;

	type T_SIM_WAVEFORM_TUPLE_SLV_32 is record
		Delay		: time;
		Value		: T_SLV_32;
	end record;

	type T_SIM_WAVEFORM_TUPLE_SLV_48 is record
		Delay		: time;
		Value		: T_SLV_48;
	end record;

	type T_SIM_WAVEFORM_TUPLE_SLV_64 is record
		Delay		: time;
		Value		: T_SLV_64;
	end record;

	subtype T_SIM_WAVEFORM			is TIME_VECTOR; -- use predefined physical type TIME here
	type T_SIM_WAVEFORM_SL			is array(natural range <>) of T_SIM_WAVEFORM_TUPLE_SL;
	type T_SIM_WAVEFORM_SLV_8		is array(natural range <>) of T_SIM_WAVEFORM_TUPLE_SLV_8;
	type T_SIM_WAVEFORM_SLV_16	is array(natural range <>) of T_SIM_WAVEFORM_TUPLE_SLV_16;
	type T_SIM_WAVEFORM_SLV_24	is array(natural range <>) of T_SIM_WAVEFORM_TUPLE_SLV_24;
	type T_SIM_WAVEFORM_SLV_32	is array(natural range <>) of T_SIM_WAVEFORM_TUPLE_SLV_32;
	type T_SIM_WAVEFORM_SLV_48	is array(natural range <>) of T_SIM_WAVEFORM_TUPLE_SLV_48;
	type T_SIM_WAVEFORM_SLV_64	is array(natural range <>) of T_SIM_WAVEFORM_TUPLE_SLV_64;

	-- waveform generation procedures
	-- ===========================================================================
	-- TODO: get initial value from Waveform(0) if .Delay = o fs, otherwise use (others => 'U') ?
	procedure simGenerateWaveform(
		signal	 Wave					: out	boolean;
		constant Waveform			: in	T_SIM_WAVEFORM;
		constant InitialValue	: in	boolean					:= FALSE
	);
	procedure simGenerateWaveform(
		constant TestID				: in	T_SIM_TEST_ID;
		signal	 Wave					: out	boolean;
		constant Waveform			: in	T_SIM_WAVEFORM;
		constant InitialValue	: in	boolean					:= FALSE
	);
	procedure simGenerateWaveform(
		signal	 Wave					: out	std_logic;
		constant Waveform			: in	T_SIM_WAVEFORM;
		constant InitialValue	: in	std_logic				:= '0'
	);
	procedure simGenerateWaveform(
		constant TestID				: in	T_SIM_TEST_ID;
		signal	 Wave					: out	std_logic;
		constant Waveform			: in	T_SIM_WAVEFORM;
		constant InitialValue	: in	std_logic				:= '0'
	);
	procedure simGenerateWaveform(
		signal	 Wave					: out	std_logic;
		constant Waveform			: in	T_SIM_WAVEFORM_SL;
		constant InitialValue	: in	std_logic				:= '0'
	);
	procedure simGenerateWaveform(
		constant TestID				: in	T_SIM_TEST_ID;
		signal	 Wave					: out	std_logic;
		constant Waveform			: in	T_SIM_WAVEFORM_SL;
		constant InitialValue	: in	std_logic				:= '0'
	);
	procedure simGenerateWaveform(
		signal	 Wave					: out	T_SLV_8;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_8;
		constant InitialValue	: in	T_SLV_8					:= (others => '0')
	);
	procedure simGenerateWaveform(
		constant TestID				: in	T_SIM_TEST_ID;
		signal	 Wave					: out	T_SLV_8;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_8;
		constant InitialValue	: in	T_SLV_8					:= (others => '0')
	);
	procedure simGenerateWaveform(
		signal	 Wave					: out	T_SLV_16;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_16;
		constant InitialValue	: in	T_SLV_16				:= (others => '0')
	);
	procedure simGenerateWaveform(
		constant TestID				: in	T_SIM_TEST_ID;
		signal	 Wave					: out	T_SLV_16;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_16;
		constant InitialValue	: in	T_SLV_16				:= (others => '0')
	);
	procedure simGenerateWaveform(
		signal	 Wave					: out	T_SLV_24;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_24;
		constant InitialValue	: in	T_SLV_24				:= (others => '0')
	);
	procedure simGenerateWaveform(
		constant TestID				: in	T_SIM_TEST_ID;
		signal	 Wave					: out	T_SLV_24;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_24;
		constant InitialValue	: in	T_SLV_24				:= (others => '0')
	);
	procedure simGenerateWaveform(
		signal	 Wave					: out	T_SLV_32;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_32;
		constant InitialValue	: in	T_SLV_32				:= (others => '0')
	);
	procedure simGenerateWaveform(
		constant TestID				: in	T_SIM_TEST_ID;
		signal	 Wave					: out	T_SLV_32;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_32;
		constant InitialValue	: in	T_SLV_32				:= (others => '0')
	);
	procedure simGenerateWaveform(
		signal	 Wave					: out	T_SLV_48;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_48;
		constant InitialValue	: in	T_SLV_48				:= (others => '0')
	);
	procedure simGenerateWaveform(
		constant TestID				: in	T_SIM_TEST_ID;
		signal	 Wave					: out	T_SLV_48;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_48;
		constant InitialValue	: in	T_SLV_48				:= (others => '0')
	);
	procedure simGenerateWaveform(
		signal	 Wave					: out	T_SLV_64;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_64;
		constant InitialValue	: in	T_SLV_64				:= (others => '0')
	);
	procedure simGenerateWaveform(
		constant TestID				: in	T_SIM_TEST_ID;
		signal	 Wave					: out	T_SLV_64;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_64;
		constant InitialValue	: in	T_SLV_64				:= (others => '0')
	);

	function "*" (Wave : T_SIM_WAVEFORM; Times : natural) return T_SIM_WAVEFORM;
	function ">" (Wave : T_SIM_WAVEFORM; Offset : time) return T_SIM_WAVEFORM;
	function "<" (Wave : T_SIM_WAVEFORM; Offset : time) return T_SIM_WAVEFORM;

	function "*" (Wave : T_SIM_WAVEFORM_SLV_8; Times : natural) return T_SIM_WAVEFORM_SLV_8;
	function ">" (Wave : T_SIM_WAVEFORM_SLV_8; Offset : time) return T_SIM_WAVEFORM_SLV_8;
	-- function "<" (Wave : T_SIM_WAVEFORM_SLV_8; Offset : TIME) return T_SIM_WAVEFORM_SLV_8;

	function "*" (Wave : T_SIM_WAVEFORM_SLV_16; Times : natural) return T_SIM_WAVEFORM_SLV_16;
	function ">" (Wave : T_SIM_WAVEFORM_SLV_16; Offset : time) return T_SIM_WAVEFORM_SLV_16;
	-- function "<" (Wave : T_SIM_WAVEFORM_SLV_16; Offset : TIME) return T_SIM_WAVEFORM_SLV_16;

	function "*" (Wave : T_SIM_WAVEFORM_SLV_24; Times : natural) return T_SIM_WAVEFORM_SLV_24;
	function ">" (Wave : T_SIM_WAVEFORM_SLV_24; Offset : time) return T_SIM_WAVEFORM_SLV_24;
	-- function "<" (Wave : T_SIM_WAVEFORM_SLV_24; Offset : TIME) return T_SIM_WAVEFORM_SLV_24;

	function "*" (Wave : T_SIM_WAVEFORM_SLV_32; Times : natural) return T_SIM_WAVEFORM_SLV_32;
	function ">" (Wave : T_SIM_WAVEFORM_SLV_32; Offset : time) return T_SIM_WAVEFORM_SLV_32;
	-- function "<" (Wave : T_SIM_WAVEFORM_SLV_32; Offset : TIME) return T_SIM_WAVEFORM_SLV_32;

	function "*" (Wave : T_SIM_WAVEFORM_SLV_48; Times : natural) return T_SIM_WAVEFORM_SLV_48;
	function ">" (Wave : T_SIM_WAVEFORM_SLV_48; Offset : time) return T_SIM_WAVEFORM_SLV_48;
	-- function "<" (Wave : T_SIM_WAVEFORM_SLV_48; Offset : TIME) return T_SIM_WAVEFORM_SLV_48;

	function "*" (Wave : T_SIM_WAVEFORM_SLV_64; Times : natural) return T_SIM_WAVEFORM_SLV_64;
	function ">" (Wave : T_SIM_WAVEFORM_SLV_64; Offset : time) return T_SIM_WAVEFORM_SLV_64;
	-- function "<" (Wave : T_SIM_WAVEFORM_SLV_64; Offset : TIME) return T_SIM_WAVEFORM_SLV_64;

	-- convert arrays to waveforms
	-- TODO: optimize waveform if input data doesn't change
	-- TODO: write single bit variant
	function to_waveform(bv : bit_vector; Delay : time) return T_SIM_WAVEFORM;
	function to_waveform(slv : std_logic_vector; Delay : time) return T_SIM_WAVEFORM_SL;
	function to_waveform(slvv : T_SLVV_8; Delay : time) return T_SIM_WAVEFORM_SLV_8;
	function to_waveform(slvv : T_SLVV_16; Delay : time) return T_SIM_WAVEFORM_SLV_16;
	function to_waveform(slvv : T_SLVV_24; Delay : time) return T_SIM_WAVEFORM_SLV_24;
	function to_waveform(slvv : T_SLVV_32; Delay : time) return T_SIM_WAVEFORM_SLV_32;
	function to_waveform(slvv : T_SLVV_48; Delay : time) return T_SIM_WAVEFORM_SLV_48;
	function to_waveform(slvv : T_SLVV_64; Delay : time) return T_SIM_WAVEFORM_SLV_64;

	-- predefined common waveforms
	function simGenerateWaveform_Reset(constant Pause : time := 0 ns; ResetPulse : time := 10 ns) return T_SIM_WAVEFORM;

	-- TODO: integrate VCD simulation functions and procedures from sim_value_change_dump.vhdl here
	end package;


package body waveform is
	-- clock generation
	-- ===========================================================================
	procedure simGenerateClock(
		signal	 Clock			: out	std_logic;
		constant Frequency	: in	FREQ;
		constant Phase			: in	T_PHASE			:=	0 deg;
		constant DutyCycle	: in	T_DUTYCYCLE	:= 50 percent;
		constant Wander			: in	T_WANDER		:=	0 permil
	) is
		constant Period : time := to_time(Frequency);
	begin
		simGenerateClock(C_SIM_DEFAULT_TEST_ID, Clock, Period, Phase, DutyCycle, Wander);
	end procedure;

	procedure simGenerateClock(
		constant TestID			: in	T_SIM_TEST_ID;
		signal	 Clock			: out	std_logic;
		constant Frequency	: in	FREQ;
		constant Phase			: in	T_PHASE			:=	0 deg;
		constant DutyCycle	: in	T_DUTYCYCLE	:= 50 percent;
		constant Wander			: in	T_WANDER		:=	0 permil
	) is
		constant Period : time := to_time(Frequency);
	begin
		simGenerateClock(TestID, Clock, Period, Phase, DutyCycle, Wander);
	end procedure;

	procedure simGenerateClock(
		signal	 Clock			: out	std_logic;
		constant Period			: in	time;
		constant Phase			: in	T_PHASE			:=	0 deg;
		constant DutyCycle	: in	T_DUTYCYCLE	:= 50 percent;
		constant Wander			: in	T_WANDER		:=	0 permil
	) is
	begin
		simGenerateClock(C_SIM_DEFAULT_TEST_ID, Clock, Period, Phase, DutyCycle, Wander);
	end procedure;

	procedure simGenerateClock(
		constant TestID			: in	T_SIM_TEST_ID;
		signal	 Clock			: out	std_logic;
		constant Period			: in	time;
		constant Phase			: in	T_PHASE			:=	0 deg;
		constant DutyCycle	: in	T_DUTYCYCLE	:= 50 percent;
		constant Wander			: in	T_WANDER		:=	0 permil
	) is
		constant NormalizedPhase		: T_PHASE		:= ite((Phase >= 0 deg), Phase, Phase + 360 deg);						-- move Phase into the range of 0° to 360°
		constant PhaseAsFactor			: REAL			:= real(NormalizedPhase / 1 second) / 1296000.0;						-- 1,296,000 = 3,600 seconds * 360 degree per cycle
		constant WanderAsFactor			: REAL			:= real(Wander / 1 ppb) / 1.0e9;
		constant DutyCycleAsFactor	: REAL			:= real(DutyCycle / 1 permil) / 1000.0;
		constant Delay							: time			:= Period * PhaseAsFactor;
		constant TimeHigh						: time			:= Period * DutyCycleAsFactor + (Period * (WanderAsFactor / 2.0));	-- add 50% wander to the high level
		constant TimeLow						: time			:= Period - TimeHigh + (Period * WanderAsFactor);						-- and 50% to the low level
		constant ClockAfterRun_cy		: positive	:= 5;

		constant PROCESS_ID					: T_SIM_PROCESS_ID	:= simRegisterProcess(TestID, "simGenerateClock(period=" & to_string(Period, 2) & ")", IsLowPriority => TRUE);
	begin
		-- report "simGenerateClock: (Instance: '" & Clock'instance_name & "')" & LF &
			-- "Period: "						& TIME'image(Period) & LF &
			-- "Phase: "							& T_PHASE'image(Phase) & LF &
			-- "DutyCycle: "					& T_DUTYCYCLE'image(DutyCycle) & LF &
			-- "PhaseAsFactor: "			& REAL'image(PhaseAsFactor) & LF &
			-- "WanderAsFactor: "		& REAL'image(WanderAsFactor) & LF &
			-- "DutyCycleAsFactor: "	& REAL'image(DutyCycleAsFactor) & LF &
			-- "Delay: "							& TIME'image(Delay) & LF &
			-- "TimeHigh: "					& TIME'image(TimeHigh) & LF &
			-- "TimeLow: "						& TIME'image(TimeLow)
			-- severity NOTE;

		if (Delay = 0 ns) then
			null;
		elsif (Delay <= TimeLow) then
			Clock		<= '0';
			wait for Delay;
		else
			Clock		<= '1';
			wait for Delay - TimeLow;
			Clock		<= '0';
			wait for TimeLow;
		end if;
		Clock		<= '1';
		while not simIsStopped(TestID) loop
			wait for TimeHigh;
			Clock		<= '0';
			wait for TimeLow;
			Clock		<= '1';
		end loop;
		simDeactivateProcess(PROCESS_ID);
		-- create N more cycles to allow other processes to recognize the stop condition (clock after run)
		for i in 1 to ClockAfterRun_cy loop
			wait for TimeHigh;
			Clock		<= '0';
			wait for TimeLow;
			Clock		<= '1';
		end loop;
		Clock		<= '0';
	end procedure;

	type T_SIM_NORMAL_DIST_PARAMETER is record
		StandardDeviation		: REAL;
		Mean								: REAL;
	end record;
	type T_JITTER_DISTRIBUTION is array (natural range <>) of T_SIM_NORMAL_DIST_PARAMETER;

	procedure simGenerateClock2(
		constant	TestID	: in	T_SIM_TEST_ID;
		signal		Clock		: out	std_logic;
		signal		Debug		: out	REAL;
		constant	Period	: in	time
	) is
		constant TimeHigh							: time			:= Period * 0.5;
		constant TimeLow							: time			:= Period - TimeHigh;
		constant JitterPeakPeak				: REAL			:= 0.1;		-- UI
		constant JitterAsFactor				: REAL			:= JitterPeakPeak / 4.0;	-- Maximum jitter per edge
		constant JitterDistribution		: T_JITTER_DISTRIBUTION	:= (
			-- 0 => (StandardDeviation => 0.2, Mean => -0.4),
			-- 1 => (StandardDeviation => 0.2, Mean =>  0.4)

			-- 0 => (StandardDeviation => 0.2, Mean => -0.4),
			-- 1 => (StandardDeviation => 0.3, Mean => -0.1),
			-- 2 => (StandardDeviation => 0.5, Mean =>  0.0),
			-- 3 => (StandardDeviation => 0.3, Mean =>  0.1),
			-- 4 => (StandardDeviation => 0.2, Mean =>  0.4)

			0 => (StandardDeviation => 0.15,	Mean => -0.6),
			1 => (StandardDeviation => 0.2,		Mean => -0.3),
			2 => (StandardDeviation => 0.25,	Mean => -0.2),
			3 => (StandardDeviation => 0.3,		Mean =>  0.0),
			4 => (StandardDeviation => 0.25,	Mean =>  0.2),
			5 => (StandardDeviation => 0.2,		Mean =>  0.3),
			6 => (StandardDeviation => 0.15,	Mean =>  0.6)
		);
		variable Seed									: T_SIM_RAND_SEED;
		variable rand									: REAL;
		variable Jitter								: REAL;
		variable Index								: natural;

		constant ClockAfterRun_cy			: positive	:= 5;
	begin
		Clock		<= '1';
		randInitializeSeed(Seed);

		while not simIsStopped(TestID) loop
			ieee.math_real.Uniform(Seed.Seed1, Seed.Seed2, rand);
			Index		:= scale(rand, 0, JitterDistribution'length * 10) mod JitterDistribution'length;
			randNormalDistributedValue(Seed, rand, JitterDistribution(Index).StandardDeviation, JitterDistribution(Index).Mean, -1.0, 1.0);

			Jitter := JitterAsFactor * rand;
			Debug		<= rand;

			-- Debug		<= integer(rand * 256.0 + 256.0);
			wait for TimeHigh + (Period * Jitter);
			Clock		<= '0';
			wait for TimeLow + (Period * Jitter);
			Clock		<= '1';
		end loop;
		-- create N more cycles to allow other processes to recognize the stop condition (clock after run)
		for i in 1 to ClockAfterRun_cy loop
			wait for TimeHigh;
			Clock		<= '0';
			wait for TimeLow;
			Clock		<= '1';
		end loop;
		Clock		<= '0';
	end procedure;


	procedure simWaitUntilRisingEdge(signal Clock : in std_logic; constant Times : in positive) is
	begin
		simWaitUntilRisingEdge(C_SIM_DEFAULT_TEST_ID, Clock, Times);
	end procedure;

	procedure simWaitUntilRisingEdge(constant	TestID : in T_SIM_TEST_ID; signal Clock : in std_logic; constant Times : in positive) is
	begin
		for i in 1 to Times loop
			wait until rising_edge(Clock);
			exit when simIsStopped(TestID);
		end loop;
	end procedure;

	procedure simWaitUntilFallingEdge(signal Clock : in std_logic; constant Times : in positive) is
	begin
		simWaitUntilFallingEdge(C_SIM_DEFAULT_TEST_ID, Clock, Times);
	end procedure;

	procedure simWaitUntilFallingEdge(constant	TestID : in T_SIM_TEST_ID; signal Clock : in std_logic; constant Times : in positive) is
	begin
		for i in 1 to Times loop
			wait until falling_edge(Clock);
			exit when simIsStopped(TestID);
		end loop;
	end procedure;

	-- waveform generation
	-- ===========================================================================
	procedure simGenerateWaveform(
		signal	 Wave					: out	boolean;
		constant Waveform			: in	T_SIM_WAVEFORM;
		constant InitialValue	: in	boolean					:= FALSE
	) is
	begin
		simGenerateWaveform(C_SIM_DEFAULT_TEST_ID, Wave, Waveform, InitialValue);
	end procedure;

	procedure simGenerateWaveform(
		constant TestID				: in	T_SIM_TEST_ID;
		signal	 Wave					: out	boolean;
		constant Waveform			: in	T_SIM_WAVEFORM;
		constant InitialValue	: in	boolean					:= FALSE
	) is
		constant PROCESS_ID	: T_SIM_PROCESS_ID			:= simRegisterProcess(TestID, "simGenerateWaveform");
		variable State			: boolean;
	begin
		State	:= InitialValue;
		Wave	<= State;
		for i in Waveform'range loop
			wait for Waveform(i);
			State		:= not State;
			Wave		<= State;
			exit when simIsStopped(TestID);
		end loop;
		simDeactivateProcess(PROCESS_ID);
	end procedure;

	procedure simGenerateWaveform(
		signal	 Wave					: out	std_logic;
		constant Waveform			: in	T_SIM_WAVEFORM;
		constant InitialValue	: in	std_logic				:= '0'
	) is
	begin
		simGenerateWaveform(C_SIM_DEFAULT_TEST_ID, Wave, Waveform, InitialValue);
	end procedure;

	procedure simGenerateWaveform(
		constant TestID				: in	T_SIM_TEST_ID;
		signal	 Wave					: out	std_logic;
		constant Waveform			: in	T_SIM_WAVEFORM;
		constant InitialValue	: in	std_logic				:= '0'
	) is
		constant PROCESS_ID	: T_SIM_PROCESS_ID			:= simRegisterProcess(TestID, "simGenerateWaveform");
		variable State : std_logic;
	begin
		State	:= InitialValue;
		Wave	<= State;
		for i in Waveform'range loop
			wait for Waveform(i);
			State		:= not State;
			Wave		<= State;
			exit when simIsStopped(TestID);
		end loop;
		simDeactivateProcess(PROCESS_ID);
	end procedure;

	procedure simGenerateWaveform(
		signal	 Wave					: out	std_logic;
		constant Waveform			: in	T_SIM_WAVEFORM_SL;
		constant InitialValue	: in	std_logic				:= '0'
	) is
	begin
		simGenerateWaveform(C_SIM_DEFAULT_TEST_ID, Wave, Waveform, InitialValue);
	end procedure;

	procedure simGenerateWaveform(
		constant TestID				: in	T_SIM_TEST_ID;
		signal	 Wave					: out	std_logic;
		constant Waveform			: in	T_SIM_WAVEFORM_SL;
		constant InitialValue	: in	std_logic				:= '0'
	) is
		constant PROCESS_ID	: T_SIM_PROCESS_ID			:= simRegisterProcess(TestID, "simGenerateWaveform");
	begin
		Wave <= InitialValue;
		for i in Waveform'range loop
			wait for Waveform(i).Delay;
			Wave		<= Waveform(i).Value;
			exit when simIsStopped(TestID);
		end loop;
		simDeactivateProcess(PROCESS_ID);
	end procedure;

	procedure simGenerateWaveform(
		signal	 Wave					: out	T_SLV_8;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_8;
		constant InitialValue	: in	T_SLV_8				:= (others => '0')
	) is
	begin
		simGenerateWaveform(C_SIM_DEFAULT_TEST_ID, Wave, Waveform, InitialValue);
	end procedure;

	procedure simGenerateWaveform(
		constant TestID				: in	T_SIM_TEST_ID;
		signal	 Wave					: out	T_SLV_8;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_8;
		constant InitialValue	: in	T_SLV_8				:= (others => '0')
	) is
		constant PROCESS_ID	: T_SIM_PROCESS_ID			:= simRegisterProcess(TestID, "simGenerateWaveform");
	begin
		Wave <= InitialValue;
		for i in Waveform'range loop
			wait for Waveform(i).Delay;
			Wave		<= Waveform(i).Value;
			exit when simIsStopped(TestID);
		end loop;
		simDeactivateProcess(PROCESS_ID);
	end procedure;

	procedure simGenerateWaveform(
		signal	 Wave					: out	T_SLV_16;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_16;
		constant InitialValue	: in	T_SLV_16				:= (others => '0')
	) is
	begin
		simGenerateWaveform(C_SIM_DEFAULT_TEST_ID, Wave, Waveform, InitialValue);
	end procedure;

	procedure simGenerateWaveform(
		constant TestID				: in	T_SIM_TEST_ID;
		signal	 Wave					: out	T_SLV_16;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_16;
		constant InitialValue	: in	T_SLV_16				:= (others => '0')
	) is
		constant PROCESS_ID	: T_SIM_PROCESS_ID			:= simRegisterProcess(TestID, "simGenerateWaveform");
	begin
		Wave <= InitialValue;
		for i in Waveform'range loop
			wait for Waveform(i).Delay;
			Wave		<= Waveform(i).Value;
			exit when simIsStopped(TestID);
		end loop;
		simDeactivateProcess(PROCESS_ID);
	end procedure;

	procedure simGenerateWaveform(
		signal	 Wave					: out	T_SLV_24;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_24;
		constant InitialValue	: in	T_SLV_24				:= (others => '0')
	) is
	begin
		simGenerateWaveform(C_SIM_DEFAULT_TEST_ID, Wave, Waveform, InitialValue);
	end procedure;

	procedure simGenerateWaveform(
		constant TestID				: in	T_SIM_TEST_ID;
		signal	 Wave					: out	T_SLV_24;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_24;
		constant InitialValue	: in	T_SLV_24				:= (others => '0')
	) is
		constant PROCESS_ID	: T_SIM_PROCESS_ID			:= simRegisterProcess(TestID, "simGenerateWaveform");
	begin
		Wave <= InitialValue;
		for i in Waveform'range loop
			wait for Waveform(i).Delay;
			Wave		<= Waveform(i).Value;
			exit when simIsStopped(TestID);
		end loop;
		simDeactivateProcess(PROCESS_ID);
	end procedure;

	procedure simGenerateWaveform(
		signal	 Wave					: out	T_SLV_32;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_32;
		constant InitialValue	: in	T_SLV_32				:= (others => '0')
	) is
	begin
		simGenerateWaveform(C_SIM_DEFAULT_TEST_ID, Wave, Waveform, InitialValue);
	end procedure;

	procedure simGenerateWaveform(
		constant TestID				: in	T_SIM_TEST_ID;
		signal	 Wave					: out	T_SLV_32;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_32;
		constant InitialValue	: in	T_SLV_32				:= (others => '0')
	) is
		constant PROCESS_ID	: T_SIM_PROCESS_ID			:= simRegisterProcess(TestID, "simGenerateWaveform");
	begin
		Wave <= InitialValue;
		for i in Waveform'range loop
			wait for Waveform(i).Delay;
			Wave		<= Waveform(i).Value;
			exit when simIsStopped(TestID);
		end loop;
		simDeactivateProcess(PROCESS_ID);
	end procedure;

	procedure simGenerateWaveform(
		signal	 Wave					: out	T_SLV_48;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_48;
		constant InitialValue	: in	T_SLV_48				:= (others => '0')
	) is
	begin
		simGenerateWaveform(C_SIM_DEFAULT_TEST_ID, Wave, Waveform, InitialValue);
	end procedure;

	procedure simGenerateWaveform(
		constant TestID				: in	T_SIM_TEST_ID;
		signal	 Wave					: out	T_SLV_48;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_48;
		constant InitialValue	: in	T_SLV_48				:= (others => '0')
	) is
		constant PROCESS_ID	: T_SIM_PROCESS_ID			:= simRegisterProcess(TestID, "simGenerateWaveform");
	begin
		Wave <= InitialValue;
		for i in Waveform'range loop
			wait for Waveform(i).Delay;
			Wave		<= Waveform(i).Value;
			exit when simIsStopped(TestID);
		end loop;
		simDeactivateProcess(PROCESS_ID);
	end procedure;

	procedure simGenerateWaveform(
		signal	 Wave					: out	T_SLV_64;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_64;
		constant InitialValue	: in	T_SLV_64				:= (others => '0')
	) is
	begin
		simGenerateWaveform(C_SIM_DEFAULT_TEST_ID, Wave, Waveform, InitialValue);
	end procedure;

	procedure simGenerateWaveform(
		constant TestID				: in	T_SIM_TEST_ID;
		signal	 Wave					: out	T_SLV_64;
		constant Waveform			: in	T_SIM_WAVEFORM_SLV_64;
		constant InitialValue	: in	T_SLV_64				:= (others => '0')
	) is
		constant PROCESS_ID	: T_SIM_PROCESS_ID			:= simRegisterProcess(TestID, "simGenerateWaveform");
	begin
		Wave <= InitialValue;
		for i in Waveform'range loop
			wait for Waveform(i).Delay;
			Wave		<= Waveform(i).Value;
			exit when simIsStopped(TestID);
		end loop;
		simDeactivateProcess(PROCESS_ID);
	end procedure;

	-- Waveform arithmetic
	function "*" (Wave : T_SIM_WAVEFORM; Times : natural) return T_SIM_WAVEFORM is
		variable Result		: T_SIM_WAVEFORM(0 to Wave'length * Times - 1);
	begin
		for i in 0 to Times - 1 loop
			Result(i * Wave'length to (i + 1) * Wave'length - 1) := Wave;
		end loop;
		return Result;
	end function;

	function ">" (Wave : T_SIM_WAVEFORM; Offset : time) return T_SIM_WAVEFORM is
	begin
		return (Wave(Wave'low) + Offset) & Wave(Wave'low + 1 to Wave'high);
	end function;

	function "<" (Wave : T_SIM_WAVEFORM; Offset : time) return T_SIM_WAVEFORM is
		variable Result		: T_SIM_WAVEFORM(Wave'range);
		variable TimePos	: time;
	begin
		report "Has bugs" severity ERROR;
		TimePos := 0 fs;
		for i in Wave'range loop
			TimePos	:= TimePos + Wave(i);
			if TimePos > Offset then
				return (TimePos - Offset) & Wave(i + 1 to Wave'high);
			end if;
		end loop;
		return (0 => 0 fs);
	end function;

	function "*" (Wave : T_SIM_WAVEFORM_SLV_8; Times : natural) return T_SIM_WAVEFORM_SLV_8 is
		variable Result		: T_SIM_WAVEFORM_SLV_8(0 to Wave'length * Times - 1);
	begin
		for i in 0 to Times - 1 loop
			Result(i * Wave'length to (i + 1) * Wave'length - 1) := Wave;
		end loop;
		return Result;
	end function;

	function ">" (Wave : T_SIM_WAVEFORM_SLV_8; Offset : time) return T_SIM_WAVEFORM_SLV_8 is
	begin
		return T_SIM_WAVEFORM_TUPLE_SLV_8'(
			Delay => Wave(Wave'low).Delay + Offset,
			Value => Wave(Wave'low).Value
		) & Wave(Wave'low + 1 to Wave'high);
	end function;

	-- function "<" (Wave : T_SIM_WAVEFORM_SLV_8; Offset : TIME) return T_SIM_WAVEFORM_SLV_8 is
	-- begin
		-- report "Not implemented" severity FAILURE;
	-- end function;

	function "*" (Wave : T_SIM_WAVEFORM_SLV_16; Times : natural) return T_SIM_WAVEFORM_SLV_16 is
		variable Result		: T_SIM_WAVEFORM_SLV_16(0 to Wave'length * Times - 1);
	begin
		for i in 0 to Times - 1 loop
			Result(i * Wave'length to (i + 1) * Wave'length - 1) := Wave;
		end loop;
		return Result;
	end function;

	function ">" (Wave : T_SIM_WAVEFORM_SLV_16; Offset : time) return T_SIM_WAVEFORM_SLV_16 is
	begin
		return T_SIM_WAVEFORM_TUPLE_SLV_16'(
			Delay => Wave(Wave'low).Delay + Offset,
			Value => Wave(Wave'low).Value
		) & Wave(Wave'low + 1 to Wave'high);
	end function;

	-- function "<" (Wave : T_SIM_WAVEFORM_SLV_16; Offset : TIME) return T_SIM_WAVEFORM_SLV_16 is
	-- begin
		-- report "Not implemented" severity FAILURE;
	-- end function;

	function "*" (Wave : T_SIM_WAVEFORM_SLV_24; Times : natural) return T_SIM_WAVEFORM_SLV_24 is
		variable Result		: T_SIM_WAVEFORM_SLV_24(0 to Wave'length * Times - 1);
	begin
		for i in 0 to Times - 1 loop
			Result(i * Wave'length to (i + 1) * Wave'length - 1) := Wave;
		end loop;
		return Result;
	end function;

	function ">" (Wave : T_SIM_WAVEFORM_SLV_24; Offset : time) return T_SIM_WAVEFORM_SLV_24 is
	begin
		return T_SIM_WAVEFORM_TUPLE_SLV_24'(
			Delay => Wave(Wave'low).Delay + Offset,
			Value => Wave(Wave'low).Value
		) & Wave(Wave'low + 1 to Wave'high);
	end function;

	-- function "<" (Wave : T_SIM_WAVEFORM_SLV_24; Offset : TIME) return T_SIM_WAVEFORM_SLV_24 is
	-- begin
		-- report "Not implemented" severity FAILURE;
	-- end function;

	function "*" (Wave : T_SIM_WAVEFORM_SLV_32; Times : natural) return T_SIM_WAVEFORM_SLV_32 is
		variable Result		: T_SIM_WAVEFORM_SLV_32(0 to Wave'length * Times - 1);
	begin
		for i in 0 to Times - 1 loop
			Result(i * Wave'length to (i + 1) * Wave'length - 1) := Wave;
		end loop;
		return Result;
	end function;

	function ">" (Wave : T_SIM_WAVEFORM_SLV_32; Offset : time) return T_SIM_WAVEFORM_SLV_32 is
	begin
		return T_SIM_WAVEFORM_TUPLE_SLV_32'(
			Delay => Wave(Wave'low).Delay + Offset,
			Value => Wave(Wave'low).Value
		) & Wave(Wave'low + 1 to Wave'high);
	end function;

	-- function "<" (Wave : T_SIM_WAVEFORM_SLV_32; Offset : TIME) return T_SIM_WAVEFORM_SLV_32 is
	-- begin
		-- report "Not implemented" severity FAILURE;
	-- end function;

	function "*" (Wave : T_SIM_WAVEFORM_SLV_48; Times : natural) return T_SIM_WAVEFORM_SLV_48 is
		variable Result		: T_SIM_WAVEFORM_SLV_48(0 to Wave'length * Times - 1);
	begin
		for i in 0 to Times - 1 loop
			Result(i * Wave'length to (i + 1) * Wave'length - 1) := Wave;
		end loop;
		return Result;
	end function;

	function ">" (Wave : T_SIM_WAVEFORM_SLV_48; Offset : time) return T_SIM_WAVEFORM_SLV_48 is
	begin
		return T_SIM_WAVEFORM_TUPLE_SLV_48'(
			Delay => Wave(Wave'low).Delay + Offset,
			Value => Wave(Wave'low).Value
		) & Wave(Wave'low + 1 to Wave'high);
	end function;

	-- function "<" (Wave : T_SIM_WAVEFORM_SLV_48; Offset : TIME) return T_SIM_WAVEFORM_SLV_48 is
	-- begin
		-- report "Not implemented" severity FAILURE;
	-- end function;

	function "*" (Wave : T_SIM_WAVEFORM_SLV_64; Times : natural) return T_SIM_WAVEFORM_SLV_64 is
		variable Result		: T_SIM_WAVEFORM_SLV_64(0 to Wave'length * Times - 1);
	begin
		for i in 0 to Times - 1 loop
			Result(i * Wave'length to (i + 1) * Wave'length - 1) := Wave;
		end loop;
		return Result;
	end function;

	function ">" (Wave : T_SIM_WAVEFORM_SLV_64; Offset : time) return T_SIM_WAVEFORM_SLV_64 is
	begin
		return T_SIM_WAVEFORM_TUPLE_SLV_64'(
			Delay => Wave(Wave'low).Delay + Offset,
			Value => Wave(Wave'low).Value
		) & Wave(Wave'low + 1 to Wave'high);
	end function;

	-- function "<" (Wave : T_SIM_WAVEFORM_SLV_64; Offset : TIME) return T_SIM_WAVEFORM_SLV_64 is
	-- begin
		-- report "Not implemented" severity FAILURE;
	-- end function;


	function to_waveform(bv : bit_vector; Delay : time) return T_SIM_WAVEFORM is
		variable Result		: T_SIM_WAVEFORM(0 to bv'length - 1);
	begin
		report "Has bugs" severity ERROR;
		for i in 0 to bv'length - 1 loop
			Result(i)		:= Delay;
		end loop;
		return Result;
	end function;

	function to_waveform(slv : std_logic_vector; Delay : time) return T_SIM_WAVEFORM_SL is
		variable Result		: T_SIM_WAVEFORM_SL(0 to slv'length - 1);
	begin
		for i in 0 to slv'length - 1 loop
			Result(i).Delay		:= Delay;
			Result(i).Value		:= slv(i);
		end loop;
		return Result;
	end function;

	function to_waveform(slvv : T_SLVV_8; Delay : time) return T_SIM_WAVEFORM_SLV_8 is
		variable Result		: T_SIM_WAVEFORM_SLV_8(0 to slvv'length - 1);
	begin
		for i in 0 to slvv'length - 1 loop
			Result(i).Delay		:= Delay;
			Result(i).Value		:= slvv(i);
		end loop;
		return Result;
	end function;

	function to_waveform(slvv : T_SLVV_16; Delay : time) return T_SIM_WAVEFORM_SLV_16 is
		variable Result		: T_SIM_WAVEFORM_SLV_16(0 to slvv'length - 1);
	begin
		for i in 0 to slvv'length - 1 loop
			Result(i).Delay		:= Delay;
			Result(i).Value		:= slvv(i);
		end loop;
		return Result;
	end function;

	function to_waveform(slvv : T_SLVV_24; Delay : time) return T_SIM_WAVEFORM_SLV_24 is
		variable Result		: T_SIM_WAVEFORM_SLV_24(0 to slvv'length - 1);
	begin
		for i in 0 to slvv'length - 1 loop
			Result(i).Delay		:= Delay;
			Result(i).Value		:= slvv(i);
		end loop;
		return Result;
	end function;

	function to_waveform(slvv : T_SLVV_32; Delay : time) return T_SIM_WAVEFORM_SLV_32 is
		variable Result		: T_SIM_WAVEFORM_SLV_32(0 to slvv'length - 1);
	begin
		for i in 0 to slvv'length - 1 loop
			Result(i).Delay		:= Delay;
			Result(i).Value		:= slvv(i);
		end loop;
		return Result;
	end function;

	function to_waveform(slvv : T_SLVV_48; Delay : time) return T_SIM_WAVEFORM_SLV_48 is
		variable Result		: T_SIM_WAVEFORM_SLV_48(0 to slvv'length - 1);
	begin
		for i in 0 to slvv'length - 1 loop
			Result(i).Delay		:= Delay;
			Result(i).Value		:= slvv(i);
		end loop;
		return Result;
	end function;

	function to_waveform(slvv : T_SLVV_64; Delay : time) return T_SIM_WAVEFORM_SLV_64 is
		variable Result		: T_SIM_WAVEFORM_SLV_64(0 to slvv'length - 1);
	begin
		for i in 0 to slvv'length - 1 loop
			Result(i).Delay		:= Delay;
			Result(i).Value		:= slvv(i);
		end loop;
		return Result;
	end function;

	-- predefined common waveforms
	function simGenerateWaveform_Reset(constant Pause : time := 0 ns; ResetPulse : time := 10 ns) return T_SIM_WAVEFORM is
		variable p  : time;
		variable rp : time;
	begin
		-- WORKAROUND: for Mentor QuestaSim/ModelSim
		--	Version:	10.4c
		--	Issue:
		--		return (0 => Pause, 1 => ResetPulse); always evaluates to (0 ns, 10 ns),
		--		regardless of the passed function parameters
		--	Bugfix:
		--		The bugfix will be included in 10.5a, but this workaround must be
		--		present until Altera updates the embedded ModelSim Altera Edition.
		p  := Pause;
		rp := ResetPulse;
		return (0 => p, 1 => rp);
	end function;
end package body;
