-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- =============================================================================
-- Authors:					Patrick Lehmann
--									Thomas B. Preusser
-- 
-- Package:					Simulation constants, functions and utilities.
-- 
-- Description:
-- ------------------------------------
--		TODO
--
-- License:
-- =============================================================================
-- Copyright 2007-2016 Technische Universitaet Dresden - Germany
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

use			STD.TextIO.all;

library IEEE;
use			IEEE.STD_LOGIC_1164.all;

library PoC;
-- use			PoC.utils.all;
-- use			PoC.strings.all;
use			PoC.vectors.all;
-- use			PoC.physical.all;


package sim_types is
  -- Simulation Task and Status Management
	-- ===========================================================================
	subtype T_SIM_PROCESS_ID				is NATURAL range 0 to 1023;
	subtype T_SIM_PROCESS_NAME			is STRING(1 to 64);
	subtype T_SIM_PROCESS_INSTNAME	is STRING(1 to 256);
	
	type T_SIM_PROCESS_STATUS is (
		SIM_PROCESS_STATUS_ACTIVE,
		SIM_PROCESS_STATUS_ENDED
	);
	
	type T_SIM_PROCESS is record
		ID						: T_SIM_PROCESS_ID;
		Name					: T_SIM_PROCESS_NAME;
		InstanceName	: T_SIM_PROCESS_INSTNAME;
		Status				: T_SIM_PROCESS_STATUS;
	end record;
	type T_SIM_PROCESS_VECTOR is array(NATURAL range <>) of T_SIM_PROCESS;
	
	subtype T_SIM_TEST_ID		is NATURAL range 0 to 1023;
	subtype T_SIM_TEST_NAME	is STRING(1 to 256);
	
	type T_SIM_TEST_STATUS is (
		SIM_TEST_STATUS_ACTIVE,
		SIM_TEST_STATUS_ENDED
	);
	
	type T_SIM_TEST is record
		ID			: T_SIM_TEST_ID;
		Name		: T_SIM_TEST_NAME;
		Status	: T_SIM_TEST_STATUS;
	end record;
	type T_SIM_TEST_VECTOR	is array(NATURAL range <>) of T_SIM_TEST;
	
	-- clock generation
	-- ===========================================================================
	-- type T_PERCENT is INTEGER'range units
	type T_PERCENT is range INTEGER'low to INTEGER'high units
		ppb;
		ppm			= 1000 ppb;
		permil	= 1000 ppm;
		percent	= 10 permil;
		one			= 100 percent;
	end units;
	subtype T_WANDER		is T_PERCENT range -1 one to 1 one;
	subtype T_DUTYCYCLE	is T_PERCENT range	0 ppb to 1 one;
	
	type T_DEGREE is range INTEGER'low to INTEGER'high units
		second;
		minute	= 60 second;
		deg			= 60 minute;
	end units;
	subtype T_PHASE is T_DEGREE range	-360 deg to 360 deg;
	
	function ite(cond : BOOLEAN; value1 : T_DEGREE; value2 : T_DEGREE) return T_DEGREE;
	
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
	
end package;


package body sim_types is
	function ite(cond : BOOLEAN; value1 : T_DEGREE; value2 : T_DEGREE) return T_DEGREE is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;
end package body;
