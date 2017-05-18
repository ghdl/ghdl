-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- =============================================================================
-- Authors:					Patrick Lehmann
--									Thomas B. Preusser
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

use			PoC.sim_global.all;
use			PoC.sim_types.all;
use			PoC.sim_protected.all;


package simulation is
	-- Legacy interface for pre VHDL-2002
	-- ===========================================================================
	-- prepared aliases, if GHDL gets the aliases fixed. Reported on 08.02.2015 as Issue #38
	-- alias simmInitialize					is globalSimulationStatus.initialize[NATURAL, TIME];
	-- alias simmFinalize						is globalSimulationStatus.finalize[];

	-- alias simmCreateTest					is globalSimulationStatus.createTest[STRING return T_SIM_TEST_ID];
	-- alias simmFinalizeTest				is globalSimulationStatus.finalizeTest[T_SIM_TEST_ID];
	-- alias simmRegisterProcess		is globalSimulationStatus.registerProcess[T_SIM_TEST_ID, STRING, BOOLEAN return T_SIM_PROCESS_ID];
	-- alias simmRegisterProcess		is globalSimulationStatus.registerProcess[STRING, BOOLEAN return T_SIM_PROCESS_ID];
	-- alias simmDeactivateProcess	is globalSimulationStatus.deactivateProcess[T_SIM_PROCESS_ID];

	-- alias simmIsStopped					is globalSimulationStatus.isStopped[T_SIM_TEST_ID return BOOLEAN];
	-- alias simmIsFinalized				is globalSimulationStatus.isFinalized[T_SIM_TEST_ID return BOOLEAN];
	-- alias simmIsAllFinalized			is globalSimulationStatus.isAllFinalized [return BOOLEAN];

	-- alias simmAssertion					is globalSimulationStatus.assertion[BOOLEAN, STRING];
  -- alias simmFail								is globalSimulationStatus.fail[STRING];
	-- alias simmWriteMessage				is globalSimulationStatus.writeMessage[STRING];

	procedure				simInitialize(MaxAssertFailures : natural := natural'high; MaxSimulationRuntime : TIME := TIME'high);
	procedure				simFinalize;

	impure function	simCreateTest(Name : string) return T_SIM_TEST_ID;
	procedure				simFinalizeTest(constant TestID : T_SIM_TEST_ID);
	impure function	simRegisterProcess(Name : string; constant IsLowPriority : boolean := FALSE) return T_SIM_PROCESS_ID;
	impure function	simRegisterProcess(constant TestID : T_SIM_TEST_ID; Name : string; constant IsLowPriority : boolean := FALSE) return T_SIM_PROCESS_ID;
	procedure				simDeactivateProcess(ProcID : T_SIM_PROCESS_ID);

	impure function	simIsStopped(constant TestID		: T_SIM_TEST_ID := C_SIM_DEFAULT_TEST_ID) return boolean;
	impure function simIsFinalized(constant TestID	: T_SIM_TEST_ID := C_SIM_DEFAULT_TEST_ID) return boolean;
	impure function	simIsAllFinalized return boolean;

	procedure				simAssertion(cond : in boolean; Message : in string := "");
  procedure				simFail(Message : in string := "");
	procedure				simWriteMessage(Message : in string := "");

	-- TODO: integrate VCD simulation functions and procedures from sim_value_change_dump.vhdl here

	-- checksum functions
	-- ===========================================================================
	-- TODO: move checksum functions here
end package;


package body simulation is
	-- legacy procedures
	-- ===========================================================================
	-- TODO: undocumented group
	procedure simInitialize(MaxAssertFailures : natural := natural'high; MaxSimulationRuntime : TIME := TIME'high) is
	begin
		globalSimulationStatus.initialize(MaxAssertFailures, MaxSimulationRuntime);
		if C_SIM_VERBOSE then		report "simInitialize:" severity NOTE;			end if;
		if (MaxSimulationRuntime /= time'high) then
			wait for MaxSimulationRuntime;
			report "simInitialize: TIMEOUT" severity ERROR;
			globalSimulationStatus.finalize;
		end if;
	end procedure;

	procedure simFinalize is
	begin
		globalSimulationStatus.finalize;
	end procedure;

	impure function simCreateTest(Name : string) return T_SIM_TEST_ID is
	begin
		return globalSimulationStatus.createTest(Name);
	end function;

	procedure simFinalizeTest(constant TestID : T_SIM_TEST_ID) is
	begin
		globalSimulationStatus.finalizeTest(TestID);
	end procedure;

	impure function simRegisterProcess(Name : string; constant IsLowPriority : boolean := FALSE) return T_SIM_PROCESS_ID is
	begin
		return globalSimulationStatus.registerProcess(Name, IsLowPriority);
	end function;

	impure function simRegisterProcess(constant TestID : T_SIM_TEST_ID; Name : string; constant IsLowPriority : boolean := FALSE) return T_SIM_PROCESS_ID is
	begin
		return globalSimulationStatus.registerProcess(TestID, Name, IsLowPriority);
	end function;

	procedure simDeactivateProcess(ProcID : T_SIM_PROCESS_ID) is
	begin
		globalSimulationStatus.deactivateProcess(ProcID);
	end procedure;

	impure function simIsStopped(constant TestID : T_SIM_TEST_ID := C_SIM_DEFAULT_TEST_ID) return boolean is
	begin
		return globalSimulationStatus.isStopped(TestID);
	end function;

	impure function simIsFinalized(constant TestID : T_SIM_TEST_ID := C_SIM_DEFAULT_TEST_ID) return boolean is
	begin
		return globalSimulationStatus.isFinalized(TestID);
	end function;

	impure function simIsAllFinalized return boolean is
	begin
		return globalSimulationStatus.isAllFinalized;
	end function;

	-- TODO: undocumented group
	procedure simWriteMessage(Message : in string := "") is
	begin
		globalSimulationStatus.writeMessage(Message);
	end procedure;

  procedure simFail(Message : in string := "") is
  begin
		globalSimulationStatus.fail(Message);
  end procedure;

  procedure simAssertion(cond : in boolean; Message : in string := "") is
	begin
		globalSimulationStatus.assertion(cond, Message);
	end procedure;

	-- checksum functions
	-- ===========================================================================
	-- TODO: move checksum functions here
end package body;
