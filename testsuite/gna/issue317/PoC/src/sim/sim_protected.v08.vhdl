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

use			STD.TextIO.all;

library IEEE;
use			IEEE.STD_LOGIC_1164.all;

library PoC;
use			PoC.utils.all;
use			PoC.strings.all;
use			PoC.vectors.all;
use			PoC.physical.all;

use			PoC.sim_types.all;


package sim_protected is
  -- Simulation Task and Status Management
	-- ===========================================================================
	type T_SIM_STATUS is protected
		-- Initializer and Finalizer
		procedure				initialize(MaxAssertFailures : natural := natural'high; MaxSimulationRuntime : TIME := TIME'high);
		procedure				finalize;

		-- Assertions
    procedure				fail(Message : string := "");
	  procedure				assertion(Condition : boolean; Message : string := "");
	  procedure				writeMessage(Message : string);
		procedure				writeReport;

		-- Process Management
		impure function	registerProcess(Name : string; IsLowPriority : boolean := FALSE) return T_SIM_PROCESS_ID;
		impure function registerProcess(TestID : T_SIM_TEST_ID; Name : string; IsLowPriority : boolean := FALSE) return T_SIM_PROCESS_ID;
		procedure				deactivateProcess(procID : T_SIM_PROCESS_ID; SkipLowPriority : boolean := FALSE);
		procedure				stopAllProcesses;
		procedure				stopProcesses(TestID	: T_SIM_TEST_ID := C_SIM_DEFAULT_TEST_ID);

		-- Test Management
		procedure				createDefaultTest;
		impure function	createTest(Name : string) return T_SIM_TEST_ID;
		procedure				activateDefaultTest;
		procedure				finalizeTest;
		procedure				finalizeTest(TestID : T_SIM_TEST_ID);

		-- Run Management
		procedure				stopAllClocks;
		procedure				stopClocks(TestID		: T_SIM_TEST_ID := C_SIM_DEFAULT_TEST_ID);

		impure function	isStopped(TestID		: T_SIM_TEST_ID := C_SIM_DEFAULT_TEST_ID) return boolean;
		impure function	isFinalized(TestID	: T_SIM_TEST_ID := C_SIM_DEFAULT_TEST_ID) return boolean;
		impure function isAllFinalized return boolean;
	end protected;
end package;


package body sim_protected is
	-- Simulation process and Status Management
	-- ===========================================================================
	type T_SIM_STATUS_STATE is record
		IsInitialized			: boolean;
		IsFinalized				: boolean;
	end record;

	type T_SIM_STATUS is protected body
		-- status
		variable State									: T_SIM_STATUS_STATE											:= (FALSE, FALSE);

		variable Max_AssertFailures			: natural																	:= natural'high;
		variable Max_SimulationRuntime	: time																		:= time'high;

    -- Internal state variable to log a failure condition for final reporting.
    -- Once de-asserted, this variable will never return to a value of true.
		variable Passed									: boolean 																:= TRUE;
		variable AssertCount						: natural																	:= 0;
		variable FailedAssertCount			: natural																	:= 0;

		-- Clock Management
		variable MainProcessEnables			: T_SIM_BOOLVEC(T_SIM_TEST_ID)						:= (others => TRUE);
		variable MainClockEnables				: T_SIM_BOOLVEC(T_SIM_TEST_ID)						:= (others => TRUE);

		-- Process Management
		variable ProcessCount						: natural																	:= 0;
		variable ActiveProcessCount			: natural																	:= 0;
		variable Processes							: T_SIM_PROCESS_VECTOR(T_SIM_PROCESS_ID);

		-- Test Management
		variable TestCount							: natural																	:= 0;
		variable ActiveTestCount				: natural																	:= 0;
		variable Tests									: T_SIM_TEST_VECTOR(T_SIM_TEST_ID);

		-- Initializer
		procedure init is
		begin
			if (State.IsInitialized = FALSE) then
				if C_SIM_VERBOSE then		report "init:" severity NOTE;			end if;
				State.IsInitialized		:= TRUE;
				createDefaultTest;
			end if;
		end procedure;

		procedure initialize(MaxAssertFailures : natural := natural'high; MaxSimulationRuntime : TIME := TIME'high) is
		begin
			if C_SIM_VERBOSE then		report "initialize:" severity NOTE;			end if;
			init;
			Max_AssertFailures		:= MaxAssertFailures;
			Max_SimulationRuntime	:= MaxSimulationRuntime;
		end procedure;

		procedure finalize is
		begin
			if (State.IsFinalized = FALSE) then
				if C_SIM_VERBOSE then		report "finalize: " severity NOTE;		end if;
				State.IsFinalized		:= TRUE;
				for i in C_SIM_DEFAULT_TEST_ID to TestCount - 1 loop
					finalizeTest(i);
				end loop;
				writeReport;
			end if;
		end procedure;

		procedure writeReport_Header is
		  variable LineBuffer : LINE;
		begin
		  write(LineBuffer,		(			string'("========================================")));
		  write(LineBuffer,		(LF & string'("POC TESTBENCH REPORT")));
		  write(LineBuffer,		(LF & string'("========================================")));
		  writeline(output, LineBuffer);
		end procedure;

		procedure writeReport_TestReport(Prefix : string := "") is
		  variable LineBuffer : LINE;
	  begin
			if (Tests(C_SIM_DEFAULT_TEST_ID).Status /= SIM_TEST_STATUS_CREATED) then
				write(LineBuffer,				 Prefix & "Tests          " & integer'image(TestCount + 1));
				write(LineBuffer,		LF & Prefix & " " & str_ralign("-1", log10ceilnz(TestCount + 1) + 1) & ": " & C_SIM_DEFAULT_TEST_NAME);
			else
				write(LineBuffer,				 Prefix & "Tests          " & integer'image(TestCount));
			end if;
			for i in 0 to TestCount - 1 loop
				write(LineBuffer,		LF & Prefix & "  " & str_ralign(integer'image(i), log10ceilnz(TestCount)) & ": " & str_trim(Tests(i).Name));
			end loop;
		  writeline(output, LineBuffer);
		end procedure;

		procedure writeReport_AssertReport(Prefix : string := "") is
		  variable LineBuffer : LINE;
	  begin
			write(LineBuffer,					 Prefix & "Assertions   " & integer'image(AssertCount));
			write(LineBuffer,			LF & Prefix & "  failed     " & integer'image(FailedAssertCount) & ite((FailedAssertCount >= Max_AssertFailures), " Too many failed asserts!", ""));
		  writeline(output, LineBuffer);
		end procedure;

		procedure writeReport_ProcessReport(Prefix : string := "") is
		  variable LineBuffer : LINE;
	  begin
			write(LineBuffer,					 Prefix & "Processes    " & integer'image(ProcessCount));
			write(LineBuffer,			LF & Prefix & "  active     " & integer'image(ActiveProcessCount));
			-- report killed processes
			for i in 0 to ProcessCount - 1 loop
				if ((Processes(i).Status = SIM_PROCESS_STATUS_ACTIVE) and (Processes(i).IsLowPriority = FALSE)) then
					write(LineBuffer,	LF & Prefix & "    " & str_ralign(integer'image(i), log10ceilnz(ProcessCount)) & ": " & str_trim(Processes(i).Name));
				end if;
			end loop;
		  writeline(output, LineBuffer);
		end procedure;

		procedure writeReport_RuntimeReport(Prefix : string := "") is
		  variable LineBuffer : LINE;
	  begin
			write(LineBuffer,					 Prefix & "Runtime      " & to_string(now, 1));
		  writeline(output, LineBuffer);
		end procedure;

		procedure writeReport_SimulationResult is
		  variable LineBuffer : LINE;
	  begin
		  write(LineBuffer,																(			string'("========================================")));
		  if not Passed then		write(LineBuffer, (LF & string'("SIMULATION RESULT = FAILED")));
			elsif AssertCount = 0 then	write(LineBuffer, (LF & string'("SIMULATION RESULT = NO ASSERTS")));
		  elsif Passed then		write(LineBuffer, (LF & string'("SIMULATION RESULT = PASSED")));
		  end if;
		  write(LineBuffer,																(LF & string'("========================================")));
		  writeline(output, LineBuffer);
		end procedure;

	  procedure writeReport is
		  variable LineBuffer : LINE;
	  begin
			writeReport_Header;
			writeReport_TestReport("");
			write(LineBuffer, LF & "Overall");
		  writeline(output, LineBuffer);
			writeReport_AssertReport("  ");
			writeReport_ProcessReport("  ");
			writeReport_RuntimeReport("  ");
			writeReport_SimulationResult;
		end procedure;

	  procedure assertion(condition : boolean; Message : string := "") is
  	begin
			AssertCount := AssertCount + 1;
		  if not condition then
		    fail(Message);
				FailedAssertCount := FailedAssertCount + 1;
				if (FailedAssertCount >= Max_AssertFailures) then
					stopAllProcesses;
				end if;
		  end if;
	  end procedure;

	  procedure fail(Message : string := "") is
		begin
	  	if (Message'length > 0) then
		  	report Message severity ERROR;
		  end if;
		  Passed := FALSE;
		end procedure;

		procedure writeMessage(Message : string) is
		  variable LineBuffer : LINE;
	  begin
		  write(LineBuffer, Message);
		  writeline(output, LineBuffer);
		end procedure;

		procedure createDefaultTest is
			variable Test							: T_SIM_TEST;
		begin
			if (State.IsInitialized = FALSE) then
				init;
			end if;
			if C_SIM_VERBOSE then		report "createDefaultTest(" & C_SIM_DEFAULT_TEST_NAME & "):" severity NOTE;		end if;
			Test.ID										:= C_SIM_DEFAULT_TEST_ID;
			Test.Name									:= resize(C_SIM_DEFAULT_TEST_NAME, T_SIM_TEST_NAME'length);
			Test.Status								:= SIM_TEST_STATUS_CREATED;
			Test.ProcessIDs						:= (others => 0);
			Test.ProcessCount					:= 0;
			Test.ActiveProcessCount		:= 0;
			-- add to the internal structure
			Tests(Test.ID)						:= Test;
		end procedure;

		impure function createTest(Name : string) return T_SIM_TEST_ID is
			variable Test						: T_SIM_TEST;
		begin
			if (State.IsInitialized = FALSE) then
				init;
			end if;
			if C_SIM_VERBOSE then		report "createTest(" & Name & "): => " & T_SIM_TEST_ID'image(TestCount) severity NOTE;		end if;
			Test.ID									:= TestCount;
			Test.Name								:= resize(Name, T_SIM_TEST_NAME'length);
			Test.Status							:= SIM_TEST_STATUS_ACTIVE;
			Test.ProcessIDs					:= (others => 0);
			Test.ProcessCount				:= 0;
			Test.ActiveProcessCount	:= 0;
			-- add to the internal structure
			Tests(Test.ID)					:= Test;
			TestCount								:= TestCount + 1;
			ActiveTestCount					:= ActiveTestCount + 1;
			-- return TestID for finalizeTest
			return Test.ID;
		end function;

		procedure activateDefaultTest is
		begin
			if (Tests(C_SIM_DEFAULT_TEST_ID).Status = SIM_TEST_STATUS_CREATED) then
				Tests(C_SIM_DEFAULT_TEST_ID).Status := SIM_TEST_STATUS_ACTIVE;
				ActiveTestCount											:= ActiveTestCount + 1;
			end if;
		end procedure;

		procedure finalizeTest is
		begin
			finalizeTest(C_SIM_DEFAULT_TEST_ID);
		end procedure;

		procedure finalizeTest(TestID : T_SIM_TEST_ID) is
		begin
			if (TestID >= TestCount) then
				report "TestID (" & T_SIM_TEST_ID'image(TestID) & ") is unknown." severity FAILURE;
				return;
			end if;

			if TestID = C_SIM_DEFAULT_TEST_ID then
				if (Tests(C_SIM_DEFAULT_TEST_ID).Status = SIM_TEST_STATUS_CREATED) then
					if C_SIM_VERBOSE then		report "finalizeTest(" & integer'image(C_SIM_DEFAULT_TEST_ID) & "): inactive" severity NOTE;	end if;
					Tests(C_SIM_DEFAULT_TEST_ID).Status	:= SIM_TEST_STATUS_ENDED;
					stopProcesses(C_SIM_DEFAULT_TEST_ID);
					return;
				elsif (Tests(C_SIM_DEFAULT_TEST_ID).Status = SIM_TEST_STATUS_ACTIVE) then
					if ActiveTestCount > 1 then
						for ProcIdx in 0 to Tests(C_SIM_DEFAULT_TEST_ID).ProcessCount - 1 loop
							deactivateProcess(Tests(C_SIM_DEFAULT_TEST_ID).ProcessIDs(ProcIdx), TRUE);
						end loop;
						Tests(C_SIM_DEFAULT_TEST_ID).Status := SIM_TEST_STATUS_ZOMBI;
						return;
					else
						if C_SIM_VERBOSE then		report "finalizeTest(" & integer'image(C_SIM_DEFAULT_TEST_ID) & "): active" severity NOTE;		end if;
						Tests(C_SIM_DEFAULT_TEST_ID).Status	:= SIM_TEST_STATUS_ENDED;
						ActiveTestCount											:= ActiveTestCount - 1;
						stopProcesses(C_SIM_DEFAULT_TEST_ID);
					end if;
				end if;
			elsif (Tests(TestID).Status /= SIM_TEST_STATUS_ENDED) then
				if C_SIM_VERBOSE then		report "finalizeTest(TestID=" & T_SIM_TEST_ID'image(TestID) & "): " severity NOTE;		end if;
				Tests(TestID).Status	:= SIM_TEST_STATUS_ENDED;
				ActiveTestCount				:= ActiveTestCount - 1;

				if (Tests(TestID).ActiveProcessCount > 0) then
					fail("Test " & integer'image(TestID) & " '" & str_trim(Tests(TestID).Name) & "' has still active process while finalizing:");
					for ProcIdx in 0 to Tests(TestID).ProcessCount - 1 loop
						if (Processes(Tests(TestID).ProcessIDs(ProcIdx)).Status = SIM_PROCESS_STATUS_ACTIVE) then
							report "  " & Processes(Tests(TestID).ProcessIDs(ProcIdx)).Name severity WARNING;
						end if;
					end loop;
				end if;
				stopProcesses(TestID);
			end if;

			if ActiveTestCount = 0 then
				finalize;
			elsif ActiveTestCount = 1 then
				if (Tests(C_SIM_DEFAULT_TEST_ID).Status = SIM_TEST_STATUS_ACTIVE) then
					finalizeTest(C_SIM_DEFAULT_TEST_ID);
				elsif (Tests(C_SIM_DEFAULT_TEST_ID).Status = SIM_TEST_STATUS_ZOMBI) then
					stopProcesses(C_SIM_DEFAULT_TEST_ID);
				else
					return;
				end if;
				finalize;
			end if;
		end procedure;

		impure function registerProcess(Name : string; IsLowPriority : boolean := FALSE) return T_SIM_PROCESS_ID is
		begin
			return registerProcess(C_SIM_DEFAULT_TEST_ID, Name, IsLowPriority);
		end function;

		impure function registerProcess(TestID : T_SIM_TEST_ID; Name : string; IsLowPriority : boolean := FALSE) return T_SIM_PROCESS_ID is
			variable Proc						: T_SIM_PROCESS;
			variable TestProcID			: T_SIM_TEST_ID;
		begin
			if (State.IsInitialized = FALSE) then
				init;
			end if;
			if TestID = C_SIM_DEFAULT_TEST_ID then
				activateDefaultTest;
			end if;

			if (TestID >= TestCount) then
				report "TestID (" & T_SIM_TEST_ID'image(TestID) & ") is unknown." severity FAILURE;
				return T_SIM_PROCESS_ID'high;
			end if;

			if C_SIM_VERBOSE then		report "registerProcess(TestID=" & T_SIM_TEST_ID'image(TestID) & ", " & Name & "): => " & T_SIM_PROCESS_ID'image(ProcessCount) severity NOTE;		end if;
			Proc.ID									:= ProcessCount;
			Proc.TestID							:= TestID;
			Proc.Name								:= resize(Name, T_SIM_PROCESS_NAME'length);
			Proc.Status							:= SIM_PROCESS_STATUS_ACTIVE;
			Proc.IsLowPriority			:= IsLowPriority;

			-- add process to list
			Processes(Proc.ID)										:= Proc;
			ProcessCount													:= ProcessCount + 1;
			ActiveProcessCount										:= inc_if(not IsLowPriority, ActiveProcessCount);
			-- add process to test
			TestProcID														:= Tests(TestID).ProcessCount;
			Tests(TestID).ProcessIDs(TestProcID)	:= Proc.ID;
			Tests(TestID).ProcessCount						:= TestProcID + 1;
			Tests(TestID).ActiveProcessCount			:= inc_if(not IsLowPriority, Tests(TestID).ActiveProcessCount);
			-- return the process ID
			return Proc.ID;
		end function;

		procedure deactivateProcess(ProcID : T_SIM_PROCESS_ID; SkipLowPriority : boolean := FALSE) is
			variable TestID		: T_SIM_TEST_ID;
		begin
			if (ProcID >= ProcessCount) then
				report "ProcID (" & T_SIM_PROCESS_ID'image(ProcID) & ") is unknown." severity FAILURE;
				return;
			elsif (Processes(ProcID).IsLowPriority and SkipLowPriority) then
				return;
			end if;

			TestID	:= Processes(ProcID).TestID;
			-- deactivate process
			if (Processes(ProcID).Status = SIM_PROCESS_STATUS_ACTIVE) then
				if C_SIM_VERBOSE then		report "deactivateProcess(ProcID=" & T_SIM_PROCESS_ID'image(ProcID) & "): TestID=" & T_SIM_TEST_ID'image(TestID) & "  Name=" & str_trim(Processes(ProcID).Name) severity NOTE;		end if;
				Processes(ProcID).Status					:= SIM_PROCESS_STATUS_ENDED;
				ActiveProcessCount								:= dec_if(not Processes(ProcID).IsLowPriority, ActiveProcessCount);
				Tests(TestID).ActiveProcessCount	:= dec_if(not Processes(ProcID).IsLowPriority, Tests(TestID).ActiveProcessCount);
				if (Tests(TestID).ActiveProcessCount = 0) then
					finalizeTest(TestID);
				end if;
			end if;
		end procedure;

		procedure stopAllProcesses is
		begin
			if C_SIM_VERBOSE then		report "stopAllProcesses:" severity NOTE;		end if;
			for i in C_SIM_DEFAULT_TEST_ID to TestCount - 1 loop
				stopProcesses(i);
			end loop;
		end procedure;

		procedure stopProcesses(TestID : T_SIM_TEST_ID := C_SIM_DEFAULT_TEST_ID) is
		begin
			if (TestID >= TestCount) then
				report "TestID (" & T_SIM_TEST_ID'image(TestID) & ") is unknown." severity FAILURE;
				return;
			end if;

			if C_SIM_VERBOSE then		report "stopProcesses(TestID=" & T_SIM_TEST_ID'image(TestID) & "): Name=" & str_trim(Tests(TestID).Name) severity NOTE;		end if;
			MainProcessEnables(TestID)	:= FALSE;
			stopClocks(TestID);
		end procedure;

		procedure stopAllClocks is
		begin
			if C_SIM_VERBOSE then		report "stopAllClocks:" severity NOTE;		end if;
			for i in C_SIM_DEFAULT_TEST_ID to TestCount - 1 loop
				stopClocks(i);
			end loop;
		end procedure;

		procedure stopClocks(TestID : T_SIM_TEST_ID := C_SIM_DEFAULT_TEST_ID) is
		begin
			if (TestID >= TestCount) then
				report "TestID (" & T_SIM_TEST_ID'image(TestID) & ") is unknown." severity FAILURE;
				return;
			end if;

			if C_SIM_VERBOSE then		report "stopClocks(TestID=" & T_SIM_TEST_ID'image(TestID) & "): Name=" & str_trim(Tests(TestID).Name) severity NOTE;		end if;
			MainClockEnables(TestID)		:= FALSE;
		end procedure;

		impure function isStopped(TestID : T_SIM_TEST_ID := C_SIM_DEFAULT_TEST_ID) return boolean is
		begin
			return not MainClockEnables(TestID);
		end function;

		impure function isFinalized(TestID : T_SIM_TEST_ID := C_SIM_DEFAULT_TEST_ID) return boolean is
		begin
			return (Tests(TestID).Status = SIM_TEST_STATUS_ENDED);
		end function;

		impure function isAllFinalized return boolean is
		begin
			if (State.IsFinalized = TRUE) then
				if ActiveTestCount = 0 then
					return TRUE;
				end if;
				report "isAllFinalized: " severity ERROR;
				return FALSE;
			else
				return FALSE;
			end if;
		end function;
	end protected body;
end package body;
