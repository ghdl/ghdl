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
		procedure initialize;
		procedure finalize;
		
		-- Assertions
    procedure fail(Message : STRING := "");
	  procedure assertion(Condition : BOOLEAN; Message : STRING := "");
	  procedure writeMessage(Message : STRING);
		procedure writeReport;
		
		-- Process Management
		-- impure function	registerProcess(Name : STRING; InstanceName : STRING) return T_SIM_PROCESS_ID;
		impure function	registerProcess(Name : STRING) return T_SIM_PROCESS_ID;
		procedure				deactivateProcess(procID : T_SIM_PROCESS_ID);
		
		-- Test Management
		impure function createTest(Name : STRING) return T_SIM_TEST_ID;
		
		-- Run Management
		procedure				stopAllClocks;
		impure function	isStopped return BOOLEAN;
	end protected;
end package;


package body sim_protected is
	-- Simulation process and Status Management
	-- ===========================================================================
	type T_SIM_STATUS is protected body
		-- status
		variable IsInitialized			: BOOLEAN		:= FALSE;
		variable IsFinalized				: BOOLEAN		:= FALSE;
		
    -- Internal state variable to log a failure condition for final reporting.
    -- Once de-asserted, this variable will never return to a value of true.
		variable Passed							: BOOLEAN := TRUE;
		variable AssertCount				: NATURAL		:= 0;
		variable FailedAssertCount	: NATURAL		:= 0;
		
		-- Clock Management
		variable MainClockEnable		: BOOLEAN		:= TRUE;
		
		-- Process Management
		variable ProcessCount				: NATURAL																	:= 0;
		variable ActiveProcessCount	: NATURAL																	:= 0;
		variable Processes					: T_SIM_PROCESS_VECTOR(T_SIM_PROCESS_ID);
		
		-- Test Management
		variable TestCount					: NATURAL																	:= 0;
		variable Tests							: T_SIM_TEST_VECTOR(T_SIM_TEST_ID);
		
		-- Initializer
		procedure initialize is
		begin
			IsInitialized			:= TRUE;
		end procedure;
		
		procedure finalize is
		begin
			if (IsFinalized = FALSE) then
				if (ActiveProcessCount = 0) then
					writeReport;
					IsFinalized		:= TRUE;
				end if;
			end if;
		end procedure;
		
	  procedure fail(Message : STRING := "") is
		begin
	  	if (Message'length > 0) then
		  	report Message severity ERROR;
		  end if;
		  Passed := FALSE;
		end procedure;

	  procedure assertion(condition : BOOLEAN; Message : STRING := "") is
  	begin
			AssertCount := AssertCount + 1;
		  if (condition = FALSE) then
		    fail(Message);
				FailedAssertCount := FailedAssertCount + 1;
		  end if;
	  end procedure;

		procedure writeMessage(Message : STRING) is
		  variable LineBuffer : LINE;
	  begin
		  write(LineBuffer, Message);
		  writeline(output, LineBuffer);
		end procedure;
		
	  procedure writeReport is
		  variable LineBuffer : LINE;
	  begin
		  write(LineBuffer,		(CR & STRING'("========================================")));
		  write(LineBuffer,		(CR & STRING'("POC TESTBENCH REPORT")));
		  write(LineBuffer,		(CR & STRING'("========================================")));
			write(LineBuffer,		(CR & STRING'("Assertions   ") & INTEGER'image(AssertCount)));
			write(LineBuffer,		(CR & STRING'("  failed     ") & INTEGER'image(FailedAssertCount)));
			write(LineBuffer,		(CR & STRING'("Processes    ") & INTEGER'image(ProcessCount)));
			write(LineBuffer,		(CR & STRING'("  active     ") & INTEGER'image(ActiveProcessCount)));
			for i in 0 to ProcessCount - 1 loop
				if (Processes(i).Status = SIM_PROCESS_STATUS_ACTIVE) then
					write(LineBuffer,	(CR & STRING'("    ") & str_trim(Processes(i).Name)));
				end if;
			end loop;
			write(LineBuffer,		(CR & STRING'("Tests        ") & INTEGER'image(TestCount)));
			for i in 0 to TestCount - 1 loop
				write(LineBuffer,	(CR & STRING'("  ") & str_ralign(INTEGER'image(i), log10ceil(T_SIM_TEST_ID'high)) & ": " & str_trim(Tests(i).Name)));
			end loop;
		  write(LineBuffer,		(CR & STRING'("========================================")));
			if (AssertCount = 0) then
			  write(LineBuffer, (CR & STRING'("SIMULATION RESULT = NO ASSERTS")));
		  elsif (Passed = TRUE) then
			  write(LineBuffer, (CR & STRING'("SIMULATION RESULT = PASSED")));
		  else
		  	write(LineBuffer, (CR & STRING'("SIMULATION RESULT = FAILED")));
		  end if;
		  write(LineBuffer,		(CR & STRING'("========================================")));
		  writeline(output, LineBuffer);
		end procedure;
		
		-- impure function registerProcess(Name : STRING; InstanceName : STRING) return T_SIM_PROCESS_ID is
		impure function registerProcess(Name : STRING) return T_SIM_PROCESS_ID is
			variable Proc						: T_SIM_PROCESS;
		begin
			Proc.ID									:= ProcessCount;
			Proc.Name								:= resize(Name, T_SIM_PROCESS_NAME'length);
			-- Proc.InstanceName				:= resize(InstanceName, T_SIM_PROCESS_INSTNAME'length);
			Proc.Status							:= SIM_PROCESS_STATUS_ACTIVE;
			
			Processes(Proc.ID)			:= Proc;
			ProcessCount						:= ProcessCount + 1;
			ActiveProcessCount			:= ActiveProcessCount + 1;
			return Proc.ID;
		end function;
		
		procedure deactivateProcess(ProcID : T_SIM_PROCESS_ID) is
			variable hasActiveProcesses		: BOOLEAN		:= FALSE;
		begin
			if (ProcID < ProcessCount) then
				if (Processes(ProcID).Status = SIM_PROCESS_STATUS_ACTIVE) then
					Processes(ProcID).Status	:= SIM_PROCESS_STATUS_ENDED;
					ActiveProcessCount				:= ActiveProcessCount - 1;
				end if;
			end if;
			
			if (ActiveProcessCount = 0) then
				stopAllClocks;
			end if;
		end procedure;
		
		impure function createTest(Name : STRING) return T_SIM_TEST_ID is
			variable Test			: T_SIM_TEST;
		begin
			Test.ID						:= TestCount;
			Test.Name					:= resize(Name, T_SIM_TEST_NAME'length);
			Test.Status				:= SIM_TEST_STATUS_ACTIVE;
		
			Tests(Test.ID)		:= Test;
			TestCount					:= TestCount + 1;
			return Test.ID;
		end function;
		
		procedure stopAllClocks is
		begin
			MainClockEnable		:= FALSE;
		end procedure;
		
		impure function isStopped return BOOLEAN is
		begin
			return not MainClockEnable;
		end function;
	end protected body;
end package body;
