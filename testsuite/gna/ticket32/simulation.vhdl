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

LIBRARY IEEE;
USE			IEEE.STD_LOGIC_1164.ALL;

package simulation is
  --+ Test Bench Status Management ++++++++++++++++++++++++++++++++++++++++++

  --* The testbench is marked as failed. If a message is provided, it is
  --* reported as an error.
  procedure tbFail(msg : in string := "");

  --* If the passed condition has evaluated false, the testbench is marked
  --* as failed. In this case, the optional message will be reported as an
  --* an error if one was provided.
	procedure tbAssert(cond : in boolean; msg : in string := "");

  --* Prints out the overall testbench result as defined by the automated
  --* testbench process. Unless tbFail() or tbAssert() with a false condition
  --* have been called before, a successful completion will be reported, a
  --* failure otherwise.
	procedure tbPrintResult;
	
	-- TODO: integrate VCD simulation functions and procedures from sim_value_change_dump.vhdl here
	
	-- checksum functions
	-- ===========================================================================
	-- TODO: move checksum functions here
end;


use	std.TextIO.all;

package body simulation is

  --+ Test Bench Status Management ++++++++++++++++++++++++++++++++++++++++++

  --* Internal state variable to log a failure condition for final reporting.
  --* Once de-asserted, this variable will never return to a value of true.
  shared variable pass : boolean := true;

  procedure tbFail(msg : in string := "") is
  begin
		if msg'length > 0 then
			report msg severity error;
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

	-- checksum functions
	-- ===========================================================================

end package body;
