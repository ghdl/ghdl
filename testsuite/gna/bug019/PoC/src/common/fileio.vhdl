-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
--
-- ============================================================================
-- Package:     File I/O-related Functions.
--
-- Authors:			Patrick Lehmann
--							Thomas B. Preusser
--
-- Description:
--   Exploring the options for providing a more convenient API than std.textio.
--   Not yet recommended for adoption as it depends on the VHDL generation and
--   still is under discussion.
--
--	 Open problems:
--     - verify that std.textio.write(text, string) is, indeed, specified and
--              that it does *not* print a trailing \newline
--          -> would help to eliminate line buffering in shared variables
--     - move C_LINEBREAK to my_config to keep platform dependency out?
--
-- License:
-- ============================================================================
-- Copyright 2007-2015 Technische Universitaet Dresden - Germany,
--  					 				 Chair for VLSI-Design, Diagnostics and Architecture
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

library	PoC;
use			PoC.my_project.all;


package FileIO is
	-- Constant declarations
	constant C_LINEBREAK : STRING;

	-- =============================================================================
	procedure stdout_write    (str : STRING);
	procedure stdout_writeline(str : STRING := "");

end package;

package body FileIO is
	function ite(cond : BOOLEAN; value1 : STRING; value2 : STRING) return STRING is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;

	function str_equal(str1 : STRING; str2 : STRING) return BOOLEAN is
	begin
		if str1'length /= str2'length then
			return FALSE;
		else
			return (str1 = str2);
		end if;
	end function;
	
	-- =============================================================================
	constant C_LINEBREAK : STRING := ite(str_equal(MY_OPERATING_SYSTEM, "WINDOWS"), (CR & LF), (1 => LF));

	-- =============================================================================
	shared variable stdout_line : line;
	shared variable stderr_line : line;

	procedure stdout_write(str : STRING) is
	begin
		write(stdout_line, str);
	end procedure;
	
	procedure stdout_writeline(str : STRING := "") is
	begin
		write(stdout_line, str);
		writeline(output, stdout_line);
	end procedure;
	
end package body;
