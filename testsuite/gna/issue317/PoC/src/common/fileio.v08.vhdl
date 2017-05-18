-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- =============================================================================
-- Authors:					Patrick Lehmann
--
-- Package:     		File I/O-related Functions.
--
-- Description:
-- -------------------------------------
-- .. TODO:: No documentation available.
--
-- License:
-- =============================================================================
-- Copyright 2007-2016 Technische Universitaet Dresden - Germany,
--  					 				 Chair of VLSI-Design, Diagnostics and Architecture
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
use			PoC.utils.all;
use			PoC.strings.all;
use			PoC.ProtectedTypes.all;


package FileIO is
	subtype T_LOGFILE_OPEN_KIND is FILE_OPEN_KIND range WRITE_MODE to APPEND_MODE;

	-- Constant declarations
	constant C_LINEBREAK : string;

	-- ===========================================================================
	type T_LOGFILE is protected
		procedure				OpenFile(FileName : string; OpenKind : T_LOGFILE_OPEN_KIND := WRITE_MODE);
		impure function	OpenFile(FileName : string; OpenKind : T_LOGFILE_OPEN_KIND := WRITE_MODE) return FILE_OPEN_STATUS;
		procedure				OpenFile(Status : out FILE_OPEN_STATUS; FileName : string; OpenKind : T_LOGFILE_OPEN_KIND := WRITE_MODE);
		impure function IsOpen return boolean;
		procedure				CloseFile;

		procedure				Print(str : string);
		procedure				PrintLine(str : string := "");
		procedure				Flush;
		-- procedure WriteLine(LineBuffer : inout LINE);
	end protected;

	-- ===========================================================================
	type T_FILE is protected
		procedure				OpenFile(FileName : string; OpenKind : FILE_OPEN_KIND := WRITE_MODE);
		impure function	OpenFile(FileName : string; OpenKind : FILE_OPEN_KIND := WRITE_MODE) return FILE_OPEN_STATUS;
		procedure				OpenFile(Status : out FILE_OPEN_STATUS; FileName : string; OpenKind : FILE_OPEN_KIND := WRITE_MODE);
		impure function IsOpen return boolean;
		procedure				CloseFile;

		procedure				Print(str : string);
		procedure				PrintLine(str : string := "");
		procedure				Flush;
		-- procedure WriteLine(LineBuffer : inout LINE);
	end protected;

	type T_STDOUT is protected
		procedure				Print(str : string);
		procedure				PrintLine(str : string := "");
		procedure				Flush;
	end protected;
end package;


package body FileIO is
	constant C_LINEBREAK : string := ite(str_equal(MY_OPERATING_SYSTEM, "WINDOWS"), (CR & LF), (1 => LF));

	-- ===========================================================================
	file						Global_LogFile			: TEXT;
	-- shared variable	LogFile_IsOpen			: P_BOOLEAN;
	-- shared variable	LogFile							: T_LOGFILE;
	-- shared variable	StdOut							: T_STDOUT;
	-- shared variable LogFile_IsMirrored	: P_BOOLEAN;

	-- ===========================================================================
	type T_LOGFILE is protected body
		variable LineBuffer			: LINE;
		variable Local_IsOpen		: boolean;
		variable Local_FileName	: string(1 to 256);

		procedure OpenFile(FileName : string; OpenKind : T_LOGFILE_OPEN_KIND := WRITE_MODE) is
			variable Status	: FILE_OPEN_STATUS;
		begin
			OpenFile(Status, FileName, OpenKind);
		end procedure;

		impure function OpenFile(FileName : string; OpenKind : T_LOGFILE_OPEN_KIND := WRITE_MODE) return FILE_OPEN_STATUS is
			variable Status	: FILE_OPEN_STATUS;
		begin
			OpenFile(Status, FileName, OpenKind);
			return Status;
		end function;

		procedure OpenFile(Status : out FILE_OPEN_STATUS; FileName : string; OpenKind : T_LOGFILE_OPEN_KIND := WRITE_MODE) is
			variable Status_i : FILE_OPEN_STATUS;
		begin
			if not Local_IsOpen then
				file_open(Status_i, Global_LogFile, FileName, OpenKind);
				Local_IsOpen		:= Status_i = OPEN_OK;
				Local_FileName	:= resize(FileName, Local_FileName'length);
				Status 					:= Status_i;
			else
				report "Global log file '" & str_trim(Local_FileName) & "' is already open." severity ERROR;
			end if;
		end procedure;

		impure function IsOpen return boolean is
		begin
			return Local_IsOpen;
		end function;

		procedure CloseFile is
		begin
			if Local_IsOpen then
				file_close(Global_LogFile);
				Local_IsOpen	:= FALSE;
			end if;
		end procedure;

		procedure WriteLine(LineBuffer : inout LINE) is
		begin
			if not Local_IsOpen then
				writeline(OUTPUT, LineBuffer);
			-- elsif (LogFile_IsMirrored.Get = TRUE) then
				-- tee(Global_LogFile, LineBuffer);
			else
				writeline(Global_LogFile, LineBuffer);
			end if ;
		end procedure;

		procedure Print(str : string) is
		begin
			write(LineBuffer, str);
		end procedure;

		procedure PrintLine(str : string := "") is
		begin
			write(LineBuffer, str);
			WriteLine(LineBuffer);
		end procedure;

		procedure Flush is
		begin
			WriteLine(LineBuffer);
		end procedure;
	end protected body;

	type T_FILE is protected body
		file			LocalFile				: TEXT;
		variable	LineBuffer			: LINE;
		variable	Local_IsOpen		: boolean;
		variable	Local_FileName	: string(1 to 256);

		procedure OpenFile(FileName : string; OpenKind : FILE_OPEN_KIND := WRITE_MODE) is
			variable Status	: FILE_OPEN_STATUS;
		begin
			OpenFile(Status, FileName, OpenKind);
		end procedure;

		impure function OpenFile(FileName : string; OpenKind : FILE_OPEN_KIND := WRITE_MODE) return FILE_OPEN_STATUS is
			variable Status	: FILE_OPEN_STATUS;
		begin
			OpenFile(Status, FileName, OpenKind);
			return Status;
		end function;

		impure function IsOpen return boolean is
		begin
			return Local_IsOpen;
		end function;

		procedure OpenFile(Status : out FILE_OPEN_STATUS; FileName : string; OpenKind : FILE_OPEN_KIND := WRITE_MODE) is
			variable Status_i : FILE_OPEN_STATUS;
		begin
			if not Local_IsOpen then
				file_open(Status_i, LocalFile, FileName, OpenKind);
				Local_IsOpen		:= Status_i = OPEN_OK;
				Local_FileName	:= resize(FileName, Local_FileName'length);
				Status 					:= Status_i;
			else
				report "File '" & str_trim(Local_FileName) & "' is already open." severity ERROR;
			end if;
		end procedure;

		procedure CloseFile is
		begin
			if Local_IsOpen then
				file_close(LocalFile);
				Local_IsOpen	:= FALSE;
			end if;
		end procedure;

		procedure WriteLine(LineBuffer : inout LINE) is
		begin
			if not Local_IsOpen then
				report "File is not open." severity ERROR;
			else
				writeline(LocalFile, LineBuffer);
			end if ;
		end procedure;

		procedure Print(str : string) is
		begin
			write(LineBuffer, str);
		end procedure;

		procedure PrintLine(str : string := "") is
		begin
			write(LineBuffer, str);
			WriteLine(LineBuffer);
		end procedure;

		procedure Flush is
		begin
			WriteLine(LineBuffer);
		end procedure;
	end protected body;

	type T_STDOUT is protected body
		variable LineBuffer	: LINE;

		procedure Print(str : string) is
		begin
			write(LineBuffer, str);
		end procedure;

		procedure PrintLine(str : string := "") is
		begin
			write(LineBuffer, str);
			writeline(OUTPUT, LineBuffer);
		end procedure;

		procedure Flush is
		begin
			writeline(OUTPUT, LineBuffer);
		end procedure;
	end protected body;
end package body;
