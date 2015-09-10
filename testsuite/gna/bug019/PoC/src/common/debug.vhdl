-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
--
-- ============================================================================
-- Authors:					Thomas B. Preusser
--									Martin Zabel
--									Patrick Lehmann
--
-- Package:					Debug helper functions.
--
-- Description:
-- ------------------------------------
--		This file declares a debug helper function to export enum encodings as a
--		ChipScope readable token file (*.tok).
--
-- License:
-- ============================================================================
-- Copyright 2007-2015 Technische Universitaet Dresden - Germany,
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
-- ============================================================================

use			STD.TextIO.all;

library	PoC;
use			PoC.strings.all;


package debug is
	impure function dbg_ExportEncoding(Name : STRING; encodings : string; tokenFileName : STRING) return BOOLEAN;

end package;


package body debug is
	impure function dbg_ExportEncoding(Name : STRING; encodings : string; tokenFileName : STRING) return BOOLEAN is
		file		tokenFile : TEXT open WRITE_MODE is tokenFileName;

		variable cnt, base : integer;
		variable l : line;
	begin
		report "Exporting encoding of '" & Name & "' to '" & tokenFileName & "'..." severity note;
		report "dbg_ExportEncoding: '" & encodings & "'" severity note;
		
		-- write file header
		write(l, STRING'("# Encoding file for '" & Name & "'"));	writeline(tokenFile, l);
		write(l, STRING'("#"));																		writeline(tokenFile, l);
		write(l, STRING'("# ChipScope Token File Version"));			writeline(tokenFile, l);
		write(l, STRING'("@FILE_VERSION=1.0.0"));									writeline(tokenFile, l);
		write(l, STRING'("#"));																		writeline(tokenFile, l);
		write(l, STRING'("# Default token value"));								writeline(tokenFile, l);
		write(l, STRING'("@DEFAULT_TOKEN="));											writeline(tokenFile, l);
		write(l, STRING'("#"));																		writeline(tokenFile, l);
		
		-- write state entires
		cnt  := 0;
		base := encodings'left;
		for i in encodings'range loop
			if encodings(i) = ';' then
				-- Leave the str_trim call in!
				-- Otherwise, the new parser of ISE 14.7 fails to slice properly.
				write(l, str_trim(encodings(base to i-1)));
				write(l, character'('='));
			  write(l, raw_format_nat_hex(cnt));
				writeline(tokenFile, l);
				cnt  := cnt + 1;
				base := i+1;
			end if;
		end loop;

		file_close(tokenFile);
		return true;
	end function;
end package body;

