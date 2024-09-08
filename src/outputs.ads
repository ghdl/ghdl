--  Output subroutine for dumps
--  Copyright (C) 2024 Tristan Gingold
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <gnu.org/licenses>.

with Types; use Types;

package Outputs is
   --  To be called to create the output file.
   --  Use stdout if FILENAME is null.
   --  Return False in case of error.
   function Open_File (Filename : String_Acc) return Boolean;

   --  Flush and close the output.
   procedure Close;

   procedure Wr (S : String);
   procedure Wr (C : Character);
   procedure Wr_Line (S : String := "");

   procedure Wr_Trim (S : String);

   --  Put without leading blank.
   procedure Wr_Uns32 (V : Uns32);
   procedure Wr_Int32 (V : Int32);

   procedure Wr_Indent (Indent : Natural);
end Outputs;
