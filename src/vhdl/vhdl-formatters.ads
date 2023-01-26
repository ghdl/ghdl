--  VHDL code formatter.
--  Copyright (C) 2019 Tristan Gingold
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

with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Prints;

package Vhdl.Formatters is
   type Format_Level is
     (
      --  No re-formatting.
      --  Trailing spaces are removed, keywords are converted to lower case.
      Format_None,

      --  Format_None + start of each line is adjusted
      Format_Indent,

      --  Format_Indent + spaces between tokens is adjusted
      Format_Space
     );

   --  Format/pretty print the file F.
   procedure Format (F : Iir_Design_File;
                     Level : Format_Level;
                     Flag_Realign : Boolean;
                     First_Line : Positive := 1;
                     Last_Line : Positive := Positive'Last);

   --  Reindent all lines of F between [First_Line; Last_Line] to HANDLE.
   procedure Indent_String (F : Iir_Design_File;
                            Handle : Vhdl.Prints.Vstring_Acc;
                            First_Line : Positive := 1;
                            Last_Line : Positive := Positive'Last);
end Vhdl.Formatters;
