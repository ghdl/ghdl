--  Specialisation of File_Comments for vhdl
--  Copyright (C) 2022 Tristan Gingold
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

--  All the variables declared in this package are set by Parse_Option function
--  and can by read as soon as the command line is parsed.
--
--  Since the names are not prefixed, this package is expected to be with'ed
--  but not to be use'd.

with Types; use Types;
with File_Comments; use File_Comments;

with Vhdl.Nodes; use Vhdl.Nodes;

package Vhdl.Comments is
   --  Attach previously scanned comments to node N.
   procedure Gather_Comments (N : Iir);

   --  Return the first comment attached to node N.  FILE must be the file
   --  of N.
   --  Use File_Comments to iterate on comments.
   function Find_First_Comment (File : Source_File_Entry; N : Node)
                               return Comment_Index;
end Vhdl.Comments;
