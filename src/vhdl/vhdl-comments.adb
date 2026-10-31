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

with Files_Map;
with Vhdl.Scanner;

package body Vhdl.Comments is
   procedure Gather_Comments_Block (Rng : Comments_Range; N : Iir) is
   begin
      Gather_Comments_Block (Rng, Uns32 (N));
   end Gather_Comments_Block;

   procedure Gather_Comments_Block (N : Iir) is
   begin
      Gather_Comments (Uns32 (N));
   end Gather_Comments_Block;

   procedure Gather_Comments_Line (N : Iir)
   is
      Coord : Source_Coord_Type;
   begin
      Coord := Scanner.Get_Current_Coord;
      Gather_Comments_Line
        (Coord.Line_Pos, Scanner.Get_Token_Position, Uns32 (N));
   end Gather_Comments_Line;

   procedure Gather_Comments_Line_End (N : Iir; Loc : Location_Type)
   is
      Coord : Source_Coord_Type;
      File : Source_File_Entry;
      Decl_Line_Start : Source_Ptr;
      Line : Positive;
      Offset : Natural;
   begin
      Files_Map.Location_To_Coord (Loc, File, Decl_Line_Start, Line, Offset);
      Coord := Scanner.Get_Current_Coord;
      Gather_Comments_Line_End (Decl_Line_Start, Coord.Line_Pos,
                                Scanner.Get_Token_Position, Uns32 (N));
   end Gather_Comments_Line_End;

   procedure Replace_Comments_Node (Old_N, New_N : Iir) is
   begin
      Replace_Comments_Node (Uns32 (Old_N), Uns32 (New_N));
   end Replace_Comments_Node;

   function Find_First_Comment (File : Source_File_Entry; N : Node)
                               return Comment_Index
   is
      pragma Assert (Files_Map.Location_To_File (Get_Location (N)) = File);
   begin
      return Find_First_Comment (File, Uns32 (N));
   end Find_First_Comment;
end Vhdl.Comments;
