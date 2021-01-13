--  VHDL parser.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with Vhdl.Nodes; use Vhdl.Nodes;

package Vhdl.Parse is
   --  If True, create nodes for parenthesis expressions.
   Flag_Parse_Parenthesis : Boolean := False;

   type Prio_Type is
     (
      Prio_Expression,
      Prio_Logical,
      Prio_Relation,
      Prio_Shift,
      Prio_Simple,
      Prio_Term,
      Prio_Factor
     );

   --  Parse an expression.
   --  (Used by PSL).
   function Parse_Expression (Prio : Prio_Type := Prio_Expression) return Iir;
   function Parse_Binary_Expression (Left : Iir; Prio : Prio_Type) return Iir;

   --  Convert the STR (0 .. LEN - 1) into a operator symbol identifier.
   --  Emit an error message if the name is not an operator name.
   function Str_To_Operator_Name (Str_Id : String8_Id;
                                  Len : Nat32;
                                  Loc : Location_Type) return Name_Id;

   --  Convert string literal STR to an operator symbol.
   --  Emit an error message if the string is not an operator name.
   function String_To_Operator_Symbol (Str : Iir) return Iir;

   --  Parse a single design unit.
   --  The scanner must have been initialized, however, the current_token
   --  shouldn't have been set.
   --  At return, the last token accepted is the semi_colon that terminates
   --  the library unit.
   --  Return Null_Iir when end of file.
   function Parse_Design_Unit return Iir_Design_Unit;

   --  Parse a file.
   --  The scanner must have been initialized as for parse_design_unit.
   --  Return Null_Iir in case of error.
   function Parse_Design_File return Iir_Design_File;
end Vhdl.Parse;
