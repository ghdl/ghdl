--  Node displaying (for debugging).
--  Copyright (C) 2002, 2003, 2004, 2005, 2009 Tristan Gingold
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
with Vhdl.Tokens; use Vhdl.Tokens;

package Vhdl.Disp_Tree is
   --  Disp TREE recursively.
   procedure Disp_Tree (Tree : Iir; Flat : Boolean := False);

   procedure Disp_Iir (N : Iir; Indent : Natural; Depth : Natural);

   procedure Disp_Tree_For_Psl
     (N : Int32; Indent : Natural; Depth : Natural);

   --  Image for various field types.
   function Image_Name_Id (Ident: Name_Id) return String;
   function Image_Iir_Staticness (Static: Iir_Staticness) return String;
   function Image_Boolean (Bool : Boolean) return String;
   function Image_Iir_Delay_Mechanism (Mech : Iir_Delay_Mechanism)
                                      return String;
   function Image_Iir_Mode (Mode : Iir_Mode) return String;
   function Image_Iir_Force_Mode (Mode : Iir_Force_Mode) return String;
   function Image_Iir_Signal_Kind (Kind : Iir_Signal_Kind) return String;
   function Image_Iir_Pure_State (State : Iir_Pure_State) return String;
   function Image_Iir_All_Sensitized (Sig : Iir_All_Sensitized)
                                     return String;
   function Image_Iir_Constraint (Const : Iir_Constraint) return String;
   function Image_Date_State_Type (State : Date_State_Type) return String;
   function Image_Tri_State_Type (State : Tri_State_Type) return String;
   function Image_Time_Stamp_Id (Id : Time_Stamp_Id) return String;
   function Image_File_Checksum_Id (Id : File_Checksum_Id) return String;
   function Image_Iir_Predefined_Functions (F : Iir_Predefined_Functions)
                                           return String;
   function Image_Location_Type (Loc : Location_Type) return String;
   function Image_Direction_Type (Dir : Direction_Type) return String;
   function Image_Token_Type (Tok : Vhdl.Tokens.Token_Type) return String;
   function Image_String8 (N : Iir) return String;
   function Image_Scalar_Size (Sz : Scalar_Size) return String;
end Vhdl.Disp_Tree;
