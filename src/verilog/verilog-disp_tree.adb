--  Verilog tree dump
--  Copyright (C) 2023 Tristan Gingold
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
with Name_Table;
with Str_Table;
with Files_Map;
with Simple_IO; use Simple_IO;

with Verilog.Types; use Verilog.Types;
with Verilog.Nodes_Meta;

pragma Unreferenced (Str_Table);

package body Verilog.Disp_Tree is

   procedure Put_Indent (Indent : Natural) is
   begin
      Put (String'(1 .. 2 * Indent => ' '));
   end Put_Indent;

   Hex_Digits : constant array (Integer range 0 .. 15) of Character
     := "0123456789abcdef";

   procedure Disp_Uns32 (Val : Uns32)
   is
      Res : String (1 .. 8);
      V : Uns32 := Val;
   begin
      for I in reverse Res'Range loop
         Res (I) := Hex_Digits (Integer (V mod 16));
         V := V / 16;
      end loop;
      Put (Res);
   end Disp_Uns32;

   procedure Disp_Int32 (Val : Int32)
   is
      Res : String (1 .. 8);
      V : Int32 := Val;
   begin
      for I in reverse Res'Range loop
         Res (I) := Hex_Digits (Integer (V mod 16));
         V := V / 16;
      end loop;
      Put (Res);
   end Disp_Int32;

   function Image_Boolean (Bool : Boolean) return String is
   begin
      if Bool then
         return "true";
      else
         return "false";
      end if;
   end Image_Boolean;

   function Image_Lifetime (Live : Lifetime_Type) return String is
   begin
      case Live is
         when Life_Static =>
            return "static";
         when Life_Automatic =>
            return "automatic";
      end case;
   end Image_Lifetime;

   function Image_Binary_Ops (Op : Binary_Ops) return String is
   begin
      case Op is
         when Binop_Unknown =>
            return "???";

         when Binop_Logic_And =>
            return "&&";
         when Binop_Logic_Or =>
            return "||";
         when Binop_Logic_Imp =>
            return "->";
         when Binop_Logic_Eqv =>
            return "<->";

         when Binop_Ult
           | Binop_Slt =>
            return "<";
         when Binop_Ule
           | Binop_Sle =>
            return "<=";
         when Binop_Ugt
           | Binop_Sgt =>
            return ">";
         when Binop_Uge
           | Binop_Sge =>
            return ">=";

         when Binop_Log_Eq =>
            return "==";
         when Binop_Log_Ne =>
            return "!=";
         when Binop_Case_Eq =>
            return "===";
         when Binop_Case_Ne =>
            return "!==";

         when Binop_Bit_And =>
            return "&";
         when Binop_Bit_Or =>
            return "|";
         when Binop_Bit_Xor =>
            return "^";
         when Binop_Bit_Xnor =>
            return "^~";
         when Binop_Bit_Nxor =>
            return "~^";

         when Binop_Add =>
            return "+";
         when Binop_Sub =>
            return "-";
         when Binop_Umul
           | Binop_Smul =>
            return "*";
         when Binop_Udiv
           | Binop_Sdiv =>
            return "/";
         when Binop_Umod
           | Binop_Smod =>
            return "%";

         when Binop_Exp =>
            return "**";

         when Binop_Left_Lshift =>
            return "<<";
         when Binop_Right_Lshift =>
            return ">>";
         when Binop_Left_Ashift =>
            return "<<<";
         when Binop_Right_Ashift =>
            return ">>>";
      end case;
   end Image_Binary_Ops;

   function Image_Join_Type (Opt : Join_Type) return String is
   begin
      case Opt is
         when Join_All =>
            return "join";
         when Join_None =>
            return "join_none";
         when Join_Any =>
            return "join_any";
      end case;
   end Image_Join_Type;

   function Image_Udp_Symbol (Sym : Udp_Symbol) return String is
   begin
      case Sym is
         when Udp_0 =>
            return "0";
         when Udp_1 =>
            return "1";
         when Udp_X =>
            return "x";
         when Udp_Qm =>
            return "?";
         when Udp_B =>
            return "b";
         when Udp_R =>
            return "r";
         when Udp_F =>
            return "f";
         when Udp_P =>
            return "p";
         when Udp_N =>
            return "n";
         when Udp_Any =>
            return "*";
         when Udp_No =>
            return "-";
      end case;
   end Image_Udp_Symbol;

   function Image_Udp_Kind (Kind : Udp_Kind) return String is
   begin
      case Kind is
         when Udp_Combinational =>
            return "combinational";
         when Udp_Level_Sensitive =>
            return "level sensitive";
         when Udp_Edge_Sensitive =>
            return "edge sensitive";
      end case;
   end Image_Udp_Kind;

   procedure Disp_Node_Number (N : Node) is
   begin
      Put ('[');
      Disp_Int32 (Int32 (N));
      Put (']');
   end Disp_Node_Number;

   procedure Disp_Header (Msg : String; Indent : Natural) is
   begin
      Put_Indent (Indent);
      Put (Msg);
      Put (':');
   end Disp_Header;

   procedure Disp_Identifier (N : Node) is
   begin
      Put (Name_Table.Image (Get_Identifier (N)));
      New_Line;
   end Disp_Identifier;

   procedure Disp_String_Id (N : Node)
   is
      pragma Unreferenced (N);
   begin
      Put ('"');
      Put ("xxx");
      Put ('"');
      New_Line;
   end Disp_String_Id;

   procedure Disp_Binary_Op (N : Node) is
   begin
      Put (Binary_Ops'Image (Get_Binary_Op (N)));
      New_Line;
   end Disp_Binary_Op;

   procedure Disp_Unary_Op (N : Node) is
   begin
      Put (Unary_Ops'Image (Get_Unary_Op (N)));
      New_Line;
   end Disp_Unary_Op;

   function Image_Location
     (File: Name_Id; Line: Natural; Col: Natural) return String
   is
      Line_Str : constant String := Natural'Image (Line);
      Col_Str : constant String := Natural'Image (Col);
   begin
      return Name_Table.Image (File)
        & ':' & Line_Str (Line_Str'First + 1 .. Line_Str'Last)
        & ':' & Col_Str (Col_Str'First + 1 .. Col_Str'Last);
   end Image_Location;

   procedure Disp_Location (Loc : Location_Type)
   is
      Name : Name_Id;
      Line : Natural;
      Col : Natural;
   begin
      if Loc = Location_Nil then
         --  Avoid a crash, but should not happen.
         Put ("??:??:??:");
      else
         Files_Map.Location_To_Position (Loc, Name, Line, Col);
         Put (Image_Location (Name, Line, Col));
      end if;
   end Disp_Location;

   procedure Disp_Header (N : Node)
   is
      use Nodes_Meta;
      K : Nkind;
   begin
      if N = Null_Node then
         Put_Line ("*null*");
         return;
      end if;

      K := Get_Kind (N);
      Put (Get_Nkind_Image (K));
      if Has_Identifier (K) then
         Put (' ');
         Put (Name_Table.Image (Get_Identifier (N)));
      end if;

      Put (' ');
      Disp_Node_Number (N);

      New_Line;
   end Disp_Header;

   procedure Disp_Chain (Tree_Chain: Node; Indent: Natural; Depth : Natural)
   is
      El: Node;
   begin
      New_Line;
      El := Tree_Chain;
      while El /= Null_Node loop
         Put_Indent (Indent);
         Disp_Tree (El, Indent + 1, Depth);
         El := Get_Chain (El);
      end loop;
   end Disp_Chain;

   procedure Disp_Tree
     (N : Node; Indent : Natural := 0; Depth : Natural := 20) is
   begin
      Disp_Header (N);

      if Depth <= 1 or else N = Null_Node then
         return;
      end if;

      Disp_Header ("location", Indent);
      Put (' ');
      Disp_Location (Get_Location (N));
      New_Line;

      declare
         use Nodes_Meta;
         Sub_Indent : constant Natural := Indent + 1;

         Fields : constant Fields_Array := Get_Fields (Get_Kind (N));
         F : Fields_Enum;
      begin
         for I in Fields'Range loop
            F := Fields (I);
            Disp_Header (Get_Field_Image (F), Indent);
            if Get_Field_Type (F) /= Type_Node
              or else Get_Field_Actual_Attribute (N, F) /= Attr_Chain
            then
               Put (' ');
            end if;
            case Get_Field_Type (F) is
               when Type_Node =>
                  case Get_Field_Actual_Attribute (N, F) is
                     when Attr_None =>
                        Disp_Tree (Get_Node (N, F), Sub_Indent, Depth - 1);
                     when Attr_Ref
                       | Attr_Forward_Ref =>
                        Disp_Tree (Get_Node (N, F), Sub_Indent, 0);
                     when Attr_Chain =>
                        Disp_Chain (Get_Node (N, F), Sub_Indent, Depth - 1);
                     when Attr_Chain_Next =>
                        Disp_Node_Number (Get_Node (N, F));
                        New_Line;
                  end case;
               when Type_String8_Id =>
                  Put_Line ("<string8>");
               when Type_Base_Type =>
                  Put_Line (Base_Type'Image (Get_Base_Type (N, F)));
               when Type_Bn_Index =>
                  Put_Line (Bn_Index'Image (Get_Bn_Index (N, F)));
               when Type_Boolean =>
                  Put_Line (Image_Boolean (Get_Boolean (N, F)));
               when Type_Lifetime_Type =>
                  Put_Line (Image_Lifetime (Get_Lifetime_Type (N, F)));
               when Type_Int32 =>
                  Put_Line (Int32'Image (Get_Int32 (N, F)));
               when Type_Uns32 =>
                  Put_Line (Uns32'Image (Get_Uns32 (N, F)));
               when Type_Fp64 =>
                  Put_Line (Fp64'Image (Get_Fp64 (N, F)));
               when Type_Width_Type =>
                  Put_Line (Width_Type'Image (Get_Width_Type (N, F)));
               when Type_Tsize_Type =>
                  Put_Line (Tsize_Type'Image (Get_Tsize_Type (N, F)));
               when Type_Name_Id =>
                  Put_Line (Name_Table.Image (Get_Name_Id (N, F)));
               when Type_Unary_Ops =>
                  Put_Line (Unary_Ops'Image (Get_Unary_Ops (N, F)));
               when Type_Binary_Ops =>
                  Put (Binary_Ops'Image (Get_Binary_Ops (N, F)));
                  Put ("  ");
                  Put (Image_Binary_Ops (Get_Binary_Ops (N, F)));
                  New_Line;
               when Type_Conv_Ops =>
                  Put_Line (Conv_Ops'Image (Get_Conv_Ops (N, F)));
               when Type_Edge_Type =>
                  Put_Line (Edge_Type'Image (Get_Edge_Type (N, F)));
               when Type_Visibility_Type =>
                  Put_Line
                    (Visibility_Type'Image (Get_Visibility_Type (N, F)));
               when Type_DPI_Spec_Type =>
                  Put_Line (DPI_Spec_Type'Image (Get_DPI_Spec_Type (N, F)));
               when Type_Violation_Type =>
                  Put_Line (Violation_Type'Image (Get_Violation_Type (N, F)));
               when Type_Polarity_Type =>
                  Put_Line ("<polarity type>");
               when Type_Obj_Id =>
                  Put_Line (Obj_Id'Image (Get_Obj_Id (N, F)));
               when Type_Scope_Id =>
                  Put_Line (Scope_Id'Image (Get_Scope_Id (N, F)));
               when Type_Lit_Id =>
                  Put_Line (Lit_Id'Image (Get_Lit_Id (N, F)));
               when Type_Proc_Id =>
                  Put_Line (Proc_Id'Image (Get_Proc_Id (N, F)));
               when Type_Sys_Tf_Id =>
                  Put_Line (Sys_Tf_Id'Image (Get_Sys_Tf_Id (N, F)));
               when Type_Join_Type =>
                  Put_Line (Image_Join_Type (Get_Join_Type (N, F)));
               when Type_Udp_Symbol =>
                  Put_Line (Image_Udp_Symbol (Get_Udp_Symbol (N, F)));
               when Type_Udp_Kind =>
                  Put_Line (Image_Udp_Kind (Get_Udp_Kind (N, F)));
            end case;
         end loop;
      end;
   end Disp_Tree;

   pragma Unreferenced (Disp_Uns32, Disp_Node_Number,
                        Disp_Header, Disp_Identifier,
                        Disp_String_Id, Disp_Binary_Op, Disp_Unary_Op);
end Verilog.Disp_Tree;
