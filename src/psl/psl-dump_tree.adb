--  This is in fact -*- Ada -*-
with Ada.Text_IO; use Ada.Text_IO;
with Types; use Types;
with Name_Table;
with PSL.Errors;
with PSL.Nodes_Meta;

package body PSL.Dump_Tree is

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

   procedure Disp_HDL_Node (Val : HDL_Node)
   is
   begin
      if Dump_Hdl_Node /= null then
         Dump_Hdl_Node.all (Val);
      else
         Disp_Int32 (Val);
      end if;
   end Disp_HDL_Node;

   procedure Disp_Node_Number (N : Node) is
   begin
      Put ('[');
      Disp_Int32 (Int32 (N));
      Put (']');
   end Disp_Node_Number;

   procedure Disp_NFA (Val : NFA) is
   begin
      Disp_Int32 (Int32 (Val));
   end Disp_NFA;

   procedure Disp_Header (Msg : String; Indent : Natural) is
   begin
      Put_Indent (Indent);
      Put (Msg);
      Put (": ");
   end Disp_Header;

   function Image_PSL_Presence_Kind (Pres : PSL_Presence_Kind) return String
   is
   begin
      case Pres is
         when Present_Pos =>
            return "+";
         when Present_Neg =>
            return "-";
         when Present_Unknown =>
            return "?";
      end case;
   end Image_PSL_Presence_Kind;

   procedure Disp_Location (Loc : Location_Type) is
   begin
      Put (PSL.Errors.Image (Loc));
   end Disp_Location;

--     procedure Disp_String_Id (N : Node) is
--     begin
--        Put ('"');
--        Put (Str_Table.Image (Get_String_Id (N)));
--        Put ('"');
--        New_Line;
--     end Disp_String_Id;

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

   procedure Disp_Tree (N : Node; Indent : Natural; Depth : Natural);

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

   procedure Disp_Tree (N : Node; Indent : Natural; Depth : Natural) is
   begin
      Disp_Header (N);

      if Depth <= 1 or else N = Null_Node then
         return;
      end if;

      Disp_Header ("location", Indent);
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
            case Get_Field_Type (F) is
               when Type_Node =>
                  case Get_Field_Attribute (F) is
                     when Attr_None =>
                        Disp_Tree (Get_Node (N, F), Sub_Indent, Depth - 1);
                     when Attr_Ref =>
                        Disp_Tree (Get_Node (N, F), Sub_Indent, 0);
                     when Attr_Chain =>
                        Disp_Chain (Get_Node (N, F), Sub_Indent, Depth - 1);
                     when Attr_Chain_Next =>
                        Disp_Node_Number (Get_Node (N, F));
                        New_Line;
                     when Attr_Maybe_Ref | Attr_Of_Ref =>
                        raise Internal_Error;
                  end case;
               when Type_Boolean =>
                  Put_Line (Image_Boolean (Get_Boolean (N, F)));
               when Type_Int32 =>
                  Disp_Int32 (Get_Int32 (N, F));
                  New_Line;
               when Type_Uns32 =>
                  Disp_Uns32 (Get_Uns32 (N, F));
                  New_Line;
               when Type_Name_Id =>
                  Put_Line (Name_Table.Image (Get_Name_Id (N, F)));
               when Type_HDL_Node =>
                  Disp_HDL_Node (Get_HDL_Node (N, F));
                  New_Line;
               when Type_NFA =>
                  Disp_NFA (Get_NFA (N, F));
                  New_Line;
               when Type_PSL_Presence_Kind =>
                  Put (Image_PSL_Presence_Kind (Get_PSL_Presence_Kind (N, F)));
                  New_Line;
            end case;
         end loop;
      end;
   end Disp_Tree;

   procedure Dump_Tree (N : Node; Full : Boolean := False) is
   begin
      if Full then
         Disp_Tree (N, 0, 20);
      else
         Disp_Tree (N, 0, 0);
      end if;
   end Dump_Tree;

end PSL.Dump_Tree;
