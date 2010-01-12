--  This is in fact -*- Ada -*-
with Ada.Text_IO; use Ada.Text_IO;
with Types; use Types;
with Name_Table;
with PSL.Errors;

package body PSL.Dump_Tree is

   procedure Disp_Indent (Indent : Natural) is
   begin
      Put (String'(1 .. 2 * Indent => ' '));
   end Disp_Indent;

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
      Disp_Indent (Indent);
      Put (Msg);
      Put (": ");
   end Disp_Header;

   procedure Disp_Identifier (N : Node) is
   begin
      Put (Name_Table.Image (Get_Identifier (N)));
      New_Line;
   end Disp_Identifier;

   procedure Disp_Label (N : Node) is
   begin
      Put (Name_Table.Image (Get_Label (N)));
      New_Line;
   end Disp_Label;

   procedure Disp_Boolean (Val : Boolean) is
   begin
      if Val then
         Put ("true");
      else
         Put ("false");
      end if;
   end Disp_Boolean;

   procedure Disp_PSL_Presence_Kind (Pres : PSL_Presence_Kind) is
   begin
      case Pres is
         when Present_Pos =>
            Put ('+');
         when Present_Neg =>
            Put ('-');
         when Present_Unknown =>
            Put ('?');
      end case;
   end Disp_PSL_Presence_Kind;

   procedure Disp_Location (Loc : Location_Type) is
   begin
      Put (PSL.Errors.Get_Location_Str (Loc));
   end Disp_Location;

--     procedure Disp_String_Id (N : Node) is
--     begin
--        Put ('"');
--        Put (Str_Table.Image (Get_String_Id (N)));
--        Put ('"');
--        New_Line;
--     end Disp_String_Id;

   --  Subprograms.
   procedure Disp_Tree (N : Node; Indent : Natural; Full : boolean := False) is
   begin
      Disp_Indent (Indent);
      Disp_Node_Number (N);
      Put (": ");
      if N = Null_Node then
         Put_Line ("*NULL*");
         return;
      end if;
      Put_Line (Nkind'Image (Get_Kind (N)));
      Disp_Indent (Indent);
      Put ("loc: ");
      Disp_Location (Get_Location (N));
      New_Line;
      case Get_Kind (N) is
         when N_Error =>
            if not Full then
               return;
            end if;
            null;
         when N_Vmode =>
            Disp_Header ("Identifier", Indent + 1);
            Disp_Identifier (N);
            if not Full then
               return;
            end if;
            Disp_Header ("Instance", Indent + 1);
            New_Line;
            Disp_Tree (Get_Instance (N), Indent + 1, Full);
            Disp_Header ("Item_Chain", Indent + 1);
            New_Line;
            Disp_Tree (Get_Item_Chain (N), Indent + 1, Full);
            Disp_Tree (Get_Chain (N), Indent, Full);
            null;
         when N_Vunit =>
            Disp_Header ("Identifier", Indent + 1);
            Disp_Identifier (N);
            if not Full then
               return;
            end if;
            Disp_Header ("Instance", Indent + 1);
            New_Line;
            Disp_Tree (Get_Instance (N), Indent + 1, Full);
            Disp_Header ("Item_Chain", Indent + 1);
            New_Line;
            Disp_Tree (Get_Item_Chain (N), Indent + 1, Full);
            Disp_Tree (Get_Chain (N), Indent, Full);
            null;
         when N_Vprop =>
            Disp_Header ("Identifier", Indent + 1);
            Disp_Identifier (N);
            if not Full then
               return;
            end if;
            Disp_Header ("Instance", Indent + 1);
            New_Line;
            Disp_Tree (Get_Instance (N), Indent + 1, Full);
            Disp_Header ("Item_Chain", Indent + 1);
            New_Line;
            Disp_Tree (Get_Item_Chain (N), Indent + 1, Full);
            Disp_Tree (Get_Chain (N), Indent, Full);
            null;
         when N_Hdl_Mod_Name =>
            Disp_Header ("Identifier", Indent + 1);
            Disp_Identifier (N);
            if not Full then
               return;
            end if;
            Disp_Header ("Prefix", Indent + 1);
            New_Line;
            Disp_Tree (Get_Prefix (N), Indent + 1, Full);
            null;
         when N_Assert_Directive =>
            Disp_Header ("Label", Indent + 1);
            Disp_Label (N);
            if not Full then
               return;
            end if;
            Disp_Header ("String", Indent + 1);
            New_Line;
            Disp_Tree (Get_String (N), Indent + 1, Full);
            Disp_Header ("Property", Indent + 1);
            New_Line;
            Disp_Tree (Get_Property (N), Indent + 1, Full);
            Disp_Header ("NFA", Indent + 1);
            Disp_NFA (Get_NFA (N));
            New_Line;
            Disp_Tree (Get_Chain (N), Indent, Full);
            null;
         when N_Property_Declaration =>
            Disp_Header ("Identifier", Indent + 1);
            Disp_Identifier (N);
            if not Full then
               return;
            end if;
            Disp_Header ("Property", Indent + 1);
            New_Line;
            Disp_Tree (Get_Property (N), Indent + 1, Full);
            Disp_Header ("Global_Clock", Indent + 1);
            New_Line;
            Disp_Tree (Get_Global_Clock (N), Indent + 1, Full);
            Disp_Header ("Parameter_List", Indent + 1);
            New_Line;
            Disp_Tree (Get_Parameter_List (N), Indent + 1, Full);
            Disp_Tree (Get_Chain (N), Indent, Full);
            null;
         when N_Sequence_Declaration =>
            Disp_Header ("Identifier", Indent + 1);
            Disp_Identifier (N);
            if not Full then
               return;
            end if;
            Disp_Header ("Parameter_List", Indent + 1);
            New_Line;
            Disp_Tree (Get_Parameter_List (N), Indent + 1, Full);
            Disp_Header ("Sequence", Indent + 1);
            New_Line;
            Disp_Tree (Get_Sequence (N), Indent + 1, Full);
            Disp_Tree (Get_Chain (N), Indent, Full);
            null;
         when N_Endpoint_Declaration =>
            Disp_Header ("Identifier", Indent + 1);
            Disp_Identifier (N);
            if not Full then
               return;
            end if;
            Disp_Header ("Parameter_List", Indent + 1);
            New_Line;
            Disp_Tree (Get_Parameter_List (N), Indent + 1, Full);
            Disp_Header ("Sequence", Indent + 1);
            New_Line;
            Disp_Tree (Get_Sequence (N), Indent + 1, Full);
            Disp_Tree (Get_Chain (N), Indent, Full);
            null;
         when N_Const_Parameter =>
            Disp_Header ("Identifier", Indent + 1);
            Disp_Identifier (N);
            if not Full then
               return;
            end if;
            Disp_Header ("Actual", Indent + 1);
            New_Line;
            Disp_Tree (Get_Actual (N), Indent + 1, Full);
            Disp_Tree (Get_Chain (N), Indent, Full);
            null;
         when N_Boolean_Parameter =>
            Disp_Header ("Identifier", Indent + 1);
            Disp_Identifier (N);
            if not Full then
               return;
            end if;
            Disp_Header ("Actual", Indent + 1);
            New_Line;
            Disp_Tree (Get_Actual (N), Indent + 1, Full);
            Disp_Tree (Get_Chain (N), Indent, Full);
            null;
         when N_Property_Parameter =>
            Disp_Header ("Identifier", Indent + 1);
            Disp_Identifier (N);
            if not Full then
               return;
            end if;
            Disp_Header ("Actual", Indent + 1);
            New_Line;
            Disp_Tree (Get_Actual (N), Indent + 1, Full);
            Disp_Tree (Get_Chain (N), Indent, Full);
            null;
         when N_Sequence_Parameter =>
            Disp_Header ("Identifier", Indent + 1);
            Disp_Identifier (N);
            if not Full then
               return;
            end if;
            Disp_Header ("Actual", Indent + 1);
            New_Line;
            Disp_Tree (Get_Actual (N), Indent + 1, Full);
            Disp_Tree (Get_Chain (N), Indent, Full);
            null;
         when N_Sequence_Instance =>
            if not Full then
               return;
            end if;
            Disp_Header ("Declaration", Indent + 1);
            New_Line;
            Disp_Tree (Get_Declaration (N), Indent + 1, False);
            Disp_Header ("Association_Chain", Indent + 1);
            New_Line;
            Disp_Tree (Get_Association_Chain (N), Indent + 1, Full);
            null;
         when N_Endpoint_Instance =>
            if not Full then
               return;
            end if;
            Disp_Header ("Declaration", Indent + 1);
            New_Line;
            Disp_Tree (Get_Declaration (N), Indent + 1, False);
            Disp_Header ("Association_Chain", Indent + 1);
            New_Line;
            Disp_Tree (Get_Association_Chain (N), Indent + 1, Full);
            null;
         when N_Property_Instance =>
            if not Full then
               return;
            end if;
            Disp_Header ("Declaration", Indent + 1);
            New_Line;
            Disp_Tree (Get_Declaration (N), Indent + 1, False);
            Disp_Header ("Association_Chain", Indent + 1);
            New_Line;
            Disp_Tree (Get_Association_Chain (N), Indent + 1, Full);
            null;
         when N_Actual =>
            if not Full then
               return;
            end if;
            Disp_Header ("Actual", Indent + 1);
            New_Line;
            Disp_Tree (Get_Actual (N), Indent + 1, Full);
            Disp_Header ("Formal", Indent + 1);
            New_Line;
            Disp_Tree (Get_Formal (N), Indent + 1, Full);
            Disp_Tree (Get_Chain (N), Indent, Full);
            null;
         when N_Clock_Event =>
            if not Full then
               return;
            end if;
            Disp_Header ("Property", Indent + 1);
            New_Line;
            Disp_Tree (Get_Property (N), Indent + 1, Full);
            Disp_Header ("Boolean", Indent + 1);
            New_Line;
            Disp_Tree (Get_Boolean (N), Indent + 1, Full);
            null;
         when N_Always =>
            if not Full then
               return;
            end if;
            Disp_Header ("Property", Indent + 1);
            New_Line;
            Disp_Tree (Get_Property (N), Indent + 1, Full);
            null;
         when N_Never =>
            if not Full then
               return;
            end if;
            Disp_Header ("Property", Indent + 1);
            New_Line;
            Disp_Tree (Get_Property (N), Indent + 1, Full);
            null;
         when N_Eventually =>
            if not Full then
               return;
            end if;
            Disp_Header ("Property", Indent + 1);
            New_Line;
            Disp_Tree (Get_Property (N), Indent + 1, Full);
            null;
         when N_Strong =>
            if not Full then
               return;
            end if;
            Disp_Header ("Property", Indent + 1);
            New_Line;
            Disp_Tree (Get_Property (N), Indent + 1, Full);
            null;
         when N_Imp_Seq =>
            if not Full then
               return;
            end if;
            Disp_Header ("Property", Indent + 1);
            New_Line;
            Disp_Tree (Get_Property (N), Indent + 1, Full);
            Disp_Header ("Sequence", Indent + 1);
            New_Line;
            Disp_Tree (Get_Sequence (N), Indent + 1, Full);
            null;
         when N_Overlap_Imp_Seq =>
            if not Full then
               return;
            end if;
            Disp_Header ("Property", Indent + 1);
            New_Line;
            Disp_Tree (Get_Property (N), Indent + 1, Full);
            Disp_Header ("Sequence", Indent + 1);
            New_Line;
            Disp_Tree (Get_Sequence (N), Indent + 1, Full);
            null;
         when N_Log_Imp_Prop =>
            if not Full then
               return;
            end if;
            Disp_Header ("Left", Indent + 1);
            New_Line;
            Disp_Tree (Get_Left (N), Indent + 1, Full);
            Disp_Header ("Right", Indent + 1);
            New_Line;
            Disp_Tree (Get_Right (N), Indent + 1, Full);
            null;
         when N_Next =>
            if not Full then
               return;
            end if;
            Disp_Header ("Property", Indent + 1);
            New_Line;
            Disp_Tree (Get_Property (N), Indent + 1, Full);
            Disp_Header ("Strong_Flag", Indent + 1);
            Disp_Boolean (Get_Strong_Flag (N));
            New_Line;
            Disp_Header ("Number", Indent + 1);
            New_Line;
            Disp_Tree (Get_Number (N), Indent + 1, Full);
            null;
         when N_Next_A =>
            if not Full then
               return;
            end if;
            Disp_Header ("Property", Indent + 1);
            New_Line;
            Disp_Tree (Get_Property (N), Indent + 1, Full);
            Disp_Header ("Strong_Flag", Indent + 1);
            Disp_Boolean (Get_Strong_Flag (N));
            New_Line;
            Disp_Header ("Low_Bound", Indent + 1);
            New_Line;
            Disp_Tree (Get_Low_Bound (N), Indent + 1, Full);
            Disp_Header ("High_Bound", Indent + 1);
            New_Line;
            Disp_Tree (Get_High_Bound (N), Indent + 1, Full);
            null;
         when N_Next_E =>
            if not Full then
               return;
            end if;
            Disp_Header ("Property", Indent + 1);
            New_Line;
            Disp_Tree (Get_Property (N), Indent + 1, Full);
            Disp_Header ("Strong_Flag", Indent + 1);
            Disp_Boolean (Get_Strong_Flag (N));
            New_Line;
            Disp_Header ("Low_Bound", Indent + 1);
            New_Line;
            Disp_Tree (Get_Low_Bound (N), Indent + 1, Full);
            Disp_Header ("High_Bound", Indent + 1);
            New_Line;
            Disp_Tree (Get_High_Bound (N), Indent + 1, Full);
            null;
         when N_Next_Event =>
            if not Full then
               return;
            end if;
            Disp_Header ("Property", Indent + 1);
            New_Line;
            Disp_Tree (Get_Property (N), Indent + 1, Full);
            Disp_Header ("Boolean", Indent + 1);
            New_Line;
            Disp_Tree (Get_Boolean (N), Indent + 1, Full);
            Disp_Header ("Strong_Flag", Indent + 1);
            Disp_Boolean (Get_Strong_Flag (N));
            New_Line;
            Disp_Header ("Number", Indent + 1);
            New_Line;
            Disp_Tree (Get_Number (N), Indent + 1, Full);
            null;
         when N_Next_Event_A =>
            if not Full then
               return;
            end if;
            Disp_Header ("Property", Indent + 1);
            New_Line;
            Disp_Tree (Get_Property (N), Indent + 1, Full);
            Disp_Header ("Boolean", Indent + 1);
            New_Line;
            Disp_Tree (Get_Boolean (N), Indent + 1, Full);
            Disp_Header ("Strong_Flag", Indent + 1);
            Disp_Boolean (Get_Strong_Flag (N));
            New_Line;
            Disp_Header ("Low_Bound", Indent + 1);
            New_Line;
            Disp_Tree (Get_Low_Bound (N), Indent + 1, Full);
            Disp_Header ("High_Bound", Indent + 1);
            New_Line;
            Disp_Tree (Get_High_Bound (N), Indent + 1, Full);
            null;
         when N_Next_Event_E =>
            if not Full then
               return;
            end if;
            Disp_Header ("Property", Indent + 1);
            New_Line;
            Disp_Tree (Get_Property (N), Indent + 1, Full);
            Disp_Header ("Boolean", Indent + 1);
            New_Line;
            Disp_Tree (Get_Boolean (N), Indent + 1, Full);
            Disp_Header ("Strong_Flag", Indent + 1);
            Disp_Boolean (Get_Strong_Flag (N));
            New_Line;
            Disp_Header ("Low_Bound", Indent + 1);
            New_Line;
            Disp_Tree (Get_Low_Bound (N), Indent + 1, Full);
            Disp_Header ("High_Bound", Indent + 1);
            New_Line;
            Disp_Tree (Get_High_Bound (N), Indent + 1, Full);
            null;
         when N_Abort =>
            if not Full then
               return;
            end if;
            Disp_Header ("Property", Indent + 1);
            New_Line;
            Disp_Tree (Get_Property (N), Indent + 1, Full);
            Disp_Header ("Boolean", Indent + 1);
            New_Line;
            Disp_Tree (Get_Boolean (N), Indent + 1, Full);
            null;
         when N_Until =>
            if not Full then
               return;
            end if;
            Disp_Header ("Strong_Flag", Indent + 1);
            Disp_Boolean (Get_Strong_Flag (N));
            New_Line;
            Disp_Header ("Left", Indent + 1);
            New_Line;
            Disp_Tree (Get_Left (N), Indent + 1, Full);
            Disp_Header ("Right", Indent + 1);
            New_Line;
            Disp_Tree (Get_Right (N), Indent + 1, Full);
            Disp_Header ("Inclusive_Flag", Indent + 1);
            Disp_Boolean (Get_Inclusive_Flag (N));
            New_Line;
            null;
         when N_Before =>
            if not Full then
               return;
            end if;
            Disp_Header ("Strong_Flag", Indent + 1);
            Disp_Boolean (Get_Strong_Flag (N));
            New_Line;
            Disp_Header ("Left", Indent + 1);
            New_Line;
            Disp_Tree (Get_Left (N), Indent + 1, Full);
            Disp_Header ("Right", Indent + 1);
            New_Line;
            Disp_Tree (Get_Right (N), Indent + 1, Full);
            Disp_Header ("Inclusive_Flag", Indent + 1);
            Disp_Boolean (Get_Inclusive_Flag (N));
            New_Line;
            null;
         when N_Or_Prop =>
            if not Full then
               return;
            end if;
            Disp_Header ("Left", Indent + 1);
            New_Line;
            Disp_Tree (Get_Left (N), Indent + 1, Full);
            Disp_Header ("Right", Indent + 1);
            New_Line;
            Disp_Tree (Get_Right (N), Indent + 1, Full);
            null;
         when N_And_Prop =>
            if not Full then
               return;
            end if;
            Disp_Header ("Left", Indent + 1);
            New_Line;
            Disp_Tree (Get_Left (N), Indent + 1, Full);
            Disp_Header ("Right", Indent + 1);
            New_Line;
            Disp_Tree (Get_Right (N), Indent + 1, Full);
            null;
         when N_Braced_SERE =>
            if not Full then
               return;
            end if;
            Disp_Header ("SERE", Indent + 1);
            New_Line;
            Disp_Tree (Get_SERE (N), Indent + 1, Full);
            null;
         when N_Concat_SERE =>
            if not Full then
               return;
            end if;
            Disp_Header ("Left", Indent + 1);
            New_Line;
            Disp_Tree (Get_Left (N), Indent + 1, Full);
            Disp_Header ("Right", Indent + 1);
            New_Line;
            Disp_Tree (Get_Right (N), Indent + 1, Full);
            null;
         when N_Fusion_SERE =>
            if not Full then
               return;
            end if;
            Disp_Header ("Left", Indent + 1);
            New_Line;
            Disp_Tree (Get_Left (N), Indent + 1, Full);
            Disp_Header ("Right", Indent + 1);
            New_Line;
            Disp_Tree (Get_Right (N), Indent + 1, Full);
            null;
         when N_Within_SERE =>
            if not Full then
               return;
            end if;
            Disp_Header ("Left", Indent + 1);
            New_Line;
            Disp_Tree (Get_Left (N), Indent + 1, Full);
            Disp_Header ("Right", Indent + 1);
            New_Line;
            Disp_Tree (Get_Right (N), Indent + 1, Full);
            null;
         when N_Match_And_Seq =>
            if not Full then
               return;
            end if;
            Disp_Header ("Left", Indent + 1);
            New_Line;
            Disp_Tree (Get_Left (N), Indent + 1, Full);
            Disp_Header ("Right", Indent + 1);
            New_Line;
            Disp_Tree (Get_Right (N), Indent + 1, Full);
            null;
         when N_And_Seq =>
            if not Full then
               return;
            end if;
            Disp_Header ("Left", Indent + 1);
            New_Line;
            Disp_Tree (Get_Left (N), Indent + 1, Full);
            Disp_Header ("Right", Indent + 1);
            New_Line;
            Disp_Tree (Get_Right (N), Indent + 1, Full);
            null;
         when N_Or_Seq =>
            if not Full then
               return;
            end if;
            Disp_Header ("Left", Indent + 1);
            New_Line;
            Disp_Tree (Get_Left (N), Indent + 1, Full);
            Disp_Header ("Right", Indent + 1);
            New_Line;
            Disp_Tree (Get_Right (N), Indent + 1, Full);
            null;
         when N_Star_Repeat_Seq =>
            if not Full then
               return;
            end if;
            Disp_Header ("Sequence", Indent + 1);
            New_Line;
            Disp_Tree (Get_Sequence (N), Indent + 1, Full);
            Disp_Header ("Low_Bound", Indent + 1);
            New_Line;
            Disp_Tree (Get_Low_Bound (N), Indent + 1, Full);
            Disp_Header ("High_Bound", Indent + 1);
            New_Line;
            Disp_Tree (Get_High_Bound (N), Indent + 1, Full);
            null;
         when N_Goto_Repeat_Seq =>
            if not Full then
               return;
            end if;
            Disp_Header ("Sequence", Indent + 1);
            New_Line;
            Disp_Tree (Get_Sequence (N), Indent + 1, Full);
            Disp_Header ("Low_Bound", Indent + 1);
            New_Line;
            Disp_Tree (Get_Low_Bound (N), Indent + 1, Full);
            Disp_Header ("High_Bound", Indent + 1);
            New_Line;
            Disp_Tree (Get_High_Bound (N), Indent + 1, Full);
            null;
         when N_Plus_Repeat_Seq =>
            if not Full then
               return;
            end if;
            Disp_Header ("Sequence", Indent + 1);
            New_Line;
            Disp_Tree (Get_Sequence (N), Indent + 1, Full);
            null;
         when N_Equal_Repeat_Seq =>
            if not Full then
               return;
            end if;
            Disp_Header ("Sequence", Indent + 1);
            New_Line;
            Disp_Tree (Get_Sequence (N), Indent + 1, Full);
            Disp_Header ("Low_Bound", Indent + 1);
            New_Line;
            Disp_Tree (Get_Low_Bound (N), Indent + 1, Full);
            Disp_Header ("High_Bound", Indent + 1);
            New_Line;
            Disp_Tree (Get_High_Bound (N), Indent + 1, Full);
            null;
         when N_Not_Bool =>
            if not Full then
               return;
            end if;
            Disp_Header ("Boolean", Indent + 1);
            New_Line;
            Disp_Tree (Get_Boolean (N), Indent + 1, Full);
            Disp_Header ("Presence", Indent + 1);
            Disp_PSL_Presence_Kind (Get_Presence (N));
            New_Line;
            Disp_Header ("Hash", Indent + 1);
            Disp_Uns32 (Get_Hash (N));
            New_Line;
            Disp_Header ("Hash_Link", Indent + 1);
            New_Line;
            Disp_Tree (Get_Hash_Link (N), Indent + 1, Full);
            null;
         when N_And_Bool =>
            if not Full then
               return;
            end if;
            Disp_Header ("Left", Indent + 1);
            New_Line;
            Disp_Tree (Get_Left (N), Indent + 1, Full);
            Disp_Header ("Right", Indent + 1);
            New_Line;
            Disp_Tree (Get_Right (N), Indent + 1, Full);
            Disp_Header ("Presence", Indent + 1);
            Disp_PSL_Presence_Kind (Get_Presence (N));
            New_Line;
            Disp_Header ("Hash", Indent + 1);
            Disp_Uns32 (Get_Hash (N));
            New_Line;
            Disp_Header ("Hash_Link", Indent + 1);
            New_Line;
            Disp_Tree (Get_Hash_Link (N), Indent + 1, Full);
            null;
         when N_Or_Bool =>
            if not Full then
               return;
            end if;
            Disp_Header ("Left", Indent + 1);
            New_Line;
            Disp_Tree (Get_Left (N), Indent + 1, Full);
            Disp_Header ("Right", Indent + 1);
            New_Line;
            Disp_Tree (Get_Right (N), Indent + 1, Full);
            Disp_Header ("Presence", Indent + 1);
            Disp_PSL_Presence_Kind (Get_Presence (N));
            New_Line;
            Disp_Header ("Hash", Indent + 1);
            Disp_Uns32 (Get_Hash (N));
            New_Line;
            Disp_Header ("Hash_Link", Indent + 1);
            New_Line;
            Disp_Tree (Get_Hash_Link (N), Indent + 1, Full);
            null;
         when N_Imp_Bool =>
            if not Full then
               return;
            end if;
            Disp_Header ("Left", Indent + 1);
            New_Line;
            Disp_Tree (Get_Left (N), Indent + 1, Full);
            Disp_Header ("Right", Indent + 1);
            New_Line;
            Disp_Tree (Get_Right (N), Indent + 1, Full);
            Disp_Header ("Presence", Indent + 1);
            Disp_PSL_Presence_Kind (Get_Presence (N));
            New_Line;
            Disp_Header ("Hash", Indent + 1);
            Disp_Uns32 (Get_Hash (N));
            New_Line;
            Disp_Header ("Hash_Link", Indent + 1);
            New_Line;
            Disp_Tree (Get_Hash_Link (N), Indent + 1, Full);
            null;
         when N_HDL_Expr =>
            if not Full then
               return;
            end if;
            Disp_Header ("Presence", Indent + 1);
            Disp_PSL_Presence_Kind (Get_Presence (N));
            New_Line;
            Disp_Header ("HDL_Node", Indent + 1);
            Disp_HDL_Node (Get_HDL_Node (N));
            New_Line;
            Disp_Header ("HDL_Index", Indent + 1);
            Disp_Int32 (Get_HDL_Index (N));
            New_Line;
            Disp_Header ("Hash", Indent + 1);
            Disp_Uns32 (Get_Hash (N));
            New_Line;
            Disp_Header ("Hash_Link", Indent + 1);
            New_Line;
            Disp_Tree (Get_Hash_Link (N), Indent + 1, Full);
            null;
         when N_False =>
            if not Full then
               return;
            end if;
            null;
         when N_True =>
            if not Full then
               return;
            end if;
            null;
         when N_EOS =>
            if not Full then
               return;
            end if;
            Disp_Header ("HDL_Index", Indent + 1);
            Disp_Int32 (Get_HDL_Index (N));
            New_Line;
            Disp_Header ("Hash", Indent + 1);
            Disp_Uns32 (Get_Hash (N));
            New_Line;
            Disp_Header ("Hash_Link", Indent + 1);
            New_Line;
            Disp_Tree (Get_Hash_Link (N), Indent + 1, Full);
            null;
         when N_Name =>
            Disp_Header ("Identifier", Indent + 1);
            Disp_Identifier (N);
            if not Full then
               return;
            end if;
            Disp_Header ("Decl", Indent + 1);
            New_Line;
            Disp_Tree (Get_Decl (N), Indent + 1, Full);
            null;
         when N_Name_Decl =>
            Disp_Header ("Identifier", Indent + 1);
            Disp_Identifier (N);
            if not Full then
               return;
            end if;
            Disp_Tree (Get_Chain (N), Indent, Full);
            null;
         when N_Number =>
            if not Full then
               return;
            end if;
            Disp_Header ("Value", Indent + 1);
            Disp_Uns32 (Get_Value (N));
            New_Line;
            null;
      end case;
   end Disp_Tree;

   procedure Dump_Tree (N : Node; Full : Boolean := False) is
   begin
      Disp_Tree (N, 0, Full);
   end Dump_Tree;

end PSL.Dump_Tree;
