--  PSL - Printer
--  Copyright (C) 2002-2016 Tristan Gingold
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
with PSL.Errors; use PSL.Errors;
with PSL.Prints;
with Ada.Text_IO; use Ada.Text_IO;
with Name_Table; use Name_Table;

package body PSL.Tprint is
   procedure Disp_Expr (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Number =>
            declare
               Str : constant String := Uns32'Image (Get_Value (N));
            begin
               Put (Str (2 .. Str'Last));
            end;
         when others =>
            Error_Kind ("disp_expr", N);
      end case;
   end Disp_Expr;

   procedure Disp_Count (N : Node) is
      B : Node;
   begin
      B := Get_Low_Bound (N);
      if B = Null_Node then
         return;
      end if;
      Disp_Expr (B);
      B := Get_High_Bound (N);
      if B = Null_Node then
         return;
      end if;
      Put (":");
      Disp_Expr (B);
   end Disp_Count;

   procedure Put_Node (Prefix : String; Name : String) is
   begin
      Put (Prefix);
      Put ("-+ ");
      Put (Name);
   end Put_Node;

   procedure Put_Node_Line (Prefix : String; Name : String) is
   begin
      Put_Node (Prefix, Name);
      New_Line;
   end Put_Node_Line;

   function Down (Str : String) return String is
      L : constant Natural := Str'Last;
   begin
      if Str (L) = '\' then
         return Str (Str'First .. L - 1) & "  \";
      elsif Str (L) = '/' then
         return Str (Str'First .. L - 1) & "| \";
      else
         raise Program_Error;
      end if;
   end Down;

   function Up (Str : String) return String is
      L : constant Natural := Str'Last;
   begin
      if Str (L) = '/' then
         return Str (Str'First .. L - 1) & "  /";
      elsif Str (L) = '\' then
         return Str (Str'First .. L - 1) & "| /";
      else
         raise Program_Error;
      end if;
   end Up;

   procedure Disp_Repeat_Sequence (Prefix : String; Name : String; N : Node) is
      S : Node;
   begin
      Put_Node (Prefix, Name);
      Disp_Count (N);
      Put_Line ("]");
      S := Get_Sequence (N);
      if S /= Null_Node then
         Disp_Property (Down (Prefix), S);
      end if;
   end Disp_Repeat_Sequence;

   procedure Disp_Binary_Sequence (Prefix : String; Name : String; N : Node) is
   begin
      Disp_Property (Up (Prefix), Get_Left (N));
      Put_Node_Line (Prefix, Name);
      Disp_Property (Down (Prefix), Get_Right (N));
   end Disp_Binary_Sequence;

   procedure Disp_Range_Property (Prefix : String; Name : String; N : Node) is
   begin
      Put_Node (Prefix, Name);
      Put ("[");
      Disp_Count (N);
      Put_Line ("]");
      Disp_Property (Down (Prefix), Get_Property (N));
   end Disp_Range_Property;

   procedure Disp_Boolean_Range_Property (Prefix : String;
                                          Name : String; N : Node) is
   begin
      Disp_Property (Up (Prefix), Get_Boolean (N));
      Put_Node (Prefix, Name);
      Put ("[");
      Disp_Count (N);
      Put_Line ("]");
      Disp_Property (Down (Prefix), Get_Property (N));
   end Disp_Boolean_Range_Property;

   procedure Disp_Property (Prefix : String; Prop : Node) is
   begin
      case Get_Kind (Prop) is
         when N_Never =>
            Put_Node_Line (Prefix, "never");
            Disp_Property (Down (Prefix), Get_Property (Prop));
         when N_Always =>
            Put_Node_Line (Prefix, "always");
            Disp_Property (Down (Prefix), Get_Property (Prop));
         when N_Eventually =>
            Put_Node_Line (Prefix, "eventually!");
            Disp_Property (Down (Prefix), Get_Property (Prop));
         when N_Next =>
            Put_Node_Line (Prefix, "next");
--              if Get_Strong_Flag (Prop) then
--                 Put ('!');
--              end if;
            Disp_Property (Down (Prefix), Get_Property (Prop));
         when N_Next_A =>
            Disp_Range_Property (Prefix, "next_a", Prop);
         when N_Next_E =>
            Disp_Range_Property (Prefix, "next_e", Prop);
         when N_Next_Event =>
            Disp_Property (Up (Prefix), Get_Boolean (Prop));
            Put_Node_Line (Prefix, "next_event");
            Disp_Property (Down (Prefix), Get_Property (Prop));
         when N_Next_Event_A =>
            Disp_Boolean_Range_Property (Prefix, "next_event_a", Prop);
         when N_Next_Event_E =>
            Disp_Boolean_Range_Property (Prefix, "next_event_e", Prop);
         when N_Braced_SERE =>
            Put_Node_Line (Prefix, "{} (braced_SERE)");
            Disp_Property (Down (Prefix), Get_SERE (Prop));
         when N_Concat_SERE =>
            Disp_Binary_Sequence (Prefix, "; (concat)", Prop);
         when N_Fusion_SERE =>
            Disp_Binary_Sequence (Prefix, ": (fusion)", Prop);
         when N_Within_SERE =>
            Disp_Binary_Sequence (Prefix, "within", Prop);
         when N_Match_And_Seq =>
            Disp_Binary_Sequence (Prefix, "&& (sequence matching len)", Prop);
         when N_Or_Seq =>
            Disp_Binary_Sequence (Prefix, "| (sequence or)", Prop);
         when N_And_Seq =>
            Disp_Binary_Sequence (Prefix, "& (sequence and)", Prop);
         when N_Imp_Seq =>
            Disp_Property (Up (Prefix), Get_Sequence (Prop));
            Put_Node_Line (Prefix, "|=> (sequence implication)");
            Disp_Property (Down (Prefix), Get_Property (Prop));
         when N_Overlap_Imp_Seq =>
            Disp_Property (Up (Prefix), Get_Sequence (Prop));
            Put_Node_Line (Prefix, "|->");
            Disp_Property (Down (Prefix), Get_Property (Prop));
         when N_Or_Prop =>
            Disp_Binary_Sequence (Prefix, "|| (property or)", Prop);
         when N_And_Prop =>
            Disp_Binary_Sequence (Prefix, "&& (property and)", Prop);
         when N_Log_Imp_Prop =>
            Disp_Binary_Sequence (Prefix, "-> (property impliciation)", Prop);
         when N_Until =>
            Disp_Binary_Sequence (Prefix, "until", Prop);
         when N_Before =>
            Disp_Binary_Sequence (Prefix, "before", Prop);
         when N_Abort =>
            Disp_Property (Up (Prefix), Get_Property (Prop));
            Put_Node_Line (Prefix, "abort");
            Disp_Property (Down (Prefix), Get_Boolean (Prop));
         when N_Not_Bool =>
            Put_Node_Line (Prefix, "! (boolean not)");
            Disp_Property (Down (Prefix), Get_Boolean (Prop));
         when N_Or_Bool =>
            Disp_Binary_Sequence (Prefix, "|| (boolean or)", Prop);
         when N_And_Bool =>
            Disp_Binary_Sequence (Prefix, "&& (boolean and)", Prop);
         when N_Name_Decl =>
            Put_Node_Line (Prefix,
                           "Name_Decl: " & Image (Get_Identifier (Prop)));
         when N_Name =>
            Put_Node_Line (Prefix, "Name: " & Image (Get_Identifier (Prop)));
            Disp_Property (Down (Prefix), Get_Decl (Prop));
         when N_True =>
            Put_Node_Line (Prefix, "TRUE");
         when N_False =>
            Put_Node_Line (Prefix, "FALSE");
         when N_HDL_Expr =>
            Put_Node (Prefix, "HDL_Expr: ");
            PSL.Prints.HDL_Expr_Printer.all (Get_HDL_Node (Prop));
            New_Line;
         when N_Star_Repeat_Seq =>
            Disp_Repeat_Sequence (Prefix, "[*", Prop);
         when N_Goto_Repeat_Seq =>
            Disp_Repeat_Sequence (Prefix, "[->", Prop);
         when N_Equal_Repeat_Seq =>
            Disp_Repeat_Sequence (Prefix, "[=", Prop);
         when N_Plus_Repeat_Seq =>
            Put_Node_Line (Prefix, "[+]");
            Disp_Property (Down (Prefix), Get_Sequence (Prop));
         when others =>
            Error_Kind ("disp_property", Prop);
      end case;
   end Disp_Property;

   procedure Disp_Assert (N : Node) is
      Label : constant Name_Id := Get_Label (N);
   begin
      Put ("  ");
      if Label /= Null_Identifier then
         Put (Image (Label));
         Put (": ");
      end if;
      Put_Line ("assert ");
      Disp_Property ("  \", Get_Property (N));
   end Disp_Assert;

   procedure Disp_Unit (Unit : Node) is
      Item : Node;
   begin
      case Get_Kind (Unit) is
         when N_Vunit =>
            Put ("vunit");
         when others =>
            Error_Kind ("disp_unit", Unit);
      end case;
      Put (' ');
      Put (Image (Get_Identifier (Unit)));
      Put_Line (" {");
      Item := Get_Item_Chain (Unit);
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when N_Assert_Directive =>
               Disp_Assert (Item);
            when N_Name_Decl =>
               null;
            when others =>
               Error_Kind ("disp_unit", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
      Put_Line ("}");
   end Disp_Unit;
end PSL.Tprint;
