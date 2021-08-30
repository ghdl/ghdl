--  PSL - Pretty print
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
with PSL.Types; use PSL.Types;
with PSL.Errors; use PSL.Errors;
with Name_Table; use Name_Table;
with Ada.Text_IO; use Ada.Text_IO;

package body PSL.Prints is
   function Get_Priority (N : Node) return Priority is
   begin
      case Get_Kind (N) is
         when N_Never
            | N_Always =>
            return Prio_FL_Invariance;
         when N_Eventually
           | N_Next
           | N_Next_A
           | N_Next_E
           | N_Next_Event
           | N_Next_Event_A
           | N_Next_Event_E =>
            return Prio_FL_Occurence;
         when N_Braced_SERE =>
            return Prio_SERE_Brace;
         when N_Concat_SERE =>
            return Prio_Seq_Concat;
         when N_Fusion_SERE =>
            return Prio_Seq_Fusion;
         when N_Within_SERE =>
            return Prio_Seq_Within;
         when N_Match_And_Seq
           | N_And_Seq =>
            return Prio_Seq_And;
         when N_Or_Seq =>
            return Prio_Seq_Or;
         when N_Until
           | N_Before =>
            return Prio_FL_Bounding;
         when N_Abort
           | N_Sync_Abort
           | N_Async_Abort =>
            return Prio_FL_Abort;
         when N_Or_Prop =>
            return Prio_Seq_Or;
         when N_And_Prop =>
            return Prio_Seq_And;
         when N_Paren_Prop =>
            return Prio_FL_Paren;
         when N_Imp_Seq
           | N_Overlap_Imp_Seq
           | N_Log_Imp_Prop
           | N_Imp_Bool =>
            return Prio_Bool_Imp;
         when N_Name_Decl
           | N_Number
           | N_Inf
           | N_True
           | N_False
           | N_EOS
           | N_HDL_Expr
           | N_HDL_Bool
           | N_Property_Instance
           | N_Sequence_Instance =>
            return Prio_HDL;
         when N_Or_Bool =>
            return Prio_Seq_Or;
         when N_And_Bool =>
            return Prio_Seq_And;
         when N_Not_Bool =>
            return Prio_Bool_Not;
         when N_Star_Repeat_Seq
           | N_Goto_Repeat_Seq
           | N_Equal_Repeat_Seq
           | N_Plus_Repeat_Seq =>
            return Prio_SERE_Repeat;
         when N_Strong =>
            return Prio_Strong;
         when others =>
            Error_Kind ("get_priority", N);
      end case;
   end Get_Priority;

   procedure Print_HDL_Expr (N : HDL_Node) is
   begin
      Put (Image (Get_Identifier (Node (N))));
   end Print_HDL_Expr;

   procedure Dump_Expr (N : Node)
   is
   begin
      case Get_Kind (N) is
         when N_HDL_Expr =>
            if HDL_Expr_Printer = null then
               Put ("Expr");
            else
               HDL_Expr_Printer.all (Get_HDL_Node (N));
            end if;
         when N_True =>
            Put ("TRUE");
         when N_False =>
            Put ("FALSE");
         when N_Not_Bool =>
            Put ("!");
            Dump_Expr (Get_Boolean (N));
         when N_And_Bool =>
            Put ("(");
            Dump_Expr (Get_Left (N));
            Put (" && ");
            Dump_Expr (Get_Right (N));
            Put (")");
         when N_Or_Bool =>
            Put ("(");
            Dump_Expr (Get_Left (N));
            Put (" || ");
            Dump_Expr (Get_Right (N));
            Put (")");
         when others =>
            PSL.Errors.Error_Kind ("dump_expr", N);
      end case;
   end Dump_Expr;

   procedure Print_Expr (N : Node; Parent_Prio : Priority := Prio_Lowest)
   is
      Prio : Priority;
   begin
      if N = Null_Node then
         Put (".");
         return;
      end if;
      Prio := Get_Priority (N);
      if Prio < Parent_Prio then
         Put ("(");
      end if;
      case Get_Kind (N) is
         when N_Number =>
            declare
               Str : constant String := Uns32'Image (Get_Value (N));
            begin
               Put (Str (2 .. Str'Last));
            end;
         when N_Inf =>
            Put ("inf");
         when N_Name_Decl =>
            Put (Image (Get_Identifier (N)));
         when N_HDL_Expr
           | N_HDL_Bool =>
            if HDL_Expr_Printer = null then
               Put ("HDL_Expr");
            else
               HDL_Expr_Printer.all (Get_HDL_Node (N));
            end if;
            --  FIXME: this is true only when using the scanner.
            --  Print_Expr (Node (Get_HDL_Node (N)));
         when N_True =>
            Put ("TRUE");
         when N_False =>
            Put ("FALSE");
         when N_EOS =>
            Put ("EOS");
         when N_Not_Bool =>
            Put ("!");
            Print_Expr (Get_Boolean (N), Prio);
         when N_And_Bool =>
            Print_Expr (Get_Left (N), Prio);
            Put (" && ");
            Print_Expr (Get_Right (N), Prio);
         when N_Or_Bool =>
            Print_Expr (Get_Left (N), Prio);
            Put (" || ");
            Print_Expr (Get_Right (N), Prio);
         when N_Imp_Bool =>
            Print_Expr (Get_Left (N), Prio);
            Put (" -> ");
            Print_Expr (Get_Right (N), Prio);
         when others =>
            Error_Kind ("print_expr", N);
      end case;
      if Prio < Parent_Prio then
         Put (")");
      end if;
   end Print_Expr;

   procedure Print_Count (N : Node) is
      B : Node;
   begin
      B := Get_Low_Bound (N);
      if B = Null_Node then
         return;
      end if;
      Print_Expr (B);
      B := Get_High_Bound (N);
      if B = Null_Node then
         return;
      end if;
      Put (":");
      Print_Expr (B);
   end Print_Count;

   procedure Print_Binary_Sequence (Name : String; N : Node; Prio : Priority)
   is
   begin
      Print_Sequence (Get_Left (N), Prio);
      Put (Name);
      Print_Sequence (Get_Right (N), Prio);
   end Print_Binary_Sequence;

   procedure Print_Repeat_Sequence (Name : String; N : Node) is
      S : Node;
   begin
      S := Get_Sequence (N);
      if S /= Null_Node then
         Print_Sequence (S, Prio_SERE_Repeat);
      end if;
      Put (Name);
      Print_Count (N);
      Put ("]");
   end Print_Repeat_Sequence;

   procedure Print_Sequence (Seq : Node; Parent_Prio : Priority := Prio_Lowest)
   is
      Prio : constant Priority := Get_Priority (Seq);
      Add_Paren : constant Boolean := Prio < Parent_Prio
        or else Parent_Prio <= Prio_FL_Paren;
   begin
      if Add_Paren then
         Put ("{");
      end if;
      case Get_Kind (Seq) is
         when N_Braced_SERE =>
            Put ("{");
            Print_Sequence (Get_SERE (Seq), Prio_Lowest);
            Put ("}");
         when N_Concat_SERE =>
            Print_Binary_Sequence (";", Seq, Prio);
         when N_Fusion_SERE =>
            Print_Binary_Sequence (":", Seq, Prio);
         when N_Within_SERE =>
            Print_Binary_Sequence (" within ", Seq, Prio);
         when N_Match_And_Seq =>
            Print_Binary_Sequence (" && ", Seq, Prio);
         when N_Or_Seq =>
            Print_Binary_Sequence (" | ", Seq, Prio);
         when N_And_Seq =>
            Print_Binary_Sequence (" & ", Seq, Prio);
         when N_Star_Repeat_Seq =>
            Print_Repeat_Sequence ("[*", Seq);
         when N_Goto_Repeat_Seq =>
            Print_Repeat_Sequence ("[->", Seq);
         when N_Equal_Repeat_Seq =>
            Print_Repeat_Sequence ("[=", Seq);
         when N_Plus_Repeat_Seq =>
            Print_Sequence (Get_Sequence (Seq), Prio);
            Put ("[+]");
         when N_Booleans
           | N_Name_Decl =>
            Print_Expr (Seq);
         when N_Sequence_Instance =>
            Put (Image (Get_Identifier (Get_Declaration (Seq))));
         when others =>
            Error_Kind ("print_sequence", Seq);
      end case;
      if Add_Paren then
         Put ("}");
      end if;
   end Print_Sequence;

   procedure Print_Binary_Property (Name : String; N : Node; Prio : Priority)
   is
   begin
      Print_Property (Get_Left (N), Prio);
      Put (Name);
      Print_Property (Get_Right (N), Prio);
   end Print_Binary_Property;

   procedure Print_Binary_Property_SI (Name : String;
                                       N : Node; Prio : Priority)
   is
   begin
      Print_Property (Get_Left (N), Prio);
      Put (Name);
      if Get_Strong_Flag (N) then
         Put ('!');
      end if;
      if Get_Inclusive_Flag (N) then
         Put ('_');
      end if;
      Put (' ');
      Print_Property (Get_Right (N), Prio);
   end Print_Binary_Property_SI;

   procedure Print_Range_Property (Name : String; N : Node) is
   begin
      Put (Name);
      Put ("[");
      Print_Count (N);
      Put ("](");
      Print_Property (Get_Property (N), Prio_FL_Paren);
      Put (")");
   end Print_Range_Property;

   procedure Print_Boolean_Range_Property (Name : String; N : Node) is
   begin
      Put (Name);
      Put ("(");
      Print_Expr (Get_Boolean (N));
      Put (")[");
      Print_Count (N);
      Put ("](");
      Print_Property (Get_Property (N), Prio_FL_Paren);
      Put (")");
   end Print_Boolean_Range_Property;

   procedure Print_Abort_Property
     (Tok : String; Prop : Node; Prio : Priority) is
   begin
      Print_Property (Get_Property (Prop), Prio);
      Put (' ');
      Put (Tok);
      Put (' ');
      Print_Expr (Get_Boolean (Prop));
   end Print_Abort_Property;

   procedure Print_Property (Prop : Node;
                             Parent_Prio : Priority := Prio_Lowest)
   is
      Prio : constant Priority := Get_Priority (Prop);
   begin
      if Prio < Parent_Prio then
         Put ("(");
      end if;
      case Get_Kind (Prop) is
         when N_Never =>
            Put ("never ");
            Print_Property (Get_Property (Prop), Prio);
         when N_Always =>
            Put ("always (");
            Print_Property (Get_Property (Prop), Prio);
            Put (")");
         when N_Eventually =>
            Put ("eventually! (");
            Print_Property (Get_Property (Prop), Prio);
            Put (")");
         when N_Strong =>
            Print_Property (Get_Property (Prop), Prio);
            Put ("!");
         when N_Next =>
            Put ("next");
--              if Get_Strong_Flag (Prop) then
--                 Put ('!');
--              end if;
            Put (" (");
            Print_Property (Get_Property (Prop), Prio);
            Put (")");
         when N_Next_A =>
            Print_Range_Property ("next_a", Prop);
         when N_Next_E =>
            Print_Range_Property ("next_e", Prop);
         when N_Next_Event =>
            Put ("next_event");
            Put ("(");
            Print_Expr (Get_Boolean (Prop));
            Put (")(");
            Print_Property (Get_Property (Prop), Prio);
            Put (")");
         when N_Next_Event_A =>
            Print_Boolean_Range_Property ("next_event_a", Prop);
         when N_Next_Event_E =>
            Print_Boolean_Range_Property ("next_event_e", Prop);
         when N_Until =>
            Print_Binary_Property_SI (" until", Prop, Prio);
         when N_Abort =>
            Print_Abort_Property ("abort", Prop, Prio);
         when N_Sync_Abort =>
            Print_Abort_Property ("sync_abort", Prop, Prio);
         when N_Async_Abort =>
            Print_Abort_Property ("async_abort", Prop, Prio);
         when N_Before =>
            Print_Binary_Property_SI (" before", Prop, Prio);
         when N_Or_Prop =>
            if True then
               Print_Binary_Property (" or ", Prop, Prio);
            else
               Print_Binary_Property (" || ", Prop, Prio);
            end if;
         when N_And_Prop =>
            if True then
               Print_Binary_Property (" and ", Prop, Prio);
            else
               Print_Binary_Property (" && ", Prop, Prio);
            end if;
         when N_Paren_Prop =>
            Put ("(");
            Print_Property (Get_Property (Prop), Prio);
            Put (")");
         when N_Imp_Seq =>
            Print_Property (Get_Sequence (Prop), Prio);
            Put (" |=> ");
            Print_Property (Get_Property (Prop), Prio);
         when N_Overlap_Imp_Seq =>
            Print_Property (Get_Sequence (Prop), Prio);
            Put (" |-> ");
            Print_Property (Get_Property (Prop), Prio);
         when N_Log_Imp_Prop =>
            Print_Binary_Property (" -> ", Prop, Prio);
         when N_Booleans
           | N_Name_Decl =>
            Print_Expr (Prop);
         when N_Sequences =>
            Print_Sequence (Prop, Parent_Prio);
         when N_Property_Instance =>
            Put (Image (Get_Identifier (Get_Declaration (Prop))));
         when N_EOS =>
            Put ("EOS");
         when others =>
            Error_Kind ("print_property", Prop);
      end case;
      if Prio < Parent_Prio then
         Put (")");
      end if;
   end Print_Property;

   procedure Print_Assert (N : Node) is
      Label : Name_Id;
   begin
      Put ("  ");
      Label := Get_Label (N);
      if Label /= Null_Identifier then
         Put (Image (Label));
         Put (": ");
      end if;
      Put ("assert ");
      Print_Property (Get_Property (N));
      Put_Line (";");
   end Print_Assert;

   procedure Print_Property_Declaration (N : Node) is
   begin
      Put ("  ");
      Put ("property ");
      Put (Image (Get_Identifier (N)));
      Put (" = ");
      Print_Property (Get_Property (N));
      Put_Line (";");
   end Print_Property_Declaration;

   procedure Print_Unit (Unit : Node) is
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
            when N_Name_Decl =>
               null;
            when N_Assert_Directive =>
               Print_Assert (Item);
            when N_Property_Declaration =>
               Print_Property_Declaration (Item);
            when others =>
               Error_Kind ("disp_unit", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
      Put_Line ("}");
   end Print_Unit;
end PSL.Prints;
