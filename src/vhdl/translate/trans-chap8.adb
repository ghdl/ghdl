--  Iir to ortho translator.
--  Copyright (C) 2002 - 2014 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

with Ada.Text_IO;
with Std_Names;
with Errorout; use Errorout;
with Iir_Chains;
with Canon;
with Evaluation; use Evaluation;
with Std_Package; use Std_Package;
with Iirs_Utils; use Iirs_Utils;
with Trans.Chap2;
with Trans.Chap3;
with Trans.Chap4;
with Trans.Chap6;
with Trans.Chap7;
with Trans.Chap14;
with Trans_Decls; use Trans_Decls;
with Translation; use Translation;
with Trans.Helpers2; use Trans.Helpers2;
with Trans.Foreach_Non_Composite;

package body Trans.Chap8 is
   use Trans.Helpers;

   procedure Translate_Return_Statement (Stmt : Iir_Return_Statement)
   is
      Subprg_Info : constant Ortho_Info_Acc :=
        Get_Info (Chap2.Current_Subprogram);
      Expr        : constant Iir := Get_Expression (Stmt);
      Ret_Type    : Iir;
      Ret_Info    : Type_Info_Acc;

      procedure Gen_Return is
      begin
         if Subprg_Info.Subprg_Exit /= O_Snode_Null then
            New_Exit_Stmt (Subprg_Info.Subprg_Exit);
         else
            New_Return_Stmt;
         end if;
      end Gen_Return;

      procedure Gen_Return_Value (Val : O_Enode) is
      begin
         if Subprg_Info.Subprg_Exit /= O_Snode_Null then
            New_Assign_Stmt (New_Obj (Subprg_Info.Subprg_Result), Val);
            New_Exit_Stmt (Subprg_Info.Subprg_Exit);
         else
            New_Return_Stmt (Val);
         end if;
      end Gen_Return_Value;
   begin
      if Expr = Null_Iir then
         --  Return in a procedure.
         Gen_Return;
         return;
      end if;

      --  Return in a function.
      Ret_Type := Get_Return_Type (Chap2.Current_Subprogram);
      Ret_Info := Get_Info (Ret_Type);
      case Ret_Info.Type_Mode is
         when Type_Mode_Scalar =>
            --  * if the return type is scalar, simply returns.
            declare
               V : O_Dnode;
               R : O_Enode;
            begin
               --  Always uses a temporary in case of the return expression
               --  uses secondary stack.
               --  FIXME: don't use the temp if not required.
               R := Chap7.Translate_Expression (Expr, Ret_Type);
               if Has_Stack2_Mark
                 or else Chap3.Need_Range_Check (Expr, Ret_Type)
               then
                  V := Create_Temp (Ret_Info.Ortho_Type (Mode_Value));
                  New_Assign_Stmt (New_Obj (V), R);
                  Stack2_Release;
                  Chap3.Check_Range (V, Expr, Ret_Type, Expr);
                  Gen_Return_Value (New_Obj_Value (V));
               else
                  Gen_Return_Value (R);
               end if;
            end;
         when Type_Mode_Acc =>
            --  * access: thin and no range.
            declare
               Res : O_Enode;
            begin
               Res := Chap7.Translate_Expression (Expr, Ret_Type);
               Gen_Return_Value (Res);
            end;
         when Type_Mode_Fat_Array =>
            --  * if the return type is unconstrained: allocate an area from
            --    the secondary stack, copy it to the area, and fill the fat
            --    pointer.
            --  Evaluate the result.
            declare
               Val  : Mnode;
               Area : Mnode;
            begin
               Area := Dp2M (Subprg_Info.Res_Interface,
                             Ret_Info, Mode_Value);
               Val := Stabilize
                 (E2M (Chap7.Translate_Expression (Expr, Ret_Type),
                  Ret_Info, Mode_Value));
               Chap3.Translate_Object_Allocation
                 (Area, Alloc_Return, Ret_Type,
                  Chap3.Get_Array_Bounds (Val));
               Chap3.Translate_Object_Copy (Area, M2Addr (Val), Ret_Type);
               Gen_Return;
            end;
         when Type_Mode_Record
            | Type_Mode_Array
            | Type_Mode_Fat_Acc =>
            --  * if the return type is a constrained composite type, copy
            --    it to the result area.
            --  Create a temporary area so that if the expression use
            --  stack2, it will be freed before the return (otherwise,
            --  the stack area will be lost).
            declare
               V : Mnode;
            begin
               Open_Temp;
               V := Dp2M (Subprg_Info.Res_Interface, Ret_Info, Mode_Value);
               Chap3.Translate_Object_Copy
                 (V, Chap7.Translate_Expression (Expr, Ret_Type), Ret_Type);
               Close_Temp;
               Gen_Return;
            end;
         when Type_Mode_File =>
            --  FIXME: Is it possible ?
            Error_Kind ("translate_return_statement", Ret_Type);
         when Type_Mode_Unknown
            | Type_Mode_Protected =>
            raise Internal_Error;
      end case;
   end Translate_Return_Statement;

   procedure Translate_If_Statement (Stmt : Iir)
   is
      Blk         : O_If_Block;
      Else_Clause : Iir;
   begin
      Start_If_Stmt
        (Blk, Chap7.Translate_Expression (Get_Condition (Stmt)));

      Translate_Statements_Chain (Get_Sequential_Statement_Chain (Stmt));

      Else_Clause := Get_Else_Clause (Stmt);
      if Else_Clause /= Null_Iir then
         New_Else_Stmt (Blk);
         if Get_Condition (Else_Clause) = Null_Iir then
            Translate_Statements_Chain
              (Get_Sequential_Statement_Chain (Else_Clause));
         else
            Open_Temp;
            Translate_If_Statement (Else_Clause);
            Close_Temp;
         end if;
      end if;
      Finish_If_Stmt (Blk);
   end Translate_If_Statement;

   function Get_Range_Ptr_Field_Value (O_Range : O_Lnode; Field : O_Fnode)
                                          return O_Enode
   is
   begin
      return New_Value (New_Selected_Element
                        (New_Access_Element (New_Value (O_Range)), Field));
   end Get_Range_Ptr_Field_Value;

   --  Inc or dec ITERATOR according to DIR.
   procedure Gen_Update_Iterator (Iterator : O_Dnode;
                                  Dir      : Iir_Direction;
                                  Val      : Unsigned_64;
                                  Itype    : Iir)
   is
      Op        : ON_Op_Kind;
      Base_Type : Iir;
      V         : O_Enode;
   begin
      case Dir is
         when Iir_To =>
            Op := ON_Add_Ov;
         when Iir_Downto =>
            Op := ON_Sub_Ov;
      end case;
      Base_Type := Get_Base_Type (Itype);
      case Get_Kind (Base_Type) is
         when Iir_Kind_Integer_Type_Definition =>
            V := New_Lit
              (New_Signed_Literal
                 (Get_Ortho_Type (Base_Type, Mode_Value), Integer_64 (Val)));
         when Iir_Kind_Enumeration_Type_Definition =>
            declare
               List : Iir_List;
            begin
               List := Get_Enumeration_Literal_List (Base_Type);
               --  FIXME: what about type E is ('T') ??
               if Natural (Val) > Get_Nbr_Elements (List) then
                  raise Internal_Error;
               end if;
               V := New_Lit
                 (Get_Ortho_Expr (Get_Nth_Element (List, Natural (Val))));
            end;

         when others =>
            Error_Kind ("gen_update_iterator", Base_Type);
      end case;
      New_Assign_Stmt (New_Obj (Iterator),
                       New_Dyadic_Op (Op, New_Obj_Value (Iterator), V));
   end Gen_Update_Iterator;

   type For_Loop_Data is record
      Iterator               : Iir_Iterator_Declaration;
      Stmt                   : Iir_For_Loop_Statement;
      --  If around the loop, to check if the loop must be executed.
      If_Blk                 : O_If_Block;
      Label_Next, Label_Exit : O_Snode;
      --  Right bound of the iterator, used only if the iterator is a
      --  range expression.
      O_Right                : O_Dnode;
      --  Range variable of the iterator, used only if the iterator is not
      --  a range expression.
      O_Range                : O_Dnode;
   end record;

   procedure Start_For_Loop (Iterator : Iir_Iterator_Declaration;
                             Stmt     : Iir_For_Loop_Statement;
                             Data     : out For_Loop_Data)
   is
      Iter_Type      : Iir;
      Iter_Base_Type : Iir;
      Var_Iter       : Var_Type;
      Constraint     : Iir;
      Cond           : O_Enode;
      Dir            : Iir_Direction;
      Iter_Type_Info : Ortho_Info_Acc;
      Op             : ON_Op_Kind;
   begin
      --  Initialize DATA.
      Data.Iterator := Iterator;
      Data.Stmt := Stmt;

      Iter_Type := Get_Type (Iterator);
      Iter_Base_Type := Get_Base_Type (Iter_Type);
      Iter_Type_Info := Get_Info (Iter_Base_Type);
      Var_Iter := Get_Info (Iterator).Iterator_Var;

      Open_Temp;

      Constraint := Get_Range_Constraint (Iter_Type);
      if Get_Kind (Constraint) = Iir_Kind_Range_Expression then
         New_Assign_Stmt
           (Get_Var (Var_Iter), Chap7.Translate_Range_Expression_Left
            (Constraint, Iter_Base_Type));
         Dir := Get_Direction (Constraint);
         Data.O_Right := Create_Temp
           (Iter_Type_Info.Ortho_Type (Mode_Value));
         New_Assign_Stmt
           (New_Obj (Data.O_Right), Chap7.Translate_Range_Expression_Right
            (Constraint, Iter_Base_Type));
         case Dir is
            when Iir_To =>
               Op := ON_Le;
            when Iir_Downto =>
               Op := ON_Ge;
         end case;
         --  Check for at least one iteration.
         Cond := New_Compare_Op
           (Op, New_Value (Get_Var (Var_Iter)),
            New_Obj_Value (Data.O_Right),
            Ghdl_Bool_Type);
      else
         Data.O_Range := Create_Temp (Iter_Type_Info.T.Range_Ptr_Type);
         New_Assign_Stmt (New_Obj (Data.O_Range),
                          New_Address (Chap7.Translate_Range
                            (Constraint, Iter_Base_Type),
                            Iter_Type_Info.T.Range_Ptr_Type));
         New_Assign_Stmt
           (Get_Var (Var_Iter), Get_Range_Ptr_Field_Value
            (New_Obj (Data.O_Range), Iter_Type_Info.T.Range_Left));
         --  Before starting the loop, check wether there will be at least
         --  one iteration.
         Cond := New_Compare_Op
           (ON_Gt,
            Get_Range_Ptr_Field_Value (New_Obj (Data.O_Range),
              Iter_Type_Info.T.Range_Length),
            New_Lit (Ghdl_Index_0),
            Ghdl_Bool_Type);
      end if;

      Start_If_Stmt (Data.If_Blk, Cond);

      --  Start loop.
      --  There are two blocks: one for the exit, one for the next.
      Start_Loop_Stmt (Data.Label_Exit);
      Start_Loop_Stmt (Data.Label_Next);

      if Stmt /= Null_Iir then
         declare
            Loop_Info : Loop_Info_Acc;
         begin
            Loop_Info := Add_Info (Stmt, Kind_Loop);
            Loop_Info.Label_Exit := Data.Label_Exit;
            Loop_Info.Label_Next := Data.Label_Next;
         end;
      end if;
   end Start_For_Loop;

   procedure Finish_For_Loop (Data : in out For_Loop_Data)
   is
      Cond           : O_Enode;
      If_Blk1        : O_If_Block;
      Iter_Type      : Iir;
      Iter_Base_Type : Iir;
      Iter_Type_Info : Type_Info_Acc;
      Var_Iter       : Var_Type;
      Constraint     : Iir;
      Deep_Rng       : Iir;
      Deep_Reverse   : Boolean;
   begin
      New_Exit_Stmt (Data.Label_Next);
      Finish_Loop_Stmt (Data.Label_Next);

      --  Check end of loop.
      --  Equality is necessary and enough.
      Iter_Type := Get_Type (Data.Iterator);
      Iter_Base_Type := Get_Base_Type (Iter_Type);
      Iter_Type_Info := Get_Info (Iter_Base_Type);
      Var_Iter := Get_Info (Data.Iterator).Iterator_Var;

      Constraint := Get_Range_Constraint (Iter_Type);

      if Get_Kind (Constraint) = Iir_Kind_Range_Expression then
         Cond := New_Obj_Value (Data.O_Right);
      else
         Cond := Get_Range_Ptr_Field_Value
           (New_Obj (Data.O_Range), Iter_Type_Info.T.Range_Right);
      end if;
      Gen_Exit_When (Data.Label_Exit,
                     New_Compare_Op (ON_Eq, New_Value (Get_Var (Var_Iter)),
                       Cond, Ghdl_Bool_Type));

      --  Update the iterator.
      Chap6.Get_Deep_Range_Expression (Iter_Type, Deep_Rng, Deep_Reverse);
      if Deep_Rng /= Null_Iir then
         if Get_Direction (Deep_Rng) = Iir_To xor Deep_Reverse then
            Gen_Update_Iterator
              (Get_Var_Label (Var_Iter), Iir_To, 1, Iter_Base_Type);
         else
            Gen_Update_Iterator
              (Get_Var_Label (Var_Iter), Iir_Downto, 1, Iter_Base_Type);
         end if;
      else
         Start_If_Stmt
           (If_Blk1, New_Compare_Op
              (ON_Eq,
               Get_Range_Ptr_Field_Value (New_Obj (Data.O_Range),
                 Iter_Type_Info.T.Range_Dir),
               New_Lit (Ghdl_Dir_To_Node),
               Ghdl_Bool_Type));
         Gen_Update_Iterator
           (Get_Var_Label (Var_Iter), Iir_To, 1, Iter_Base_Type);
         New_Else_Stmt (If_Blk1);
         Gen_Update_Iterator
           (Get_Var_Label (Var_Iter), Iir_Downto, 1, Iter_Base_Type);
         Finish_If_Stmt (If_Blk1);
      end if;

      Finish_Loop_Stmt (Data.Label_Exit);
      Finish_If_Stmt (Data.If_Blk);
      Close_Temp;

      if Data.Stmt /= Null_Iir then
         Free_Info (Data.Stmt);
      end if;
   end Finish_For_Loop;

   Current_Loop : Iir := Null_Iir;

   procedure Translate_For_Loop_Statement (Stmt : Iir_For_Loop_Statement)
   is
      Iterator       : constant Iir := Get_Parameter_Specification (Stmt);
      Iter_Type      : constant Iir := Get_Type (Iterator);
      Iter_Base_Type : constant Iir := Get_Base_Type (Iter_Type);
      Iter_Type_Info : constant Type_Info_Acc := Get_Info (Iter_Base_Type);
      Data           : For_Loop_Data;
      It_Info        : Ortho_Info_Acc;
      Var_Iter       : Var_Type;
      Prev_Loop      : Iir;
   begin
      Prev_Loop := Current_Loop;
      Current_Loop := Stmt;
      Start_Declare_Stmt;

      Chap3.Translate_Object_Subtype (Iterator, False);

      --  Create info for the iterator.
      It_Info := Add_Info (Iterator, Kind_Iterator);
      Var_Iter := Create_Var
        (Create_Var_Identifier (Iterator),
         Iter_Type_Info.Ortho_Type (Mode_Value),
         O_Storage_Local);
      It_Info.Iterator_Var := Var_Iter;

      Start_For_Loop (Iterator, Stmt, Data);

      Translate_Statements_Chain (Get_Sequential_Statement_Chain (Stmt));

      Finish_For_Loop (Data);

      Finish_Declare_Stmt;

      Free_Info (Iterator);
      Current_Loop := Prev_Loop;
   end Translate_For_Loop_Statement;

   procedure Translate_While_Loop_Statement
     (Stmt : Iir_While_Loop_Statement)
   is
      Info      : Loop_Info_Acc;
      Cond      : Iir;
      Prev_Loop : Iir;
   begin
      Prev_Loop := Current_Loop;
      Current_Loop := Stmt;

      Info := Add_Info (Stmt, Kind_Loop);

      Start_Loop_Stmt (Info.Label_Exit);
      Info.Label_Next := O_Snode_Null;

      Open_Temp;
      Cond := Get_Condition (Stmt);
      if Cond /= Null_Iir then
         Gen_Exit_When
           (Info.Label_Exit,
            New_Monadic_Op (ON_Not, Chap7.Translate_Expression (Cond)));
      end if;
      Close_Temp;

      Translate_Statements_Chain (Get_Sequential_Statement_Chain (Stmt));

      Finish_Loop_Stmt (Info.Label_Exit);
      Free_Info (Stmt);
      Current_Loop := Prev_Loop;
   end Translate_While_Loop_Statement;

   procedure Translate_Exit_Next_Statement (Stmt : Iir)
   is
      Cond       : constant Iir := Get_Condition (Stmt);
      If_Blk     : O_If_Block;
      Info       : Loop_Info_Acc;
      Loop_Label : Iir;
      Loop_Stmt  : Iir;
   begin
      if Cond /= Null_Iir then
         Start_If_Stmt (If_Blk, Chap7.Translate_Expression (Cond));
      end if;

      Loop_Label := Get_Loop_Label (Stmt);
      if Loop_Label = Null_Iir then
         Loop_Stmt := Current_Loop;
      else
         Loop_Stmt := Get_Named_Entity (Loop_Label);
      end if;

      Info := Get_Info (Loop_Stmt);
      case Get_Kind (Stmt) is
         when Iir_Kind_Exit_Statement =>
            New_Exit_Stmt (Info.Label_Exit);
         when Iir_Kind_Next_Statement =>
            if Info.Label_Next /= O_Snode_Null then
               --  For-loop.
               New_Exit_Stmt (Info.Label_Next);
            else
               --  While-loop.
               New_Next_Stmt (Info.Label_Exit);
            end if;
         when others =>
            raise Internal_Error;
      end case;
      if Cond /= Null_Iir then
         Finish_If_Stmt (If_Blk);
      end if;
   end Translate_Exit_Next_Statement;

   procedure Translate_Variable_Aggregate_Assignment
     (Targ : Iir; Targ_Type : Iir; Val : Mnode);

   procedure Translate_Variable_Array_Aggr
     (Targ      : Iir_Aggregate;
      Targ_Type : Iir;
      Val       : Mnode;
      Index     : in out Unsigned_64;
      Dim       : Natural)
   is
      El      : Iir;
      Final   : Boolean;
      El_Type : Iir;
   begin
      Final := Dim = Get_Nbr_Elements (Get_Index_Subtype_List (Targ_Type));
      if Final then
         El_Type := Get_Element_Subtype (Targ_Type);
      end if;
      El := Get_Association_Choices_Chain (Targ);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Choice_By_None =>
               if Final then
                  Translate_Variable_Aggregate_Assignment
                    (Get_Associated_Expr (El), El_Type,
                     Chap3.Index_Base
                       (Val, Targ_Type,
                        New_Lit (New_Unsigned_Literal
                          (Ghdl_Index_Type, Index))));
                  Index := Index + 1;
               else
                  Translate_Variable_Array_Aggr
                    (Get_Associated_Expr (El),
                     Targ_Type, Val, Index, Dim + 1);
               end if;
            when others =>
               Error_Kind ("translate_variable_array_aggr", El);
         end case;
         El := Get_Chain (El);
      end loop;
   end Translate_Variable_Array_Aggr;

   procedure Translate_Variable_Rec_Aggr
     (Targ : Iir_Aggregate; Targ_Type : Iir; Val : Mnode)
   is
      Aggr_El  : Iir;
      El_List  : Iir_List;
      El_Index : Natural;
      Elem     : Iir;
   begin
      El_List := Get_Elements_Declaration_List (Get_Base_Type (Targ_Type));
      El_Index := 0;
      Aggr_El := Get_Association_Choices_Chain (Targ);
      while Aggr_El /= Null_Iir loop
         case Get_Kind (Aggr_El) is
            when Iir_Kind_Choice_By_None =>
               Elem := Get_Nth_Element (El_List, El_Index);
               El_Index := El_Index + 1;
            when Iir_Kind_Choice_By_Name =>
               Elem := Get_Choice_Name (Aggr_El);
            when others =>
               Error_Kind ("translate_variable_rec_aggr", Aggr_El);
         end case;
         Translate_Variable_Aggregate_Assignment
           (Get_Associated_Expr (Aggr_El), Get_Type (Elem),
            Chap6.Translate_Selected_Element (Val, Elem));
         Aggr_El := Get_Chain (Aggr_El);
      end loop;
   end Translate_Variable_Rec_Aggr;

   procedure Translate_Variable_Aggregate_Assignment
     (Targ : Iir; Targ_Type : Iir; Val : Mnode)
   is
      Index : Unsigned_64;
   begin
      if Get_Kind (Targ) = Iir_Kind_Aggregate then
         case Get_Kind (Targ_Type) is
            when Iir_Kinds_Array_Type_Definition =>
               Index := 0;
               Translate_Variable_Array_Aggr
                 (Targ, Targ_Type, Val, Index, 1);
            when Iir_Kind_Record_Type_Definition
               | Iir_Kind_Record_Subtype_Definition =>
               Translate_Variable_Rec_Aggr (Targ, Targ_Type, Val);
            when others =>
               Error_Kind
                 ("translate_variable_aggregate_assignment", Targ_Type);
         end case;
      else
         declare
            Targ_Node : Mnode;
         begin
            Targ_Node := Chap6.Translate_Name (Targ);
            Chap3.Translate_Object_Copy (Targ_Node, M2E (Val), Targ_Type);
         end;
      end if;
   end Translate_Variable_Aggregate_Assignment;

   procedure Translate_Variable_Assignment_Statement
     (Stmt : Iir_Variable_Assignment_Statement)
   is
      Target    : constant Iir := Get_Target (Stmt);
      Targ_Type : constant Iir := Get_Type (Target);
      Expr      : constant Iir := Get_Expression (Stmt);
      Targ_Node : Mnode;
   begin
      if Get_Kind (Target) = Iir_Kind_Aggregate then
         declare
            E    : O_Enode;
            Temp : Mnode;
         begin
            Chap3.Translate_Anonymous_Type_Definition (Targ_Type, True);

            --  Use a temporary variable, to avoid overlap.
            Temp := Create_Temp (Get_Info (Targ_Type));
            Chap4.Allocate_Complex_Object (Targ_Type, Alloc_Stack, Temp);

            E := Chap7.Translate_Expression (Expr, Targ_Type);
            Chap3.Translate_Object_Copy (Temp, E, Targ_Type);
            Translate_Variable_Aggregate_Assignment
              (Target, Targ_Type, Temp);
            return;
         end;
      else
         Targ_Node := Chap6.Translate_Name (Target);
         if Get_Kind (Expr) = Iir_Kind_Aggregate then
            declare
               E : O_Enode;
            begin
               E := Chap7.Translate_Expression (Expr, Targ_Type);
               Chap3.Translate_Object_Copy (Targ_Node, E, Targ_Type);
            end;
         else
            Chap7.Translate_Assign (Targ_Node, Expr, Targ_Type);
         end if;
      end if;
   end Translate_Variable_Assignment_Statement;

   procedure Translate_Report (Stmt : Iir; Subprg : O_Dnode; Level : Iir)
   is
      Expr     : Iir;
      Msg      : O_Enode;
      Severity : O_Enode;
      Assocs   : O_Assoc_List;
      Loc      : O_Dnode;
   begin
      Loc := Chap4.Get_Location (Stmt);
      Expr := Get_Report_Expression (Stmt);
      if Expr = Null_Iir then
         Msg := New_Lit (New_Null_Access (Std_String_Ptr_Node));
      else
         Msg := Chap7.Translate_Expression (Expr, String_Type_Definition);
      end if;
      Expr := Get_Severity_Expression (Stmt);
      if Expr = Null_Iir then
         Severity := New_Lit (Get_Ortho_Expr (Level));
      else
         Severity := Chap7.Translate_Expression (Expr);
      end if;
      --  Do call.
      Start_Association (Assocs, Subprg);
      New_Association (Assocs, Msg);
      New_Association (Assocs, Severity);
      New_Association (Assocs, New_Address (New_Obj (Loc),
                       Ghdl_Location_Ptr_Node));
      New_Procedure_Call (Assocs);
   end Translate_Report;

   --  Return True if the current library unit is part of library IEEE.
   function Is_Within_Ieee_Library return Boolean
   is
      Design_File : Iir;
      Library     : Iir;
   begin
      --  Guard.
      if Current_Library_Unit = Null_Iir then
         return False;
      end if;
      Design_File :=
        Get_Design_File (Get_Design_Unit (Current_Library_Unit));
      Library := Get_Library (Design_File);
      return Get_Identifier (Library) = Std_Names.Name_Ieee;
   end Is_Within_Ieee_Library;

   procedure Translate_Assertion_Statement (Stmt : Iir_Assertion_Statement)
   is
      Expr   : Iir;
      If_Blk : O_If_Block;
      Subprg : O_Dnode;
   begin
      --  Select the procedure to call in case of assertion (so that
      --  assertions within the IEEE library could be ignored).
      if Is_Within_Ieee_Library then
         Subprg := Ghdl_Ieee_Assert_Failed;
      else
         Subprg := Ghdl_Assert_Failed;
      end if;

      Expr := Get_Assertion_Condition (Stmt);
      if Get_Expr_Staticness (Expr) = Locally then
         if Eval_Pos (Expr) = 1 then
            --  Assert TRUE is a noop.
            --  FIXME: generate a noop ?
            return;
         end if;
         Translate_Report (Stmt, Subprg, Severity_Level_Error);
      else
         --  An assertion is reported if the condition is false!
         Start_If_Stmt (If_Blk,
                        New_Monadic_Op (ON_Not,
                          Chap7.Translate_Expression (Expr)));
         --  Note: it is necessary to create a declare block, to avoid bad
         --  order with the if block.
         Open_Temp;
         Translate_Report (Stmt, Subprg, Severity_Level_Error);
         Close_Temp;
         Finish_If_Stmt (If_Blk);
      end if;
   end Translate_Assertion_Statement;

   procedure Translate_Report_Statement (Stmt : Iir_Report_Statement) is
   begin
      Translate_Report (Stmt, Ghdl_Report, Severity_Level_Note);
   end Translate_Report_Statement;

   --  Helper to compare a string choice with the selector.
   function Translate_Simple_String_Choice
     (Expr     : O_Dnode;
      Val      : O_Enode;
      Val_Node : O_Dnode;
      Tinfo    : Type_Info_Acc;
      Func     : Iir)
         return O_Enode
   is
      Assoc     : O_Assoc_List;
      Func_Info : Subprg_Info_Acc;
   begin
      New_Assign_Stmt
        (New_Selected_Element (New_Obj (Val_Node),
         Tinfo.T.Base_Field (Mode_Value)),
         Val);
      Func_Info := Get_Info (Func);
      Start_Association (Assoc, Func_Info.Ortho_Func);
      Subprgs.Add_Subprg_Instance_Assoc (Assoc, Func_Info.Subprg_Instance);
      New_Association (Assoc, New_Obj_Value (Expr));
      New_Association
        (Assoc, New_Address (New_Obj (Val_Node),
         Tinfo.Ortho_Ptr_Type (Mode_Value)));
      return New_Function_Call (Assoc);
   end Translate_Simple_String_Choice;

   --  Helper to evaluate the selector and preparing a choice variable.
   procedure Translate_String_Case_Statement_Common
     (Stmt      : Iir_Case_Statement;
      Expr_Type : out Iir;
      Tinfo     : out Type_Info_Acc;
      Expr_Node : out O_Dnode;
      C_Node    : out O_Dnode)
   is
      Expr      : Iir;
      Base_Type : Iir;
   begin
      --  Translate into if/elsif statements.
      --  FIXME: if the number of literals ** length of the array < 256,
      --   use a case statement.
      Expr := Get_Expression (Stmt);
      Expr_Type := Get_Type (Expr);
      Base_Type := Get_Base_Type (Expr_Type);
      Tinfo := Get_Info (Base_Type);

      --  Translate selector.
      Expr_Node := Create_Temp_Init
        (Tinfo.Ortho_Ptr_Type (Mode_Value),
         Chap7.Translate_Expression (Expr, Base_Type));

      --  Copy the bounds for the choices.
      C_Node := Create_Temp (Tinfo.Ortho_Type (Mode_Value));
      New_Assign_Stmt
        (New_Selected_Element (New_Obj (C_Node),
         Tinfo.T.Bounds_Field (Mode_Value)),
         New_Value_Selected_Acc_Value
           (New_Obj (Expr_Node), Tinfo.T.Bounds_Field (Mode_Value)));
   end Translate_String_Case_Statement_Common;

   --  Translate a string case statement using a dichotomy.
   procedure Translate_String_Case_Statement_Dichotomy
     (Stmt : Iir_Case_Statement)
   is
      --  Selector.
      Expr_Type : Iir;
      Tinfo     : Type_Info_Acc;
      Expr_Node : O_Dnode;
      C_Node    : O_Dnode;

      Choices_Chain : Iir;
      Choice        : Iir;
      Has_Others    : Boolean;
      Func          : Iir;

      --  Number of non-others choices.
      Nbr_Choices : Natural;
      --  Number of associations.
      Nbr_Assocs  : Natural;

      Info        : Ortho_Info_Acc;
      First, Last : Ortho_Info_Acc;
      Sel_Length  : Iir_Int64;

      --  Dichotomy table (table of choices).
      String_Type     : O_Tnode;
      Table_Base_Type : O_Tnode;
      Table_Type      : O_Tnode;
      Table           : O_Dnode;
      List            : O_Array_Aggr_List;
      Table_Cst       : O_Cnode;

      --  Association table.
      --  Indexed by the choice, returns an index to the associated
      --   statement list.
      --  Could be replaced by jump table.
      Assoc_Table_Base_Type : O_Tnode;
      Assoc_Table_Type      : O_Tnode;
      Assoc_Table           : O_Dnode;
   begin
      Choices_Chain := Get_Case_Statement_Alternative_Chain (Stmt);

      --  Count number of choices and number of associations.
      Nbr_Choices := 0;
      Nbr_Assocs := 0;
      Choice := Choices_Chain;
      First := null;
      Last := null;
      Has_Others := False;
      while Choice /= Null_Iir loop
         case Get_Kind (Choice) is
            when Iir_Kind_Choice_By_Others =>
               Has_Others := True;
               exit;
            when Iir_Kind_Choice_By_Expression =>
               null;
            when others =>
               raise Internal_Error;
         end case;
         if not Get_Same_Alternative_Flag (Choice) then
            Nbr_Assocs := Nbr_Assocs + 1;
         end if;
         Info := Add_Info (Choice, Kind_Str_Choice);
         if First = null then
            First := Info;
         else
            Last.Choice_Chain := Info;
         end if;
         Last := Info;
         Info.Choice_Chain := null;
         Info.Choice_Assoc := Nbr_Assocs - 1;
         Info.Choice_Parent := Choice;
         Info.Choice_Expr := Get_Choice_Expression (Choice);

         Nbr_Choices := Nbr_Choices + 1;
         Choice := Get_Chain (Choice);
      end loop;

      --  Sort choices.
      declare
         procedure Merge_Sort (Head : Ortho_Info_Acc;
                               Nbr  : Natural;
                               Res  : out Ortho_Info_Acc;
                               Next : out Ortho_Info_Acc)
         is
            L, R, L_End, R_End : Ortho_Info_Acc;
            E, Last            : Ortho_Info_Acc;
            Half               : constant Natural := Nbr / 2;
         begin
            --  Sorting less than 2 elements is easy!
            if Nbr < 2 then
               Res := Head;
               if Nbr = 0 then
                  Next := Head;
               else
                  Next := Head.Choice_Chain;
               end if;
               return;
            end if;

            Merge_Sort (Head, Half, L, L_End);
            Merge_Sort (L_End, Nbr - Half, R, R_End);
            Next := R_End;

            --  Merge
            Last := null;
            loop
               if L /= L_End
                 and then
                   (R = R_End
                    or else
                    Compare_String_Literals (L.Choice_Expr, R.Choice_Expr)
                    = Compare_Lt)
               then
                  E := L;
                  L := L.Choice_Chain;
               elsif R /= R_End then
                  E := R;
                  R := R.Choice_Chain;
               else
                  exit;
               end if;
               if Last = null then
                  Res := E;
               else
                  Last.Choice_Chain := E;
               end if;
               Last := E;
            end loop;
            Last.Choice_Chain := R_End;
         end Merge_Sort;
         Next               : Ortho_Info_Acc;
      begin
         Merge_Sort (First, Nbr_Choices, First, Next);
         if Next /= null then
            raise Internal_Error;
         end if;
      end;

      Translate_String_Case_Statement_Common
        (Stmt, Expr_Type, Tinfo, Expr_Node, C_Node);

      --  Generate choices table.
      Sel_Length := Eval_Discrete_Type_Length
        (Get_String_Type_Bound_Type (Expr_Type));
      String_Type := New_Constrained_Array_Type
        (Tinfo.T.Base_Type (Mode_Value),
         New_Unsigned_Literal (Ghdl_Index_Type, Unsigned_64 (Sel_Length)));
      Table_Base_Type := New_Array_Type (String_Type, Ghdl_Index_Type);
      New_Type_Decl (Create_Uniq_Identifier, Table_Base_Type);
      Table_Type := New_Constrained_Array_Type
        (Table_Base_Type,
         New_Unsigned_Literal (Ghdl_Index_Type, Unsigned_64 (Nbr_Choices)));
      New_Type_Decl (Create_Uniq_Identifier, Table_Type);
      New_Const_Decl (Table, Create_Uniq_Identifier, O_Storage_Private,
                      Table_Type);
      Start_Const_Value (Table);
      Start_Array_Aggr (List, Table_Type);
      Info := First;
      while Info /= null loop
         New_Array_Aggr_El (List, Chap7.Translate_Static_Expression
                            (Info.Choice_Expr, Expr_Type));
         Info := Info.Choice_Chain;
      end loop;
      Finish_Array_Aggr (List, Table_Cst);
      Finish_Const_Value (Table, Table_Cst);

      --  Generate assoc table.
      Assoc_Table_Base_Type :=
        New_Array_Type (Ghdl_Index_Type, Ghdl_Index_Type);
      New_Type_Decl (Create_Uniq_Identifier, Assoc_Table_Base_Type);
      Assoc_Table_Type := New_Constrained_Array_Type
        (Assoc_Table_Base_Type,
         New_Unsigned_Literal (Ghdl_Index_Type, Unsigned_64 (Nbr_Choices)));
      New_Type_Decl (Create_Uniq_Identifier, Assoc_Table_Type);
      New_Const_Decl (Assoc_Table, Create_Uniq_Identifier,
                      O_Storage_Private, Assoc_Table_Type);
      Start_Const_Value (Assoc_Table);
      Start_Array_Aggr (List, Assoc_Table_Type);
      Info := First;
      while Info /= null loop
         New_Array_Aggr_El
           (List, New_Unsigned_Literal (Ghdl_Index_Type,
            Unsigned_64 (Info.Choice_Assoc)));
         Info := Info.Choice_Chain;
      end loop;
      Finish_Array_Aggr (List, Table_Cst);
      Finish_Const_Value (Assoc_Table, Table_Cst);

      --  Generate dichotomy code.
      declare
         Var_Lo, Var_Hi, Var_Mid : O_Dnode;
         Var_Cmp                 : O_Dnode;
         Var_Idx                 : O_Dnode;
         Label                   : O_Snode;
         Others_Lit              : O_Cnode;
         If_Blk1, If_Blk2        : O_If_Block;
         Case_Blk                : O_Case_Block;
      begin
         Var_Idx := Create_Temp (Ghdl_Index_Type);

         Start_Declare_Stmt;

         New_Var_Decl (Var_Lo, Wki_Lo, O_Storage_Local, Ghdl_Index_Type);
         New_Var_Decl (Var_Hi, Wki_Hi, O_Storage_Local, Ghdl_Index_Type);
         New_Var_Decl (Var_Mid, Wki_Mid, O_Storage_Local, Ghdl_Index_Type);
         New_Var_Decl (Var_Cmp, Wki_Cmp,
                       O_Storage_Local, Ghdl_Compare_Type);

         New_Assign_Stmt (New_Obj (Var_Lo), New_Lit (Ghdl_Index_0));
         New_Assign_Stmt
           (New_Obj (Var_Hi),
            New_Lit (New_Unsigned_Literal (Ghdl_Index_Type,
                                           Unsigned_64 (Nbr_Choices - 1))));

         Func := Chap7.Find_Predefined_Function
           (Get_Base_Type (Expr_Type), Iir_Predefined_Array_Greater);

         if Has_Others then
            Others_Lit := New_Unsigned_Literal
              (Ghdl_Index_Type, Unsigned_64 (Nbr_Assocs));
         end if;

         Start_Loop_Stmt (Label);
         New_Assign_Stmt
           (New_Obj (Var_Mid),
            New_Dyadic_Op (ON_Div_Ov,
              New_Dyadic_Op (ON_Add_Ov,
                New_Obj_Value (Var_Lo),
                New_Obj_Value (Var_Hi)),
              New_Lit (New_Unsigned_Literal
                (Ghdl_Index_Type, 2))));
         New_Assign_Stmt
           (New_Obj (Var_Cmp),
            Translate_Simple_String_Choice
              (Expr_Node,
               New_Address (New_Indexed_Element (New_Obj (Table),
                 New_Obj_Value (Var_Mid)),
                 Tinfo.T.Base_Ptr_Type (Mode_Value)),
               C_Node, Tinfo, Func));
         Start_If_Stmt
           (If_Blk1,
            New_Compare_Op (ON_Eq,
              New_Obj_Value (Var_Cmp),
              New_Lit (Ghdl_Compare_Eq),
              Ghdl_Bool_Type));
         New_Assign_Stmt
           (New_Obj (Var_Idx),
            New_Value (New_Indexed_Element (New_Obj (Assoc_Table),
              New_Obj_Value (Var_Mid))));
         New_Exit_Stmt (Label);
         Finish_If_Stmt (If_Blk1);

         Start_If_Stmt
           (If_Blk1,
            New_Compare_Op (ON_Eq,
              New_Obj_Value (Var_Cmp),
              New_Lit (Ghdl_Compare_Lt),
              Ghdl_Bool_Type));
         Start_If_Stmt
           (If_Blk2,
            New_Compare_Op (ON_Le,
              New_Obj_Value (Var_Mid),
              New_Obj_Value (Var_Lo),
              Ghdl_Bool_Type));
         if not Has_Others then
            Chap6.Gen_Program_Error (Stmt, Chap6.Prg_Err_Bad_Choice);
         else
            New_Assign_Stmt (New_Obj (Var_Idx), New_Lit (Others_Lit));
            New_Exit_Stmt (Label);
         end if;
         New_Else_Stmt (If_Blk2);
         New_Assign_Stmt (New_Obj (Var_Hi),
                          New_Dyadic_Op (ON_Sub_Ov,
                            New_Obj_Value (Var_Mid),
                            New_Lit (Ghdl_Index_1)));
         Finish_If_Stmt (If_Blk2);

         New_Else_Stmt (If_Blk1);

         Start_If_Stmt
           (If_Blk2,
            New_Compare_Op (ON_Ge,
              New_Obj_Value (Var_Mid),
              New_Obj_Value (Var_Hi),
              Ghdl_Bool_Type));
         if not Has_Others then
            Chap6.Gen_Program_Error (Stmt, Chap6.Prg_Err_No_Choice);
         else
            New_Assign_Stmt (New_Obj (Var_Idx), New_Lit (Others_Lit));
            New_Exit_Stmt (Label);
         end if;
         New_Else_Stmt (If_Blk2);
         New_Assign_Stmt (New_Obj (Var_Lo),
                          New_Dyadic_Op (ON_Add_Ov,
                            New_Obj_Value (Var_Mid),
                            New_Lit (Ghdl_Index_1)));
         Finish_If_Stmt (If_Blk2);

         Finish_If_Stmt (If_Blk1);

         Finish_Loop_Stmt (Label);

         Finish_Declare_Stmt;

         Start_Case_Stmt (Case_Blk, New_Obj_Value (Var_Idx));

         Choice := Choices_Chain;
         while Choice /= Null_Iir loop
            case Get_Kind (Choice) is
               when Iir_Kind_Choice_By_Others =>
                  Start_Choice (Case_Blk);
                  New_Expr_Choice (Case_Blk, Others_Lit);
                  Finish_Choice (Case_Blk);
                  Translate_Statements_Chain
                    (Get_Associated_Chain (Choice));
               when Iir_Kind_Choice_By_Expression =>
                  if not Get_Same_Alternative_Flag (Choice) then
                     Start_Choice (Case_Blk);
                     New_Expr_Choice
                       (Case_Blk,
                        New_Unsigned_Literal
                          (Ghdl_Index_Type,
                           Unsigned_64 (Get_Info (Choice).Choice_Assoc)));
                     Finish_Choice (Case_Blk);
                     Translate_Statements_Chain
                       (Get_Associated_Chain (Choice));
                  end if;
                  Free_Info (Choice);
               when others =>
                  raise Internal_Error;
            end case;
            Choice := Get_Chain (Choice);
         end loop;

         Start_Choice (Case_Blk);
         New_Default_Choice (Case_Blk);
         Finish_Choice (Case_Blk);
         Chap6.Gen_Program_Error (Stmt, Chap6.Prg_Err_No_Choice);

         Finish_Case_Stmt (Case_Blk);
      end;
   end Translate_String_Case_Statement_Dichotomy;

   --  Case statement whose expression is an unidim array.
   --  Translate into if/elsif statements (linear search).
   procedure Translate_String_Case_Statement_Linear
     (Stmt : Iir_Case_Statement)
   is
      Expr_Type : Iir;
      --  Node containing the address of the selector.
      Expr_Node : O_Dnode;
      --  Node containing the current choice.
      Val_Node  : O_Dnode;
      Tinfo     : Type_Info_Acc;

      Cond_Var : O_Dnode;

      Func : Iir;

      procedure Translate_String_Choice (Choice : Iir)
      is
         Cond       : O_Enode;
         If_Blk     : O_If_Block;
         Stmt_Chain : Iir;
         First      : Boolean;
         Ch         : Iir;
         Ch_Expr    : Iir;
      begin
         if Choice = Null_Iir then
            return;
         end if;

         First := True;
         Stmt_Chain := Get_Associated_Chain (Choice);
         Ch := Choice;
         loop
            case Get_Kind (Ch) is
               when Iir_Kind_Choice_By_Expression =>
                  Ch_Expr := Get_Choice_Expression (Ch);
                  Cond := Translate_Simple_String_Choice
                    (Expr_Node,
                     Chap7.Translate_Expression (Ch_Expr,
                       Get_Type (Ch_Expr)),
                     Val_Node, Tinfo, Func);
               when Iir_Kind_Choice_By_Others =>
                  Translate_Statements_Chain (Stmt_Chain);
                  return;
               when others =>
                  Error_Kind ("translate_string_choice", Ch);
            end case;
            if not First then
               New_Assign_Stmt
                 (New_Obj (Cond_Var),
                  New_Dyadic_Op (ON_Or, New_Obj_Value (Cond_Var), Cond));
            end if;
            Ch := Get_Chain (Ch);
            exit when Ch = Null_Iir;
            exit when not Get_Same_Alternative_Flag (Ch);
            exit when Get_Associated_Chain (Ch) /= Null_Iir;
            if First then
               New_Assign_Stmt (New_Obj (Cond_Var), Cond);
               First := False;
            end if;
         end loop;
         if not First then
            Cond := New_Obj_Value (Cond_Var);
         end if;
         Start_If_Stmt (If_Blk, Cond);
         Translate_Statements_Chain (Stmt_Chain);
         New_Else_Stmt (If_Blk);
         Translate_String_Choice (Ch);
         Finish_If_Stmt (If_Blk);
      end Translate_String_Choice;
   begin
      Translate_String_Case_Statement_Common
        (Stmt, Expr_Type, Tinfo, Expr_Node, Val_Node);

      Func := Chap7.Find_Predefined_Function
        (Get_Base_Type (Expr_Type), Iir_Predefined_Array_Equality);

      Cond_Var := Create_Temp (Std_Boolean_Type_Node);

      Translate_String_Choice (Get_Case_Statement_Alternative_Chain (Stmt));
   end Translate_String_Case_Statement_Linear;

   procedure Translate_Case_Choice
     (Choice : Iir; Choice_Type : Iir; Blk : in out O_Case_Block)
   is
      Expr : Iir;
   begin
      case Get_Kind (Choice) is
         when Iir_Kind_Choice_By_Others =>
            New_Default_Choice (Blk);
         when Iir_Kind_Choice_By_Expression =>
            Expr := Get_Choice_Expression (Choice);
            New_Expr_Choice
              (Blk, Chap7.Translate_Static_Expression (Expr, Choice_Type));
         when Iir_Kind_Choice_By_Range =>
            declare
               H, L : Iir;
            begin
               Expr := Get_Choice_Range (Choice);
               Get_Low_High_Limit (Expr, L, H);
               New_Range_Choice
                 (Blk,
                  Chap7.Translate_Static_Expression (L, Choice_Type),
                  Chap7.Translate_Static_Expression (H, Choice_Type));
            end;
         when others =>
            Error_Kind ("translate_case_choice", Choice);
      end case;
   end Translate_Case_Choice;

   procedure Translate_Case_Statement (Stmt : Iir_Case_Statement)
   is
      Expr       : Iir;
      Expr_Type  : Iir;
      Case_Blk   : O_Case_Block;
      Choice     : Iir;
      Stmt_Chain : Iir;
   begin
      Expr := Get_Expression (Stmt);
      Expr_Type := Get_Type (Expr);
      if Get_Kind (Expr_Type) = Iir_Kind_Array_Subtype_Definition then
         declare
            Nbr_Choices : Natural := 0;
            Choice      : Iir;
         begin
            Choice := Get_Case_Statement_Alternative_Chain (Stmt);
            while Choice /= Null_Iir loop
               case Get_Kind (Choice) is
                  when Iir_Kind_Choice_By_Others =>
                     exit;
                  when Iir_Kind_Choice_By_Expression =>
                     null;
                  when others =>
                     raise Internal_Error;
               end case;
               Nbr_Choices := Nbr_Choices + 1;
               Choice := Get_Chain (Choice);
            end loop;

            if Nbr_Choices < 3 then
               Translate_String_Case_Statement_Linear (Stmt);
            else
               Translate_String_Case_Statement_Dichotomy (Stmt);
            end if;
         end;
         return;
      end if;
      Start_Case_Stmt (Case_Blk, Chap7.Translate_Expression (Expr));
      Choice := Get_Case_Statement_Alternative_Chain (Stmt);
      while Choice /= Null_Iir loop
         Start_Choice (Case_Blk);
         Stmt_Chain := Get_Associated_Chain (Choice);
         loop
            Translate_Case_Choice (Choice, Expr_Type, Case_Blk);
            Choice := Get_Chain (Choice);
            exit when Choice = Null_Iir;
            exit when not Get_Same_Alternative_Flag (Choice);
            pragma Assert (Get_Associated_Chain (Choice) = Null_Iir);
         end loop;
         Finish_Choice (Case_Blk);
         Translate_Statements_Chain (Stmt_Chain);
      end loop;
      Finish_Case_Stmt (Case_Blk);
   end Translate_Case_Statement;

   procedure Translate_Write_Procedure_Call (Imp : Iir; Param_Chain : Iir)
   is
      F_Assoc     : Iir;
      Value_Assoc : Iir;
      Value       : O_Dnode;
      Formal_Type : Iir;
      Tinfo       : Type_Info_Acc;
      Assocs      : O_Assoc_List;
      Subprg_Info : Subprg_Info_Acc;
   begin
      F_Assoc := Param_Chain;
      Value_Assoc := Get_Chain (Param_Chain);
      Formal_Type := Get_Type (Get_Formal (Value_Assoc));
      Tinfo := Get_Info (Formal_Type);
      case Tinfo.Type_Mode is
         when Type_Mode_Scalar =>
            Open_Temp;
            Start_Association (Assocs, Ghdl_Write_Scalar);
            --    compute file parameter (get an index)
            New_Association
              (Assocs, Chap7.Translate_Expression (Get_Actual (F_Assoc)));
            --    compute the value.
            Value := Create_Temp (Tinfo.Ortho_Type (Mode_Value));
            New_Assign_Stmt
              (New_Obj (Value),
               Chap7.Translate_Expression (Get_Actual (Value_Assoc),
                 Formal_Type));
            New_Association
              (Assocs,
               New_Unchecked_Address (New_Obj (Value), Ghdl_Ptr_Type));
            --    length.
            New_Association
              (Assocs, New_Lit (New_Sizeof (Tinfo.Ortho_Type (Mode_Value),
               Ghdl_Index_Type)));
            --    call a predefined procedure
            New_Procedure_Call (Assocs);
            Close_Temp;
         when Type_Mode_Array
            | Type_Mode_Record
            | Type_Mode_Fat_Array =>
            Subprg_Info := Get_Info (Imp);
            Start_Association (Assocs, Subprg_Info.Ortho_Func);
            Subprgs.Add_Subprg_Instance_Assoc
              (Assocs, Subprg_Info.Subprg_Instance);
            New_Association
              (Assocs, Chap7.Translate_Expression (Get_Actual (F_Assoc)));
            New_Association
              (Assocs,
               Chap7.Translate_Expression (Get_Actual (Value_Assoc),
                 Formal_Type));
            New_Procedure_Call (Assocs);
         when Type_Mode_Unknown
            | Type_Mode_File
            | Type_Mode_Acc
            | Type_Mode_Fat_Acc
            | Type_Mode_Protected =>
            raise Internal_Error;
      end case;
   end Translate_Write_Procedure_Call;

   procedure Translate_Read_Procedure_Call (Imp : Iir; Param_Chain : Iir)
   is
      F_Assoc     : Iir;
      Value_Assoc : Iir;
      Value       : Mnode;
      Formal_Type : Iir;
      Tinfo       : Type_Info_Acc;
      Assocs      : O_Assoc_List;
      Subprg_Info : Subprg_Info_Acc;
   begin
      F_Assoc := Param_Chain;
      Value_Assoc := Get_Chain (Param_Chain);
      Formal_Type := Get_Type (Get_Formal (Value_Assoc));
      Tinfo := Get_Info (Formal_Type);
      case Tinfo.Type_Mode is
         when Type_Mode_Scalar =>
            Open_Temp;
            Start_Association (Assocs, Ghdl_Read_Scalar);
            --    compute file parameter (get an index)
            New_Association
              (Assocs, Chap7.Translate_Expression (Get_Actual (F_Assoc)));
            --  value
            Value := Chap6.Translate_Name (Get_Actual (Value_Assoc));
            New_Association
              (Assocs, New_Convert_Ov (M2Addr (Value), Ghdl_Ptr_Type));
            --    length.
            New_Association
              (Assocs, New_Lit (New_Sizeof (Tinfo.Ortho_Type (Mode_Value),
               Ghdl_Index_Type)));
            --    call a predefined procedure
            New_Procedure_Call (Assocs);
            Close_Temp;
         when Type_Mode_Array
            | Type_Mode_Record =>
            Subprg_Info := Get_Info (Imp);
            Start_Association (Assocs, Subprg_Info.Ortho_Func);
            Subprgs.Add_Subprg_Instance_Assoc
              (Assocs, Subprg_Info.Subprg_Instance);
            New_Association
              (Assocs, Chap7.Translate_Expression (Get_Actual (F_Assoc)));
            New_Association
              (Assocs,
               Chap7.Translate_Expression (Get_Actual (Value_Assoc)));
            New_Procedure_Call (Assocs);
         when Type_Mode_Fat_Array =>
            declare
               Length_Assoc : Iir;
               Length       : Mnode;
            begin
               Length_Assoc := Get_Chain (Value_Assoc);
               Subprg_Info := Get_Info (Imp);
               Start_Association (Assocs, Subprg_Info.Ortho_Func);
               Subprgs.Add_Subprg_Instance_Assoc
                 (Assocs, Subprg_Info.Subprg_Instance);
               New_Association
                 (Assocs,
                  Chap7.Translate_Expression (Get_Actual (F_Assoc)));
               New_Association
                 (Assocs,
                  Chap7.Translate_Expression (Get_Actual (Value_Assoc),
                    Formal_Type));
               Length := Chap6.Translate_Name (Get_Actual (Length_Assoc));
               New_Assign_Stmt (M2Lv (Length), New_Function_Call (Assocs));
            end;
         when Type_Mode_Unknown
            | Type_Mode_File
            | Type_Mode_Acc
            | Type_Mode_Fat_Acc
            | Type_Mode_Protected =>
            raise Internal_Error;
      end case;
   end Translate_Read_Procedure_Call;

   procedure Translate_Implicit_Procedure_Call (Call : Iir_Procedure_Call)
   is
      Imp         : constant Iir := Get_Implementation (Call);
      Kind        : constant Iir_Predefined_Functions :=
        Get_Implicit_Definition (Imp);
      Param_Chain : constant Iir := Get_Parameter_Association_Chain (Call);
   begin
      case Kind is
         when Iir_Predefined_Write =>
            --  Check wether text or not.
            declare
               File_Param : Iir;
               Assocs     : O_Assoc_List;
            begin
               File_Param := Param_Chain;
               -- FIXME: do the test.
               if Get_Text_File_Flag (Get_Type (Get_Formal (File_Param)))
               then
                  --  If text:
                  Start_Association (Assocs, Ghdl_Text_Write);
                  --    compute file parameter (get an index)
                  New_Association
                    (Assocs,
                     Chap7.Translate_Expression (Get_Actual (File_Param)));
                  --    compute string parameter (get a fat array pointer)
                  New_Association
                    (Assocs, Chap7.Translate_Expression
                       (Get_Actual (Get_Chain (Param_Chain)),
                        String_Type_Definition));
                  --    call a predefined procedure
                  New_Procedure_Call (Assocs);
               else
                  Translate_Write_Procedure_Call (Imp, Param_Chain);
               end if;
            end;

         when Iir_Predefined_Read_Length =>
            --  FIXME: works only for text read length.
            declare
               File_Param : Iir;
               N_Param    : Iir;
               Assocs     : O_Assoc_List;
               Str        : O_Enode;
               Res        : Mnode;
            begin
               File_Param := Param_Chain;
               if Get_Text_File_Flag (Get_Type (Get_Formal (File_Param)))
               then
                  N_Param := Get_Chain (File_Param);
                  Str := Chap7.Translate_Expression
                    (Get_Actual (N_Param), String_Type_Definition);
                  N_Param := Get_Chain (N_Param);
                  Res := Chap6.Translate_Name (Get_Actual (N_Param));
                  Start_Association (Assocs, Ghdl_Text_Read_Length);
                  --    compute file parameter (get an index)
                  New_Association
                    (Assocs,
                     Chap7.Translate_Expression (Get_Actual (File_Param)));
                  --    compute string parameter (get a fat array pointer)
                  New_Association (Assocs, Str);
                  --    call a predefined procedure
                  New_Assign_Stmt
                    (M2Lv (Res), New_Function_Call (Assocs));
               else
                  Translate_Read_Procedure_Call (Imp, Param_Chain);
               end if;
            end;

         when Iir_Predefined_Read =>
            Translate_Read_Procedure_Call (Imp, Param_Chain);

         when Iir_Predefined_Deallocate =>
            Chap3.Translate_Object_Deallocation (Get_Actual (Param_Chain));

         when Iir_Predefined_File_Open =>
            declare
               N_Param    : Iir;
               File_Param : Iir;
               Name_Param : Iir;
               Kind_Param : Iir;
               Constr     : O_Assoc_List;
            begin
               File_Param := Get_Actual (Param_Chain);
               N_Param := Get_Chain (Param_Chain);
               Name_Param := Get_Actual (N_Param);
               N_Param := Get_Chain (N_Param);
               Kind_Param := Get_Actual (N_Param);
               if Get_Text_File_Flag (Get_Type (File_Param)) then
                  Start_Association (Constr, Ghdl_Text_File_Open);
               else
                  Start_Association (Constr, Ghdl_File_Open);
               end if;
               New_Association
                 (Constr, Chap7.Translate_Expression (File_Param));
               New_Association
                 (Constr, New_Convert_Ov
                    (Chap7.Translate_Expression (Kind_Param), Ghdl_I32_Type));
               New_Association
                 (Constr,
                  Chap7.Translate_Expression (Name_Param,
                    String_Type_Definition));
               New_Procedure_Call (Constr);
            end;

         when Iir_Predefined_File_Open_Status =>
            declare
               Std_File_Open_Status_Otype : constant O_Tnode :=
                 Get_Ortho_Type (File_Open_Status_Type_Definition,
                                 Mode_Value);
               N_Param      : Iir;
               Status_Param : constant Iir := Get_Actual (Param_Chain);
               File_Param   : Iir;
               Name_Param   : Iir;
               Kind_Param   : Iir;
               Constr       : O_Assoc_List;
               Status       : Mnode;
            begin
               Status := Chap6.Translate_Name (Status_Param);
               N_Param := Get_Chain (Param_Chain);
               File_Param := Get_Actual (N_Param);
               N_Param := Get_Chain (N_Param);
               Name_Param := Get_Actual (N_Param);
               N_Param := Get_Chain (N_Param);
               Kind_Param := Get_Actual (N_Param);
               if Get_Text_File_Flag (Get_Type (File_Param)) then
                  Start_Association (Constr, Ghdl_Text_File_Open_Status);
               else
                  Start_Association (Constr, Ghdl_File_Open_Status);
               end if;
               New_Association
                 (Constr, Chap7.Translate_Expression (File_Param));
               New_Association
                 (Constr, New_Convert_Ov
                    (Chap7.Translate_Expression (Kind_Param), Ghdl_I32_Type));
               New_Association
                 (Constr,
                  Chap7.Translate_Expression (Name_Param,
                    String_Type_Definition));
               New_Assign_Stmt
                 (M2Lv (Status),
                  New_Convert_Ov (New_Function_Call (Constr),
                    Std_File_Open_Status_Otype));
            end;

         when Iir_Predefined_File_Close =>
            declare
               File_Param : constant Iir := Get_Actual (Param_Chain);
               Constr     : O_Assoc_List;
            begin
               if Get_Text_File_Flag (Get_Type (File_Param)) then
                  Start_Association (Constr, Ghdl_Text_File_Close);
               else
                  Start_Association (Constr, Ghdl_File_Close);
               end if;
               New_Association
                 (Constr, Chap7.Translate_Expression (File_Param));
               New_Procedure_Call (Constr);
            end;

         when Iir_Predefined_Flush =>
            declare
               File_Param : constant Iir := Get_Actual (Param_Chain);
               Constr     : O_Assoc_List;
            begin
               Start_Association (Constr, Ghdl_File_Flush);
               New_Association
                 (Constr, Chap7.Translate_Expression (File_Param));
               New_Procedure_Call (Constr);
            end;

         when others =>
            Ada.Text_IO.Put_Line
              ("translate_implicit_procedure_call: cannot handle "
               & Iir_Predefined_Functions'Image (Kind));
            raise Internal_Error;
      end case;
   end Translate_Implicit_Procedure_Call;

   function Do_Conversion (Conv : Iir; Expr : Iir; Src : Mnode) return O_Enode
   is
   begin
      if Conv = Null_Iir then
         return M2E (Src);
         --  case Get_Type_Info (Dest).Type_Mode is
         --     when Type_Mode_Thin =>
         --        New_Assign_Stmt (M2Lv (Dest), M2E (Src));
         --     when Type_Mode_Fat_Acc =>
         --        Copy_Fat_Pointer (Stabilize (Dest), Stabilize (Src));
         --     when others =>
         --        raise Internal_Error;
         --  end case;
      else
         case Get_Kind (Conv) is
            when Iir_Kind_Function_Call =>
               --  Call conversion function.
               declare
                  Imp : constant Iir := Get_Implementation (Conv);
                  Conv_Info : constant Subprg_Info_Acc := Get_Info (Imp);
                  Constr : O_Assoc_List;
                  Res_Otype : Type_Info_Acc;
                  Res : O_Dnode;
               begin
                  Start_Association (Constr, Conv_Info.Ortho_Func);

                  if Conv_Info.Res_Interface /= O_Dnode_Null then
                     Res_Otype := Get_Info (Get_Return_Type (Imp));
                     Res := Create_Temp (Res_Otype.Ortho_Type (Mode_Value));
                     --  Composite result.
                     New_Association
                       (Constr,
                        New_Address (New_Obj (Res),
                                     Res_Otype.Ortho_Ptr_Type (Mode_Value)));
                  end if;

                  Subprgs.Add_Subprg_Instance_Assoc
                    (Constr, Conv_Info.Subprg_Instance);

                  New_Association (Constr, M2E (Src));

                  if Conv_Info.Res_Interface /= O_Dnode_Null then
                     --  Composite result.
                     New_Procedure_Call (Constr);
                     return New_Address
                       (New_Obj (Res), Res_Otype.Ortho_Ptr_Type (Mode_Value));
                  else
                     return New_Function_Call (Constr);
                  end if;
               end;
            when Iir_Kind_Type_Conversion =>
               return Chap7.Translate_Type_Conversion
                 (M2E (Src), Get_Type (Expr),
                  Get_Type (Conv), Null_Iir);
            when others =>
               Error_Kind ("do_conversion", Conv);
         end case;
      end if;
   end Do_Conversion;

   function Translate_Subprogram_Call (Imp : Iir; Assoc_Chain : Iir; Obj : Iir)
                                      return O_Enode
   is
      Is_Procedure : constant Boolean :=
        Get_Kind (Imp) = Iir_Kind_Procedure_Declaration;
      Is_Function : constant Boolean := not Is_Procedure;
      type Mnode_Array is array (Natural range <>) of Mnode;
      type O_Enode_Array is array (Natural range <>) of O_Enode;
      Nbr_Assoc : constant Natural :=
        Iir_Chains.Get_Chain_Length (Assoc_Chain);
      Params : Mnode_Array (0 .. Nbr_Assoc - 1);
      E_Params : O_Enode_Array (0 .. Nbr_Assoc - 1);
      Info : constant Subprg_Info_Acc := Get_Info (Imp);
      Copy_Out : O_Dnode;
      Res : Mnode;
      El : Iir;
      Pos : Natural;
      Constr : O_Assoc_List;
      Act : Iir;
      Actual_Type : Iir;
      Formal : Iir;
      Base_Formal : Iir;
      Formal_Type : Iir;
      Ftype_Info : Type_Info_Acc;
      Ftype_Binfo : Type_Info_Acc;
      Formal_Info : Ortho_Info_Acc;
      Val : O_Enode;
      Param : Mnode;
      Last_Individual : Natural;
      Ptr : O_Lnode;
      In_Conv : Iir;
      In_Expr : Iir;
      Out_Conv : Iir;
      Out_Expr : Iir;
      Formal_Object_Kind : Object_Kind_Type;
      Bounds : Mnode;
   begin
      --  For functions returning an unconstrained object: save the mark.
      if Is_Function and then Info.Use_Stack2 then
         Create_Temp_Stack2_Mark;
      end if;

      if Is_Function and then Info.Res_Interface /= O_Dnode_Null then
         --  Composite result.
         --  If we need to allocate, do it before starting the call!
         declare
            Res_Type : constant Iir := Get_Return_Type (Imp);
            Res_Info : constant Type_Info_Acc := Get_Info (Res_Type);
         begin
            Res := Create_Temp (Res_Info);
            if Res_Info.Type_Mode /= Type_Mode_Fat_Array then
               Chap4.Allocate_Complex_Object (Res_Type, Alloc_Stack, Res);
            end if;
         end;
      end if;

      --  Create an in-out result record for in-out arguments passed by
      --  value.
      if Is_Procedure and then Info.Res_Record_Type /= O_Tnode_Null then
         Copy_Out := Create_Temp (Info.Res_Record_Type);
      else
         Copy_Out := O_Dnode_Null;
      end if;

      --  Evaluate in-out parameters and parameters passed by ref, since
      --  they can add declarations.
      --  Non-composite in-out parameters address are saved in order to
      --  be able to assignate the result.
      El := Assoc_Chain;
      Pos := 0;
      while El /= Null_Iir loop
         Params (Pos) := Mnode_Null;
         E_Params (Pos) := O_Enode_Null;

         Formal := Get_Formal (El);
         if Get_Kind (Formal) in Iir_Kinds_Denoting_Name then
            Formal := Get_Named_Entity (Formal);
         end if;
         Base_Formal := Get_Association_Interface (El);
         Formal_Type := Get_Type (Formal);
         Formal_Info := Get_Info (Base_Formal);
         if Get_Kind (Base_Formal) = Iir_Kind_Interface_Signal_Declaration
         then
            Formal_Object_Kind := Mode_Signal;
         else
            Formal_Object_Kind := Mode_Value;
         end if;
         Ftype_Info := Get_Info (Formal_Type);
         Ftype_Binfo := Get_Info (Get_Base_Type (Formal_Type));

         case Get_Kind (El) is
            when Iir_Kind_Association_Element_Open =>
               Act := Get_Default_Value (Formal);
               In_Conv := Null_Iir;
               Out_Conv := Null_Iir;
            when Iir_Kind_Association_Element_By_Expression =>
               Act := Get_Actual (El);
               In_Conv := Get_In_Conversion (El);
               Out_Conv := Get_Out_Conversion (El);
            when Iir_Kind_Association_Element_By_Individual =>
               Actual_Type := Get_Actual_Type (El);

               --  A non-composite type cannot be associated by element.
               pragma Assert (Formal_Info.Interface_Field = O_Fnode_Null);

               if Ftype_Info.Type_Mode = Type_Mode_Fat_Array then
                  Chap3.Create_Array_Subtype (Actual_Type, True);
                  Bounds := Chap3.Get_Array_Type_Bounds (Actual_Type);
                  Param := Create_Temp (Ftype_Info, Formal_Object_Kind);
                  Chap3.Translate_Object_Allocation
                    (Param, Alloc_Stack, Formal_Type, Bounds);
               else
                  Param := Create_Temp (Ftype_Info, Formal_Object_Kind);
                  Chap4.Allocate_Complex_Object
                    (Formal_Type, Alloc_Stack, Param);
               end if;
               Last_Individual := Pos;
               Params (Pos) := Param;
               goto Continue;
            when others =>
               Error_Kind ("translate_procedure_call", El);
         end case;
         Actual_Type := Get_Type (Act);

         if Formal_Info.Interface_Field /= O_Fnode_Null then
            --  Copy-out argument.
            --  This is not a composite type.
            Param := Chap6.Translate_Name (Act);
            pragma Assert (Get_Object_Kind (Param) = Mode_Value);
            Params (Pos) := Stabilize (Param);
            if In_Conv /= Null_Iir
              or else Get_Mode (Formal) = Iir_Inout_Mode
            then
               --  Arguments may be assigned if there is an in conversion.
               Ptr := New_Selected_Element
                 (New_Obj (Copy_Out), Formal_Info.Interface_Field);
               Param := Lv2M (Ptr, Ftype_Info, Mode_Value);
               if In_Conv /= Null_Iir then
                  In_Expr := In_Conv;
               else
                  In_Expr := Act;
               end if;
               Chap7.Translate_Assign
                 (Param,
                  Do_Conversion (In_Conv, Act, Params (Pos)),
                  In_Expr,
                  Formal_Type, El);
            end if;
         elsif Ftype_Binfo.Type_Mode not in Type_Mode_By_Value then
            --  Passed by reference.
            case Get_Kind (Base_Formal) is
               when Iir_Kind_Interface_Constant_Declaration
                  | Iir_Kind_Interface_File_Declaration =>
                  --  No conversion here.
                  E_Params (Pos) :=
                    Chap7.Translate_Expression (Act, Formal_Type);
               when Iir_Kind_Interface_Variable_Declaration
                  | Iir_Kind_Interface_Signal_Declaration =>
                  Param := Chap6.Translate_Name (Act);
                  --  Atype may not have been set (eg: slice).
                  if Base_Formal /= Formal then
                     Stabilize (Param);
                     Params (Pos) := Param;
                  end if;
                  E_Params (Pos) := M2E (Param);
                  if Formal_Type /= Actual_Type then
                     --  Implicit array conversion or subtype check.
                     E_Params (Pos) := Chap7.Translate_Implicit_Conv
                       (E_Params (Pos), Actual_Type, Formal_Type,
                        Get_Object_Kind (Param), Act);
                  end if;
               when others =>
                  Error_Kind ("translate_procedure_call(2)", Formal);
            end case;
         end if;
         if Base_Formal /= Formal then
            --  Individual association.
            if Ftype_Binfo.Type_Mode not in Type_Mode_By_Value then
               --  Not by-value actual already translated.
               Val := E_Params (Pos);
            else
               --  By value association.
               Act := Get_Actual (El);
               if Get_Kind (Base_Formal)
                 = Iir_Kind_Interface_Constant_Declaration
               then
                  Val := Chap7.Translate_Expression (Act, Formal_Type);
               else
                  Params (Pos) := Chap6.Translate_Name (Act);
                  --  Since signals are passed by reference, they are not
                  --  copied back, so do not stabilize them (furthermore,
                  --  it is not possible to stabilize them).
                  if Formal_Object_Kind = Mode_Value then
                     Params (Pos) := Stabilize (Params (Pos));
                  end if;
                  Val := M2E (Params (Pos));
               end if;
            end if;
            --  Assign formal.
            --  Change the formal variable so that it is the local variable
            --  that will be passed to the subprogram.
            declare
               Prev_Node : O_Dnode;
            begin
               Prev_Node := Formal_Info.Interface_Node;
               --  We need a pointer since the interface is by reference.
               Formal_Info.Interface_Node :=
                 M2Dp (Params (Last_Individual));
               Param := Chap6.Translate_Name (Formal);
               Formal_Info.Interface_Node := Prev_Node;
            end;
            Chap7.Translate_Assign (Param, Val, Act, Formal_Type, El);
         end if;
         << Continue >> null;
         El := Get_Chain (El);
         Pos := Pos + 1;
      end loop;

      --  Second stage:  really perform the call.
      Start_Association (Constr, Info.Ortho_Func);

      if Is_Function and then Info.Res_Interface /= O_Dnode_Null then
         --  Composite result.
         New_Association (Constr, M2E (Res));
      end if;

      if Copy_Out /= O_Dnode_Null then
         New_Association
           (Constr, New_Address (New_Obj (Copy_Out), Info.Res_Record_Ptr));
      end if;

      if Obj /= Null_Iir then
         New_Association (Constr, M2E (Chap6.Translate_Name (Obj)));
      else
         Subprgs.Add_Subprg_Instance_Assoc (Constr, Info.Subprg_Instance);
      end if;

      --  Parameters.
      El := Assoc_Chain;
      Pos := 0;
      while El /= Null_Iir loop
         Formal := Get_Formal (El);
         if Get_Kind (Formal) in Iir_Kinds_Denoting_Name then
            Formal := Get_Named_Entity (Formal);
         end if;
         Base_Formal := Get_Association_Interface (El);
         Formal_Info := Get_Info (Base_Formal);
         Formal_Type := Get_Type (Formal);
         Ftype_Info := Get_Info (Formal_Type);

         if Get_Kind (El) = Iir_Kind_Association_Element_By_Individual then
            Last_Individual := Pos;
            New_Association (Constr, M2E (Params (Pos)));
         elsif Base_Formal /= Formal then
            --  Individual association.
            null;
         elsif Formal_Info.Interface_Field = O_Fnode_Null then
            if Ftype_Info.Type_Mode in Type_Mode_By_Value then
               --  Parameter passed by value.
               if E_Params (Pos) /= O_Enode_Null then
                  Val := E_Params (Pos);
                  raise Internal_Error;
               else
                  case Get_Kind (El) is
                     when Iir_Kind_Association_Element_Open =>
                        Act := Get_Default_Value (Formal);
                        In_Conv := Null_Iir;
                     when Iir_Kind_Association_Element_By_Expression =>
                        Act := Get_Actual (El);
                        In_Conv := Get_In_Conversion (El);
                     when others =>
                        Error_Kind ("translate_procedure_call(2)", El);
                  end case;
                  case Get_Kind (Formal) is
                     when Iir_Kind_Interface_Signal_Declaration =>
                        Param := Chap6.Translate_Name (Act);
                        --  This is a scalar.
                        Val := M2E (Param);
                     when others =>
                        if In_Conv = Null_Iir then
                           Val := Chap7.Translate_Expression
                             (Act, Formal_Type);
                           Val := Chap3.Maybe_Insert_Scalar_Check
                             (Val, Act, Formal_Type);
                        else
                           Actual_Type := Get_Type (Act);
                           Val := Do_Conversion
                             (In_Conv,
                              Act,
                              E2M (Chap7.Translate_Expression (Act,
                                Actual_Type),
                                Get_Info (Actual_Type),
                                Mode_Value));
                        end if;
                  end case;
               end if;
               New_Association (Constr, Val);
            else
               --  Parameter passed by ref, which was already computed.
               New_Association (Constr, E_Params (Pos));
            end if;
         end if;
         El := Get_Chain (El);
         Pos := Pos + 1;
      end loop;

      if Is_Procedure then
         New_Procedure_Call (Constr);
      else
         if Info.Res_Interface /= O_Dnode_Null then
            --  Composite result.
            New_Procedure_Call (Constr);
            return M2E (Res);
         else
            return New_Function_Call (Constr);
         end if;
      end if;

      --  Copy-out non-composite parameters.
      El := Assoc_Chain;
      Pos := 0;
      while El /= Null_Iir loop
         Formal := Get_Formal (El);
         Base_Formal := Get_Association_Interface (El);
         Formal_Type := Get_Type (Formal);
         Ftype_Info := Get_Info (Formal_Type);
         Formal_Info := Get_Info (Base_Formal);
         if Get_Kind (Base_Formal) = Iir_Kind_Interface_Variable_Declaration
           and then Get_Mode (Base_Formal) in Iir_Out_Modes
           and then Params (Pos) /= Mnode_Null
         then
            if Formal_Info.Interface_Field /= O_Fnode_Null then
               --  OUT parameters.
               Out_Conv := Get_Out_Conversion (El);
               if Out_Conv = Null_Iir then
                  Out_Expr := Formal;
               else
                  Out_Expr := Out_Conv;
               end if;
               Ptr := New_Selected_Element
                 (New_Obj (Copy_Out), Formal_Info.Interface_Field);
               Param := Lv2M (Ptr, Ftype_Info, Mode_Value);
               Chap7.Translate_Assign (Params (Pos),
                                       Do_Conversion (Out_Conv, Formal,
                                         Param),
                                       Out_Expr,
                                       Get_Type (Get_Actual (El)), El);
            elsif Base_Formal /= Formal then
               --  By individual.
               --  Copy back.
               Act := Get_Actual (El);
               declare
                  Prev_Node : O_Dnode;
               begin
                  Prev_Node := Formal_Info.Interface_Node;
                  --  We need a pointer since the interface is by reference.
                  Formal_Info.Interface_Node :=
                    M2Dp (Params (Last_Individual));
                  Val := Chap7.Translate_Expression
                    (Formal, Get_Type (Act));
                  Formal_Info.Interface_Node := Prev_Node;
               end;
               Chap7.Translate_Assign
                 (Params (Pos), Val, Formal, Get_Type (Act), El);
            end if;
         end if;
         El := Get_Chain (El);
         Pos := Pos + 1;
      end loop;

      return O_Enode_Null;
   end Translate_Subprogram_Call;

   procedure Translate_Procedure_Call (Stmt : Iir_Procedure_Call)
   is
      Assoc_Chain : constant Iir := Get_Parameter_Association_Chain (Stmt);
      Imp : constant Iir := Get_Implementation (Stmt);
      Obj : constant Iir := Get_Method_Object (Stmt);
      Res : O_Enode;
   begin
      Res := Translate_Subprogram_Call (Imp, Assoc_Chain, Obj);
      pragma Assert (Res = O_Enode_Null);
   end Translate_Procedure_Call;

   procedure Translate_Wait_Statement (Stmt : Iir)
   is
      Sensitivity : Iir_List;
      Cond        : Iir;
      Timeout     : Iir;
      Constr      : O_Assoc_List;
   begin
      Sensitivity := Get_Sensitivity_List (Stmt);
      Cond := Get_Condition_Clause (Stmt);
      Timeout := Get_Timeout_Clause (Stmt);

      if Sensitivity = Null_Iir_List and Cond /= Null_Iir then
         Sensitivity := Create_Iir_List;
         Canon.Canon_Extract_Sensitivity (Cond, Sensitivity);
         Set_Sensitivity_List (Stmt, Sensitivity);
      end if;

      --  Check for simple cases.
      if Sensitivity = Null_Iir_List
        and then Cond = Null_Iir
      then
         if Timeout = Null_Iir then
            --  Process exit.
            Start_Association (Constr, Ghdl_Process_Wait_Exit);
            New_Procedure_Call (Constr);
         else
            --  Wait for a timeout.
            Start_Association (Constr, Ghdl_Process_Wait_Timeout);
            New_Association (Constr, Chap7.Translate_Expression
                             (Timeout, Time_Type_Definition));
            New_Procedure_Call (Constr);
         end if;
         return;
      end if;

      --  Evaluate the timeout (if any) and register it,
      if Timeout /= Null_Iir then
         Start_Association (Constr, Ghdl_Process_Wait_Set_Timeout);
         New_Association (Constr, Chap7.Translate_Expression
                          (Timeout, Time_Type_Definition));
         New_Procedure_Call (Constr);
      end if;

      --  Evaluate the sensitivity list and register it.
      if Sensitivity /= Null_Iir_List then
         Register_Signal_List
           (Sensitivity, Ghdl_Process_Wait_Add_Sensitivity);
      end if;

      if Cond = Null_Iir then
         declare
            V : O_Dnode;
         begin
            --  declare
            --     v : __ghdl_bool_type_node;
            --  begin
            --     v := suspend ();
            --  end;
            Open_Temp;
            V := Create_Temp (Ghdl_Bool_Type);
            Start_Association (Constr, Ghdl_Process_Wait_Suspend);
            New_Assign_Stmt (New_Obj (V), New_Function_Call (Constr));
            Close_Temp;
         end;
      else
         declare
            Label : O_Snode;
         begin
            --  start loop
            Start_Loop_Stmt (Label);

            --    if suspend() then        --  return true if timeout.
            --      exit;
            --    end if;
            Start_Association (Constr, Ghdl_Process_Wait_Suspend);
            Gen_Exit_When (Label, New_Function_Call (Constr));

            --    if condition then
            --      exit;
            --    end if;
            Open_Temp;
            Gen_Exit_When
              (Label,
               Chap7.Translate_Expression (Cond, Boolean_Type_Definition));
            Close_Temp;

            --  end loop;
            Finish_Loop_Stmt (Label);
         end;
      end if;

      --  wait_close;
      Start_Association (Constr, Ghdl_Process_Wait_Close);
      New_Procedure_Call (Constr);
   end Translate_Wait_Statement;

   --  Signal assignment.
   Signal_Assign_Line : Natural;
   procedure Gen_Simple_Signal_Assign_Non_Composite (Targ      : Mnode;
                                                     Targ_Type : Iir;
                                                     Val       : O_Enode)
   is
      Type_Info : Type_Info_Acc;
      Subprg    : O_Dnode;
      Conv      : O_Tnode;
      Assoc     : O_Assoc_List;
   begin
      Type_Info := Get_Info (Targ_Type);
      case Type_Info.Type_Mode is
         when Type_Mode_B1 =>
            Subprg := Ghdl_Signal_Simple_Assign_B1;
            Conv := Ghdl_Bool_Type;
         when Type_Mode_E8 =>
            Subprg := Ghdl_Signal_Simple_Assign_E8;
            Conv := Ghdl_I32_Type;
         when Type_Mode_E32 =>
            Subprg := Ghdl_Signal_Simple_Assign_E32;
            Conv := Ghdl_I32_Type;
         when Type_Mode_I32
            | Type_Mode_P32 =>
            Subprg := Ghdl_Signal_Simple_Assign_I32;
            Conv := Ghdl_I32_Type;
         when Type_Mode_P64
            | Type_Mode_I64 =>
            Subprg := Ghdl_Signal_Simple_Assign_I64;
            Conv := Ghdl_I64_Type;
         when Type_Mode_F64 =>
            Subprg := Ghdl_Signal_Simple_Assign_F64;
            Conv := Ghdl_Real_Type;
         when Type_Mode_Array =>
            raise Internal_Error;
         when others =>
            Error_Kind ("gen_signal_assign_non_composite", Targ_Type);
      end case;
      if Chap3.Need_Range_Check (Null_Iir, Targ_Type) then
         declare
            If_Blk : O_If_Block;
            Val2   : O_Dnode;
            Targ2  : O_Dnode;
         begin
            Open_Temp;
            Val2 := Create_Temp_Init
              (Type_Info.Ortho_Type (Mode_Value), Val);
            Targ2 := Create_Temp_Init
              (Ghdl_Signal_Ptr, New_Convert_Ov (New_Value (M2Lv (Targ)),
               Ghdl_Signal_Ptr));
            Start_If_Stmt (If_Blk, Chap3.Not_In_Range (Val2, Targ_Type));
            Start_Association (Assoc, Ghdl_Signal_Simple_Assign_Error);
            New_Association (Assoc, New_Obj_Value (Targ2));
            Assoc_Filename_Line (Assoc, Signal_Assign_Line);
            New_Procedure_Call (Assoc);
            New_Else_Stmt (If_Blk);
            Start_Association (Assoc, Subprg);
            New_Association (Assoc, New_Obj_Value (Targ2));
            New_Association
              (Assoc, New_Convert_Ov (New_Obj_Value (Val2), Conv));
            New_Procedure_Call (Assoc);
            Finish_If_Stmt (If_Blk);
            Close_Temp;
         end;
      else
         Start_Association (Assoc, Subprg);
         New_Association (Assoc, New_Convert_Ov (New_Value (M2Lv (Targ)),
                          Ghdl_Signal_Ptr));
         New_Association (Assoc, New_Convert_Ov (Val, Conv));
         New_Procedure_Call (Assoc);
      end if;
   end Gen_Simple_Signal_Assign_Non_Composite;

   procedure Gen_Simple_Signal_Assign is new Foreach_Non_Composite
     (Data_Type => O_Enode,
      Composite_Data_Type => Mnode,
      Do_Non_Composite => Gen_Simple_Signal_Assign_Non_Composite,
      Prepare_Data_Array => Gen_Oenode_Prepare_Data_Composite,
      Update_Data_Array => Gen_Oenode_Update_Data_Array,
      Finish_Data_Array => Gen_Oenode_Finish_Data_Composite,
      Prepare_Data_Record => Gen_Oenode_Prepare_Data_Composite,
      Update_Data_Record => Gen_Oenode_Update_Data_Record,
      Finish_Data_Record => Gen_Oenode_Finish_Data_Composite);

   type Signal_Assign_Data is record
      Expr   : Mnode;
      Reject : O_Dnode;
      After  : O_Dnode;
   end record;

   procedure Gen_Start_Signal_Assign_Non_Composite
     (Targ : Mnode; Targ_Type : Iir; Data : Signal_Assign_Data)
   is
      Type_Info : Type_Info_Acc;
      Subprg    : O_Dnode;
      Conv      : O_Tnode;
      Assoc     : O_Assoc_List;
   begin
      if Data.Expr = Mnode_Null then
         --  Null transaction.
         Start_Association (Assoc, Ghdl_Signal_Start_Assign_Null);
         New_Association (Assoc, New_Convert_Ov (New_Value (M2Lv (Targ)),
                          Ghdl_Signal_Ptr));
         New_Association (Assoc, New_Obj_Value (Data.Reject));
         New_Association (Assoc, New_Obj_Value (Data.After));
         New_Procedure_Call (Assoc);
         return;
      end if;

      Type_Info := Get_Info (Targ_Type);
      case Type_Info.Type_Mode is
         when Type_Mode_B1 =>
            Subprg := Ghdl_Signal_Start_Assign_B1;
            Conv := Ghdl_Bool_Type;
         when Type_Mode_E8 =>
            Subprg := Ghdl_Signal_Start_Assign_E8;
            Conv := Ghdl_I32_Type;
         when Type_Mode_E32 =>
            Subprg := Ghdl_Signal_Start_Assign_E32;
            Conv := Ghdl_I32_Type;
         when Type_Mode_I32
            | Type_Mode_P32 =>
            Subprg := Ghdl_Signal_Start_Assign_I32;
            Conv := Ghdl_I32_Type;
         when Type_Mode_P64
            | Type_Mode_I64 =>
            Subprg := Ghdl_Signal_Start_Assign_I64;
            Conv := Ghdl_I64_Type;
         when Type_Mode_F64 =>
            Subprg := Ghdl_Signal_Start_Assign_F64;
            Conv := Ghdl_Real_Type;
         when Type_Mode_Array =>
            raise Internal_Error;
         when others =>
            Error_Kind ("gen_signal_assign_non_composite", Targ_Type);
      end case;
      --  Check range.
      if Chap3.Need_Range_Check (Null_Iir, Targ_Type) then
         declare
            If_Blk : O_If_Block;
            V      : Mnode;
            Starg  : O_Dnode;
         begin
            Open_Temp;
            V := Stabilize_Value (Data.Expr);
            Starg := Create_Temp_Init
              (Ghdl_Signal_Ptr,
               New_Convert_Ov (New_Value (M2Lv (Targ)), Ghdl_Signal_Ptr));
            Start_If_Stmt
              (If_Blk, Chap3.Not_In_Range (M2Dv (V), Targ_Type));
            Start_Association (Assoc, Ghdl_Signal_Start_Assign_Error);
            New_Association (Assoc, New_Obj_Value (Starg));
            New_Association (Assoc, New_Obj_Value (Data.Reject));
            New_Association (Assoc, New_Obj_Value (Data.After));
            Assoc_Filename_Line (Assoc, Signal_Assign_Line);
            New_Procedure_Call (Assoc);
            New_Else_Stmt (If_Blk);
            Start_Association (Assoc, Subprg);
            New_Association (Assoc, New_Obj_Value (Starg));
            New_Association (Assoc, New_Obj_Value (Data.Reject));
            New_Association (Assoc, New_Convert_Ov (M2E (V), Conv));
            New_Association (Assoc, New_Obj_Value (Data.After));
            New_Procedure_Call (Assoc);
            Finish_If_Stmt (If_Blk);
            Close_Temp;
         end;
      else
         Start_Association (Assoc, Subprg);
         New_Association (Assoc, New_Convert_Ov (New_Value (M2Lv (Targ)),
                          Ghdl_Signal_Ptr));
         New_Association (Assoc, New_Obj_Value (Data.Reject));
         New_Association (Assoc, New_Convert_Ov (M2E (Data.Expr), Conv));
         New_Association (Assoc, New_Obj_Value (Data.After));
         New_Procedure_Call (Assoc);
      end if;
   end Gen_Start_Signal_Assign_Non_Composite;

   function Gen_Signal_Prepare_Data_Composite
     (Targ : Mnode; Targ_Type : Iir; Val : Signal_Assign_Data)
         return Signal_Assign_Data
   is
      pragma Unreferenced (Targ, Targ_Type);
   begin
      return Val;
   end Gen_Signal_Prepare_Data_Composite;

   function Gen_Signal_Prepare_Data_Record
     (Targ : Mnode; Targ_Type : Iir; Val : Signal_Assign_Data)
         return Signal_Assign_Data
   is
      pragma Unreferenced (Targ, Targ_Type);
   begin
      if Val.Expr = Mnode_Null then
         return Val;
      else
         return Signal_Assign_Data'
           (Expr => Stabilize (Val.Expr),
            Reject => Val.Reject,
            After => Val.After);
      end if;
   end Gen_Signal_Prepare_Data_Record;

   function Gen_Signal_Update_Data_Array
     (Val       : Signal_Assign_Data;
      Targ_Type : Iir;
      Index     : O_Dnode)
         return Signal_Assign_Data
   is
      Res : Signal_Assign_Data;
   begin
      if Val.Expr = Mnode_Null then
         --  Handle null transaction.
         return Val;
      end if;
      Res := Signal_Assign_Data'
        (Expr => Chap3.Index_Base (Chap3.Get_Array_Base (Val.Expr),
         Targ_Type, New_Obj_Value (Index)),
         Reject => Val.Reject,
         After => Val.After);
      return Res;
   end Gen_Signal_Update_Data_Array;

   function Gen_Signal_Update_Data_Record
     (Val       : Signal_Assign_Data;
      Targ_Type : Iir;
      El        : Iir_Element_Declaration)
         return Signal_Assign_Data
   is
      pragma Unreferenced (Targ_Type);
      Res : Signal_Assign_Data;
   begin
      if Val.Expr = Mnode_Null then
         --  Handle null transaction.
         return Val;
      end if;
      Res := Signal_Assign_Data'
        (Expr => Chap6.Translate_Selected_Element (Val.Expr, El),
         Reject => Val.Reject,
         After => Val.After);
      return Res;
   end Gen_Signal_Update_Data_Record;

   procedure Gen_Signal_Finish_Data_Composite
     (Data : in out Signal_Assign_Data)
   is
      pragma Unreferenced (Data);
   begin
      null;
   end Gen_Signal_Finish_Data_Composite;

   procedure Gen_Start_Signal_Assign is new Foreach_Non_Composite
     (Data_Type => Signal_Assign_Data,
      Composite_Data_Type => Signal_Assign_Data,
      Do_Non_Composite => Gen_Start_Signal_Assign_Non_Composite,
      Prepare_Data_Array => Gen_Signal_Prepare_Data_Composite,
      Update_Data_Array => Gen_Signal_Update_Data_Array,
      Finish_Data_Array => Gen_Signal_Finish_Data_Composite,
      Prepare_Data_Record => Gen_Signal_Prepare_Data_Record,
      Update_Data_Record => Gen_Signal_Update_Data_Record,
      Finish_Data_Record => Gen_Signal_Finish_Data_Composite);

   procedure Gen_Next_Signal_Assign_Non_Composite
     (Targ : Mnode; Targ_Type : Iir; Data : Signal_Assign_Data)
   is
      Type_Info : Type_Info_Acc;
      Subprg    : O_Dnode;
      Conv      : O_Tnode;
      Assoc     : O_Assoc_List;
   begin
      if Data.Expr = Mnode_Null then
         --  Null transaction.
         Start_Association (Assoc, Ghdl_Signal_Next_Assign_Null);
         New_Association (Assoc, New_Convert_Ov (New_Value (M2Lv (Targ)),
                          Ghdl_Signal_Ptr));
         New_Association (Assoc, New_Obj_Value (Data.After));
         New_Procedure_Call (Assoc);
         return;
      end if;

      Type_Info := Get_Info (Targ_Type);
      case Type_Info.Type_Mode is
         when Type_Mode_B1 =>
            Subprg := Ghdl_Signal_Next_Assign_B1;
            Conv := Ghdl_Bool_Type;
         when Type_Mode_E8 =>
            Subprg := Ghdl_Signal_Next_Assign_E8;
            Conv := Ghdl_I32_Type;
         when Type_Mode_E32 =>
            Subprg := Ghdl_Signal_Next_Assign_E32;
            Conv := Ghdl_I32_Type;
         when Type_Mode_I32
            | Type_Mode_P32 =>
            Subprg := Ghdl_Signal_Next_Assign_I32;
            Conv := Ghdl_I32_Type;
         when Type_Mode_P64
            | Type_Mode_I64 =>
            Subprg := Ghdl_Signal_Next_Assign_I64;
            Conv := Ghdl_I64_Type;
         when Type_Mode_F64 =>
            Subprg := Ghdl_Signal_Next_Assign_F64;
            Conv := Ghdl_Real_Type;
         when Type_Mode_Array =>
            raise Internal_Error;
         when others =>
            Error_Kind ("gen_signal_next_assign_non_composite", Targ_Type);
      end case;
      if Chap3.Need_Range_Check (Null_Iir, Targ_Type) then
         declare
            If_Blk : O_If_Block;
            V      : Mnode;
            Starg  : O_Dnode;
         begin
            Open_Temp;
            V := Stabilize_Value (Data.Expr);
            Starg := Create_Temp_Init
              (Ghdl_Signal_Ptr,
               New_Convert_Ov (New_Value (M2Lv (Targ)), Ghdl_Signal_Ptr));
            Start_If_Stmt
              (If_Blk, Chap3.Not_In_Range (M2Dv (V), Targ_Type));

            Start_Association (Assoc, Ghdl_Signal_Next_Assign_Error);
            New_Association (Assoc, New_Obj_Value (Starg));
            New_Association (Assoc, New_Obj_Value (Data.After));
            Assoc_Filename_Line (Assoc, Signal_Assign_Line);
            New_Procedure_Call (Assoc);

            New_Else_Stmt (If_Blk);

            Start_Association (Assoc, Subprg);
            New_Association (Assoc, New_Obj_Value (Starg));
            New_Association (Assoc, New_Convert_Ov (M2E (V), Conv));
            New_Association (Assoc, New_Obj_Value (Data.After));
            New_Procedure_Call (Assoc);

            Finish_If_Stmt (If_Blk);
            Close_Temp;
         end;
      else
         Start_Association (Assoc, Subprg);
         New_Association (Assoc, New_Convert_Ov (New_Value (M2Lv (Targ)),
                          Ghdl_Signal_Ptr));
         New_Association (Assoc, New_Convert_Ov (M2E (Data.Expr), Conv));
         New_Association (Assoc, New_Obj_Value (Data.After));
         New_Procedure_Call (Assoc);
      end if;
   end Gen_Next_Signal_Assign_Non_Composite;

   procedure Gen_Next_Signal_Assign is new Foreach_Non_Composite
     (Data_Type => Signal_Assign_Data,
      Composite_Data_Type => Signal_Assign_Data,
      Do_Non_Composite => Gen_Next_Signal_Assign_Non_Composite,
      Prepare_Data_Array => Gen_Signal_Prepare_Data_Composite,
      Update_Data_Array => Gen_Signal_Update_Data_Array,
      Finish_Data_Array => Gen_Signal_Finish_Data_Composite,
      Prepare_Data_Record => Gen_Signal_Prepare_Data_Record,
      Update_Data_Record => Gen_Signal_Update_Data_Record,
      Finish_Data_Record => Gen_Signal_Finish_Data_Composite);

   procedure Translate_Signal_Target_Aggr
     (Aggr : Mnode; Target : Iir; Target_Type : Iir);

   procedure Translate_Signal_Target_Array_Aggr
     (Aggr        : Mnode;
      Target      : Iir;
      Target_Type : Iir;
      Idx         : O_Dnode;
      Dim         : Natural)
   is
      Index_List : constant Iir_List :=
        Get_Index_Subtype_List (Target_Type);
      Nbr_Dim    : constant Natural := Get_Nbr_Elements (Index_List);
      Sub_Aggr   : Mnode;
      El         : Iir;
      Expr       : Iir;
   begin
      El := Get_Association_Choices_Chain (Target);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Choice_By_None =>
               Sub_Aggr := Chap3.Index_Base
                 (Aggr, Target_Type, New_Obj_Value (Idx));
            when others =>
               Error_Kind ("translate_signal_target_array_aggr", El);
         end case;
         Expr := Get_Associated_Expr (El);
         if Dim = Nbr_Dim then
            Translate_Signal_Target_Aggr
              (Sub_Aggr, Expr, Get_Element_Subtype (Target_Type));
            if Get_Kind (El) = Iir_Kind_Choice_By_None then
               Inc_Var (Idx);
            else
               raise Internal_Error;
            end if;
         else
            Translate_Signal_Target_Array_Aggr
              (Sub_Aggr, Expr, Target_Type, Idx, Dim + 1);
         end if;
         El := Get_Chain (El);
      end loop;
   end Translate_Signal_Target_Array_Aggr;

   procedure Translate_Signal_Target_Record_Aggr
     (Aggr : Mnode; Target : Iir; Target_Type : Iir)
   is
      Aggr_El  : Iir;
      El_List  : Iir_List;
      El_Index : Natural;
      Element  : Iir_Element_Declaration;
   begin
      El_List := Get_Elements_Declaration_List
        (Get_Base_Type (Target_Type));
      El_Index := 0;
      Aggr_El := Get_Association_Choices_Chain (Target);
      while Aggr_El /= Null_Iir loop
         case Get_Kind (Aggr_El) is
            when Iir_Kind_Choice_By_None =>
               Element := Get_Nth_Element (El_List, El_Index);
               El_Index := El_Index + 1;
            when Iir_Kind_Choice_By_Name =>
               Element := Get_Choice_Name (Aggr_El);
               El_Index := Natural'Last;
            when others =>
               Error_Kind ("translate_signal_target_record_aggr", Aggr_El);
         end case;
         Translate_Signal_Target_Aggr
           (Chap6.Translate_Selected_Element (Aggr, Element),
            Get_Associated_Expr (Aggr_El), Get_Type (Element));
         Aggr_El := Get_Chain (Aggr_El);
      end loop;
   end Translate_Signal_Target_Record_Aggr;

   procedure Translate_Signal_Target_Aggr
     (Aggr : Mnode; Target : Iir; Target_Type : Iir)
   is
      Src : Mnode;
   begin
      if Get_Kind (Target) = Iir_Kind_Aggregate then
         declare
            Idx     : O_Dnode;
            St_Aggr : Mnode;
         begin
            Open_Temp;
            St_Aggr := Stabilize (Aggr);
            case Get_Kind (Target_Type) is
               when Iir_Kinds_Array_Type_Definition =>
                  Idx := Create_Temp (Ghdl_Index_Type);
                  Init_Var (Idx);
                  Translate_Signal_Target_Array_Aggr
                    (St_Aggr, Target, Target_Type, Idx, 1);
               when Iir_Kind_Record_Type_Definition
                  | Iir_Kind_Record_Subtype_Definition =>
                  Translate_Signal_Target_Record_Aggr
                    (St_Aggr, Target, Target_Type);
               when others =>
                  Error_Kind ("translate_signal_target_aggr", Target_Type);
            end case;
            Close_Temp;
         end;
      else
         Src := Chap6.Translate_Name (Target);
         Chap3.Translate_Object_Copy (Aggr, M2E (Src), Target_Type);
      end if;
   end Translate_Signal_Target_Aggr;

   type Signal_Direct_Assign_Data is record
      --  The driver
      Drv : Mnode;

      --  The value
      Expr : Mnode;

      --  The node for the expression (used to locate errors).
      Expr_Node : Iir;
   end record;

   procedure Gen_Signal_Direct_Assign_Non_Composite
     (Targ : Mnode; Targ_Type : Iir; Data : Signal_Direct_Assign_Data)
   is
      Targ_Sig : Mnode;
      If_Blk   : O_If_Block;
      Constr   : O_Assoc_List;
      Cond     : O_Dnode;
      Drv      : Mnode;
   begin
      Open_Temp;
      Targ_Sig := Stabilize (Targ, True);
      Cond := Create_Temp (Ghdl_Bool_Type);
      Drv := Stabilize (Data.Drv, False);

      --  Set driver.
      Chap7.Translate_Assign
        (Drv, M2E (Data.Expr), Data.Expr_Node, Targ_Type, Data.Expr_Node);

      --  Test if the signal is active.
      Start_If_Stmt
        (If_Blk,
         New_Value (Chap14.Get_Signal_Field
           (Targ_Sig, Ghdl_Signal_Has_Active_Field)));
      --  Either because has_active is true.
      New_Assign_Stmt (New_Obj (Cond),
                       New_Lit (Ghdl_Bool_True_Node));
      New_Else_Stmt (If_Blk);
      --  Or because the value is different from the current driving value.
      --  FIXME: ideally, we should compare the value with the current
      --   value of the driver. This is an approximation that might break
      --   with weird resolution functions.
      New_Assign_Stmt
        (New_Obj (Cond),
         New_Compare_Op (ON_Neq,
           Chap7.Translate_Signal_Driving_Value
             (M2E (Targ_Sig), Targ_Type),
           M2E (Drv),
           Ghdl_Bool_Type));
      Finish_If_Stmt (If_Blk);

      --  Put signal into active list (if not already in the list).
      --  FIXME: this is not thread-safe!
      Start_If_Stmt (If_Blk, New_Obj_Value (Cond));
      Start_Association (Constr, Ghdl_Signal_Direct_Assign);
      New_Association (Constr,
                       New_Convert_Ov (New_Value (M2Lv (Targ_Sig)),
                         Ghdl_Signal_Ptr));
      New_Procedure_Call (Constr);
      Finish_If_Stmt (If_Blk);

      Close_Temp;
   end Gen_Signal_Direct_Assign_Non_Composite;

   function Gen_Signal_Direct_Prepare_Data_Composite
     (Targ : Mnode; Targ_Type : Iir; Val : Signal_Direct_Assign_Data)
         return Signal_Direct_Assign_Data
   is
      pragma Unreferenced (Targ, Targ_Type);
   begin
      return Val;
   end Gen_Signal_Direct_Prepare_Data_Composite;

   function Gen_Signal_Direct_Prepare_Data_Record
     (Targ : Mnode; Targ_Type : Iir; Val : Signal_Direct_Assign_Data)
         return Signal_Direct_Assign_Data
   is
      pragma Unreferenced (Targ, Targ_Type);
   begin
      return Signal_Direct_Assign_Data'
        (Drv => Stabilize (Val.Drv),
         Expr => Stabilize (Val.Expr),
         Expr_Node => Val.Expr_Node);
   end Gen_Signal_Direct_Prepare_Data_Record;

   function Gen_Signal_Direct_Update_Data_Array
     (Val       : Signal_Direct_Assign_Data;
      Targ_Type : Iir;
      Index     : O_Dnode)
         return Signal_Direct_Assign_Data
   is
   begin
      return Signal_Direct_Assign_Data'
        (Drv => Chap3.Index_Base (Chap3.Get_Array_Base (Val.Drv),
         Targ_Type, New_Obj_Value (Index)),
         Expr => Chap3.Index_Base (Chap3.Get_Array_Base (Val.Expr),
           Targ_Type, New_Obj_Value (Index)),
         Expr_Node => Val.Expr_Node);
   end Gen_Signal_Direct_Update_Data_Array;

   function Gen_Signal_Direct_Update_Data_Record
     (Val       : Signal_Direct_Assign_Data;
      Targ_Type : Iir;
      El        : Iir_Element_Declaration)
         return Signal_Direct_Assign_Data
   is
      pragma Unreferenced (Targ_Type);
   begin
      return Signal_Direct_Assign_Data'
        (Drv => Chap6.Translate_Selected_Element (Val.Drv, El),
         Expr => Chap6.Translate_Selected_Element (Val.Expr, El),
         Expr_Node => Val.Expr_Node);
   end Gen_Signal_Direct_Update_Data_Record;

   procedure Gen_Signal_Direct_Finish_Data_Composite
     (Data : in out Signal_Direct_Assign_Data)
   is
      pragma Unreferenced (Data);
   begin
      null;
   end Gen_Signal_Direct_Finish_Data_Composite;

   procedure Gen_Signal_Direct_Assign is new Foreach_Non_Composite
     (Data_Type => Signal_Direct_Assign_Data,
      Composite_Data_Type => Signal_Direct_Assign_Data,
      Do_Non_Composite => Gen_Signal_Direct_Assign_Non_Composite,
      Prepare_Data_Array => Gen_Signal_Direct_Prepare_Data_Composite,
      Update_Data_Array => Gen_Signal_Direct_Update_Data_Array,
      Finish_Data_Array => Gen_Signal_Direct_Finish_Data_Composite,
      Prepare_Data_Record => Gen_Signal_Direct_Prepare_Data_Record,
      Update_Data_Record => Gen_Signal_Direct_Update_Data_Record,
      Finish_Data_Record => Gen_Signal_Direct_Finish_Data_Composite);

   procedure Translate_Direct_Signal_Assignment (Stmt : Iir; We : Iir)
   is
      Target      : constant Iir := Get_Target (Stmt);
      Target_Type : constant Iir := Get_Type (Target);
      Arg         : Signal_Direct_Assign_Data;
      Targ_Sig    : Mnode;
   begin
      Chap6.Translate_Direct_Driver (Target, Targ_Sig, Arg.Drv);

      Arg.Expr := E2M (Chap7.Translate_Expression (We, Target_Type),
                       Get_Info (Target_Type), Mode_Value);
      Arg.Expr_Node := We;
      Gen_Signal_Direct_Assign (Targ_Sig, Target_Type, Arg);
   end Translate_Direct_Signal_Assignment;

   procedure Translate_Signal_Assignment_Statement (Stmt : Iir)
   is
      Target      : Iir;
      Target_Type : Iir;
      We          : Iir_Waveform_Element;
      Targ        : Mnode;
      Val         : O_Enode;
      Value       : Iir;
      Is_Simple   : Boolean;
   begin
      Target := Get_Target (Stmt);
      Target_Type := Get_Type (Target);
      We := Get_Waveform_Chain (Stmt);

      if We /= Null_Iir
        and then Get_Chain (We) = Null_Iir
        and then Get_Time (We) = Null_Iir
        and then Get_Delay_Mechanism (Stmt) = Iir_Inertial_Delay
        and then Get_Reject_Time_Expression (Stmt) = Null_Iir
      then
         --  Simple signal assignment ?
         Value := Get_We_Value (We);
         Is_Simple := Get_Kind (Value) /= Iir_Kind_Null_Literal;
      else
         Is_Simple := False;
      end if;

      if Get_Kind (Target) = Iir_Kind_Aggregate then
         Chap3.Translate_Anonymous_Type_Definition (Target_Type, True);
         Targ := Create_Temp (Get_Info (Target_Type), Mode_Signal);
         Chap4.Allocate_Complex_Object (Target_Type, Alloc_Stack, Targ);
         Translate_Signal_Target_Aggr (Targ, Target, Target_Type);
      else
         if Is_Simple
           and then Flag_Direct_Drivers
           and then Chap4.Has_Direct_Driver (Target)
         then
            Translate_Direct_Signal_Assignment (Stmt, Value);
            return;
         end if;
         Targ := Chap6.Translate_Name (Target);
         if Get_Object_Kind (Targ) /= Mode_Signal then
            raise Internal_Error;
         end if;
      end if;

      if We = Null_Iir then
         --  Implicit disconnect statment.
         Register_Signal (Targ, Target_Type, Ghdl_Signal_Disconnect);
         return;
      end if;

      --  Handle a simple and common case: only one waveform, inertial,
      --  and no time (eg: sig <= expr).
      Value := Get_We_Value (We);
      Signal_Assign_Line := Get_Line_Number (Value);
      if Get_Chain (We) = Null_Iir
        and then Get_Time (We) = Null_Iir
        and then Get_Delay_Mechanism (Stmt) = Iir_Inertial_Delay
        and then Get_Reject_Time_Expression (Stmt) = Null_Iir
        and then Get_Kind (Value) /= Iir_Kind_Null_Literal
      then
         Val := Chap7.Translate_Expression (Value, Target_Type);
         Gen_Simple_Signal_Assign (Targ, Target_Type, Val);
         return;
      end if;

      --  General case.
      declare
         Var_Targ   : Mnode;
         Targ_Tinfo : Type_Info_Acc;
      begin
         Open_Temp;
         Targ_Tinfo := Get_Info (Target_Type);
         Var_Targ := Stabilize (Targ, True);

         --  Translate the first waveform element.
         declare
            Reject_Time : O_Dnode;
            After_Time  : O_Dnode;
            Del         : Iir;
            Rej         : Iir;
            Val         : Mnode;
            Data        : Signal_Assign_Data;
         begin
            Open_Temp;
            Reject_Time := Create_Temp (Std_Time_Otype);
            After_Time := Create_Temp (Std_Time_Otype);
            Del := Get_Time (We);
            if Del = Null_Iir then
               New_Assign_Stmt
                 (New_Obj (After_Time),
                  New_Lit (New_Signed_Literal (Std_Time_Otype, 0)));
            else
               New_Assign_Stmt
                 (New_Obj (After_Time),
                  Chap7.Translate_Expression (Del, Time_Type_Definition));
            end if;
            case Get_Delay_Mechanism (Stmt) is
               when Iir_Transport_Delay =>
                  New_Assign_Stmt
                    (New_Obj (Reject_Time),
                     New_Lit (New_Signed_Literal (Std_Time_Otype, 0)));
               when Iir_Inertial_Delay =>
                  Rej := Get_Reject_Time_Expression (Stmt);
                  if Rej = Null_Iir then
                     New_Assign_Stmt (New_Obj (Reject_Time),
                                      New_Obj_Value (After_Time));
                  else
                     New_Assign_Stmt
                       (New_Obj (Reject_Time), Chap7.Translate_Expression
                        (Rej, Time_Type_Definition));
                  end if;
            end case;
            if Get_Kind (Value) = Iir_Kind_Null_Literal then
               Val := Mnode_Null;
            else
               Val := E2M (Chap7.Translate_Expression (Value, Target_Type),
                           Targ_Tinfo, Mode_Value);
               Val := Stabilize (Val);
            end if;
            Data := Signal_Assign_Data'(Expr => Val,
                                        Reject => Reject_Time,
                                        After => After_Time);
            Gen_Start_Signal_Assign (Var_Targ, Target_Type, Data);
            Close_Temp;
         end;

         --  Translate other waveform elements.
         We := Get_Chain (We);
         while We /= Null_Iir loop
            declare
               After_Time : O_Dnode;
               Val        : Mnode;
               Data       : Signal_Assign_Data;
            begin
               Open_Temp;
               After_Time := Create_Temp (Std_Time_Otype);
               New_Assign_Stmt
                 (New_Obj (After_Time),
                  Chap7.Translate_Expression (Get_Time (We),
                    Time_Type_Definition));
               Value := Get_We_Value (We);
               Signal_Assign_Line := Get_Line_Number (Value);
               if Get_Kind (Value) = Iir_Kind_Null_Literal then
                  Val := Mnode_Null;
               else
                  Val :=
                    E2M (Chap7.Translate_Expression (Value, Target_Type),
                         Targ_Tinfo, Mode_Value);
               end if;
               Data := Signal_Assign_Data'(Expr => Val,
                                           Reject => O_Dnode_Null,
                                           After => After_Time);
               Gen_Next_Signal_Assign (Var_Targ, Target_Type, Data);
               Close_Temp;
            end;
            We := Get_Chain (We);
         end loop;

         Close_Temp;
      end;
   end Translate_Signal_Assignment_Statement;

   procedure Translate_Statement (Stmt : Iir)
   is
   begin
      New_Debug_Line_Stmt (Get_Line_Number (Stmt));
      Open_Temp;
      case Get_Kind (Stmt) is
         when Iir_Kind_Return_Statement =>
            Translate_Return_Statement (Stmt);

         when Iir_Kind_If_Statement =>
            Translate_If_Statement (Stmt);
         when Iir_Kind_Assertion_Statement =>
            Translate_Assertion_Statement (Stmt);
         when Iir_Kind_Report_Statement =>
            Translate_Report_Statement (Stmt);
         when Iir_Kind_Case_Statement =>
            Translate_Case_Statement (Stmt);

         when Iir_Kind_For_Loop_Statement =>
            Translate_For_Loop_Statement (Stmt);
         when Iir_Kind_While_Loop_Statement =>
            Translate_While_Loop_Statement (Stmt);
         when Iir_Kind_Next_Statement
            | Iir_Kind_Exit_Statement =>
            Translate_Exit_Next_Statement (Stmt);

         when Iir_Kind_Signal_Assignment_Statement =>
            Translate_Signal_Assignment_Statement (Stmt);
         when Iir_Kind_Variable_Assignment_Statement =>
            Translate_Variable_Assignment_Statement (Stmt);

         when Iir_Kind_Null_Statement =>
            --  A null statement is translated to a NOP, so that the
            --  statement generates code (and a breakpoint can be set on
            --  it).
            --  Emit_Nop;
            null;

         when Iir_Kind_Procedure_Call_Statement =>
            declare
               Call : constant Iir := Get_Procedure_Call (Stmt);
               Imp  : constant Iir := Get_Implementation (Call);
            begin
               Canon.Canon_Subprogram_Call (Call);
               if Is_Implicit_Subprogram (Imp) then
                  Translate_Implicit_Procedure_Call (Call);
               else
                  Translate_Procedure_Call (Call);
               end if;
            end;

         when Iir_Kind_Wait_Statement =>
            Translate_Wait_Statement (Stmt);

         when others =>
            Error_Kind ("translate_statement", Stmt);
      end case;
      Close_Temp;
   end Translate_Statement;

   procedure Translate_Statements_Chain (First : Iir)
   is
      Stmt : Iir;
   begin
      Stmt := First;
      while Stmt /= Null_Iir loop
         Translate_Statement (Stmt);
         Stmt := Get_Chain (Stmt);
      end loop;
   end Translate_Statements_Chain;

   function Translate_Statements_Chain_Has_Return (First : Iir)
                                                      return Boolean
   is
      Stmt       : Iir;
      Has_Return : Boolean := False;
   begin
      Stmt := First;
      while Stmt /= Null_Iir loop
         Translate_Statement (Stmt);
         if Get_Kind (Stmt) = Iir_Kind_Return_Statement then
            Has_Return := True;
         end if;
         Stmt := Get_Chain (Stmt);
      end loop;
      return Has_Return;
   end Translate_Statements_Chain_Has_Return;
end Trans.Chap8;
