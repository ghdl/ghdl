--  Iir to ortho translator.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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

with Simple_IO;
with Std_Names;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Nodes_Utils; use Vhdl.Nodes_Utils;
with Vhdl.Canon;
with Vhdl.Evaluation; use Vhdl.Evaluation;
with Vhdl.Std_Package; use Vhdl.Std_Package;
with Vhdl.Utils; use Vhdl.Utils;
with Trans.Chap2;
with Trans.Chap3;
with Trans.Chap4;
with Trans.Chap6;
with Trans.Chap7;
with Trans.Chap9;
with Trans.Chap14;
with Trans_Decls; use Trans_Decls;
with Translation; use Translation;
with Trans.Helpers2; use Trans.Helpers2;
with Trans.Foreach_Non_Composite;

package body Trans.Chap8 is
   use Trans.Helpers;

   --  The LOCAL_STATE is a local variable read from the frame at entry and
   --  written before return.  The value INITIAL_STATE (0) is the initial
   --  state.  For processes, this is the state for the first statement.  For
   --  subprograms, this is the state at call, before dynamic elaboration of
   --  local declarations.
   --  Subprograms have more special values:
   --   1: The return state.  Finalization is performed.
   Local_State : O_Dnode := O_Dnode_Null;

   Initial_State : constant State_Type := 0;
   --  Return_State  : constant State_Value_Type := 1;

   --  Next value available.
   State_Next : State_Type := Initial_State;

   --  Info node to which the state variable is attached.  Used to set and save
   --  the state variable.
   State_Info : Ortho_Info_Acc := null;

   --  Statements construct for the state machine.  The generated code is:
   --    local var STATE: index_type;
   --  begin
   --    STATE := FRAME.all.STATE;
   --    loop
   --       case STATE is
   --         when 0 => ...
   --         when 1 => ...
   --         ...
   --       end case;
   --    end loop;
   --   end;
   State_Case : Ortho_Nodes.O_Case_Block;
   State_Loop : Ortho_Nodes.O_Snode;

   function Get_State_Var (Info : Ortho_Info_Acc) return O_Lnode is
   begin
      case Info.Kind is
         when Kind_Process =>
            return Get_Var (Info.Process_State);
         when Kind_Subprg =>
            return New_Selected_Acc_Value
              (New_Obj (Info.Res_Interface), Info.Subprg_State_Field);
         when others =>
            raise Internal_Error;
      end case;
   end Get_State_Var;

   procedure State_Entry (Info : Ortho_Info_Acc) is
   begin
      --  Not reentrant.
      pragma Assert (not State_Enabled);

      State_Info := Info;

      --  For optimization, create a copy of the STATE variable.
      New_Var_Decl (Local_State, Get_Identifier ("STATE"),
                    O_Storage_Local, Ghdl_Index_Type);

      --  Initialize it from the frame.
      New_Assign_Stmt (New_Obj (Local_State),
                       New_Value (Get_State_Var (Info)));

      Start_Loop_Stmt (State_Loop);
      Start_Case_Stmt (State_Case, New_Obj_Value (Local_State));

      State_Start (0);
      State_Next := 0;
   end State_Entry;

   procedure State_Leave (Parent : Iir) is
   begin
      pragma Assert (State_Enabled);
      pragma Assert (Get_Info (Parent) = State_Info);

      if State_Debug then
         Start_Choice (State_Case);
         New_Default_Choice (State_Case);
         Finish_Choice (State_Case);
         Chap6.Gen_Program_Error (Parent, Chap6.Prg_Err_Unreach_State);
      end if;

      Finish_Case_Stmt (State_Case);
      Finish_Loop_Stmt (State_Loop);
      Local_State := O_Dnode_Null;
      State_Info := null;
   end State_Leave;

   function State_Enabled return Boolean is
   begin
      return Local_State /= O_Dnode_Null;
   end State_Enabled;

   function State_Allocate return State_Type is
   begin
      State_Next := State_Next + 1;
      return State_Next;
   end State_Allocate;

   function State_To_Lit (State : State_Type) return O_Cnode is
   begin
      return New_Index_Lit (Unsigned_64 (State));
   end State_To_Lit;

   procedure State_Start (State : State_Type) is
   begin
      Start_Choice (State_Case);
      New_Expr_Choice (State_Case, State_To_Lit (State));
      Finish_Choice (State_Case);
   end State_Start;

   procedure State_Jump (Next_State : State_Type) is
   begin
      New_Assign_Stmt (New_Obj (Local_State),
                       New_Lit (State_To_Lit (Next_State)));
   end State_Jump;

   procedure State_Jump_Force is
   begin
      New_Next_Stmt (State_Loop);
   end State_Jump_Force;

   procedure State_Suspend (Next_State : State_Type) is
   begin
      New_Assign_Stmt (Get_State_Var (State_Info),
                       New_Lit (State_To_Lit (Next_State)));
      New_Return_Stmt;
   end State_Suspend;

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
         if Get_Suspend_Flag (Chap2.Current_Subprogram) then
            State_Jump (State_Return);
            State_Jump_Force;
         else
            Gen_Return;
         end if;

         return;
      end if;

      --  Return in a function.
      Ret_Type := Get_Return_Type (Chap2.Current_Subprogram);
      Ret_Info := Get_Info (Ret_Type);
      case Ret_Info.Type_Mode is
         when Type_Mode_Scalar
            | Type_Mode_Acc
            | Type_Mode_Bounds_Acc =>
            --  * if the return type is scalar, simply returns.
            --  * access: no range.
            declare
               V : O_Dnode;
               R : O_Enode;
            begin
               --  Always uses a temporary in case of the return expression
               --  uses secondary stack.  This can happen in constructs like:
               --    return my_func (param)(index);
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
         when Type_Mode_Unbounded_Array
           | Type_Mode_Unbounded_Record =>
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
               Val := Stabilize (Chap7.Translate_Expression (Expr, Ret_Type));
               Chap3.Translate_Object_Allocation
                 (Area, Alloc_Return, Ret_Type,
                  Chap3.Get_Composite_Bounds (Val));
               Chap3.Translate_Object_Copy (Area, Val, Ret_Type);
               Gen_Return;
            end;
         when Type_Mode_Bounded_Records
            | Type_Mode_Bounded_Arrays =>
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
         when Type_Mode_File
            | Type_Mode_Unknown
            | Type_Mode_Protected =>
            raise Internal_Error;
      end case;
   end Translate_Return_Statement;

   --  Translate the condition COND of a control statement.
   --  This is special as it frees immediately the stack2 (if needed) because
   --  the control statement may prevent the execution of the normal stack2
   --  release at the end of the temporary region.
   --  As a consequence, this function must be called within a brand new
   --  and dedicated temporary region.
   --  Use of this function is not needed for processes with state, because
   --  the control statement becomes an assignment to the next state.
   function Translate_Condition (Cond : Iir) return O_Enode
   is
      Res     : O_Enode;
      Res_Var : O_Dnode;
   begin
      --  As a statement is always wrapped into a temporary region, the
      --  stack2 is not used (in the inner region).
      pragma Assert (not Has_Stack2_Mark);

      --  Translate the condition.
      Res := Chap7.Translate_Expression (Cond);

      --  If the condition needs stack2, free it now as a inner statement
      --  may return (and this skipping the release of stack2).
      if Has_Stack2_Mark then
         Res_Var := Create_Temp_Init (Std_Boolean_Type_Node, Res);
         Stack2_Release;
         Res := New_Obj_Value (Res_Var);
      end if;

      return Res;
   end Translate_Condition;

   procedure Translate_If_Statement_State_Jumps
     (Stmt : Iir; Fall_State : State_Type)
   is
      Blk         : O_If_Block;
      Else_Clause : Iir;
   begin
      Start_If_Stmt
        (Blk, Chap7.Translate_Expression (Get_Condition (Stmt)));
      State_Jump (State_Allocate);
      New_Else_Stmt (Blk);
      Else_Clause := Get_Else_Clause (Stmt);
      if Else_Clause = Null_Iir then
         State_Jump (Fall_State);
      else
         if Get_Condition (Else_Clause) = Null_Iir then
            State_Jump (State_Allocate);
         else
            Open_Temp;
            New_Debug_Line_Stmt (Get_Line_Number (Else_Clause));
            Translate_If_Statement_State_Jumps (Else_Clause, Fall_State);
            Close_Temp;
         end if;
      end if;
      Finish_If_Stmt (Blk);
   end Translate_If_Statement_State_Jumps;

   procedure Translate_If_Statement_State (Stmt : Iir)
   is
      Fall_State : State_Type;
      Next_State : State_Type;
      Branch : Iir;
   begin
      Fall_State := State_Allocate;
      Next_State := Fall_State;

      --  Generate the jumps.
      Open_Temp;
      Translate_If_Statement_State_Jumps (Stmt, Fall_State);
      Close_Temp;

      --  Generate statements.
      Branch := Stmt;
      loop
         Next_State := Next_State + 1;
         State_Start (Next_State);
         Translate_Statements_Chain (Get_Sequential_Statement_Chain (Branch));
         State_Jump (Fall_State);

         Branch := Get_Else_Clause (Branch);
         exit when Branch = Null_Iir;
      end loop;

      State_Start (Fall_State);
   end Translate_If_Statement_State;

   procedure Translate_If_Statement_Direct (Stmt : Iir)
   is
      Blk         : O_If_Block;
      Else_Clause : Iir;
      Cond        : O_Enode;
   begin
      Cond := Translate_Condition
        (Strip_Reference_Name (Get_Condition (Stmt)));

      Start_If_Stmt (Blk, Cond);
      Translate_Statements_Chain (Get_Sequential_Statement_Chain (Stmt));

      Else_Clause := Get_Else_Clause (Stmt);
      if Else_Clause /= Null_Iir then
         New_Else_Stmt (Blk);
         if Get_Condition (Else_Clause) = Null_Iir then
            Translate_Statements_Chain
              (Get_Sequential_Statement_Chain (Else_Clause));
         else
            Open_Temp;
            New_Debug_Line_Stmt (Get_Line_Number (Else_Clause));
            Translate_If_Statement_Direct (Else_Clause);
            Close_Temp;
         end if;
      end if;
      Finish_If_Stmt (Blk);
   end Translate_If_Statement_Direct;

   procedure Translate_If_Statement (Stmt : Iir) is
   begin
      if Get_Suspend_Flag (Stmt) then
         Translate_If_Statement_State (Stmt);
      else
         Translate_If_Statement_Direct (Stmt);
      end if;
   end Translate_If_Statement;

   --  Inc or dec ITERATOR according to DIR.
   procedure Gen_Update_Iterator (Iterator : Var_Type;
                                  Dir      : Direction_Type;
                                  Itype    : Iir)
   is
      Base_Type : constant Iir := Get_Base_Type (Itype);
      Op        : ON_Op_Kind;
      V         : O_Enode;
   begin
      case Get_Kind (Base_Type) is
         when Iir_Kind_Integer_Type_Definition =>
            V := New_Lit
              (New_Signed_Literal
                 (Get_Ortho_Type (Base_Type, Mode_Value), 1));
         when Iir_Kind_Enumeration_Type_Definition =>
            declare
               List : constant Iir_Flist :=
                 Get_Enumeration_Literal_List (Base_Type);
               Num : Natural;
            begin
               if Get_Nbr_Elements (List) = 1 then
                  --  In the case of:
                  --    type E is ('T')
                  --  the iterator must have already finished.  So it doesn't
                  --  matter if not incremented.
                  Num := 0;
               else
                  Num := 1;
               end if;
               V := New_Lit (Get_Ortho_Literal (Get_Nth_Element (List, Num)));
            end;

         when others =>
            Error_Kind ("gen_update_iterator", Base_Type);
      end case;

      case Dir is
         when Dir_To =>
            Op := ON_Add_Ov;
         when Dir_Downto =>
            Op := ON_Sub_Ov;
      end case;

      New_Assign_Stmt (Get_Var (Iterator),
                       New_Dyadic_Op (Op, New_Value (Get_Var (Iterator)), V));
   end Gen_Update_Iterator;

   function Is_For_Loop_Iterator_Stable (Iterator : Iir) return Boolean
   is
      Iter_Type : constant Iir := Get_Type (Iterator);
      Constraint : constant Iir := Get_Range_Constraint (Iter_Type);
      Name : Iir;
   begin
      case Iir_Kinds_Range_Attribute (Get_Kind (Constraint)) is
         when Iir_Kind_Reverse_Range_Array_Attribute =>
            --  Need to create a reversed range...
            return False;
         when Iir_Kind_Range_Array_Attribute =>
            Name := Get_Prefix (Constraint);
            Name := Get_Base_Name (Name);

            case Get_Kind (Name) is
               when Iir_Kind_Implicit_Dereference
                 | Iir_Kind_Dereference =>
                  return False;
               when Iir_Kind_Function_Call =>
                  if not Is_Fully_Constrained_Type (Get_Type (Name)) then
                     return False;
                  end if;
               when Iir_Kinds_Object_Declaration =>
                  null;
               when Iir_Kind_Subtype_Declaration =>
                  null;
               when others =>
                  Error_Kind ("is_for_loop_iterator_stable(2)", Name);
            end case;
            return True;
      end case;
   end Is_For_Loop_Iterator_Stable;

   function Get_Iterator_Range_Var (Iterator : Iir) return Mnode
   is
      Iter_Type : constant Iir := Get_Type (Iterator);
      Iter_Type_Info : constant Type_Info_Acc :=
        Get_Info (Get_Base_Type (Iter_Type));
      It_Info : constant Ortho_Info_Acc := Get_Info (Iterator);
   begin
      if It_Info.Iterator_Range_Copy then
         return Lv2M (Get_Var (It_Info.Iterator_Range),
                      Iter_Type_Info, Mode_Value,
                      Iter_Type_Info.B.Range_Type,
                      Iter_Type_Info.B.Range_Ptr_Type);
      else
         return Lp2M (Get_Var (It_Info.Iterator_Range),
                      Iter_Type_Info, Mode_Value,
                      Iter_Type_Info.B.Range_Type,
                      Iter_Type_Info.B.Range_Ptr_Type);
      end if;
   end Get_Iterator_Range_Var;

   procedure Translate_For_Loop_Statement_Declaration (Stmt : Iir)
   is
      Iterator : constant Iir := Get_Parameter_Specification (Stmt);
      Iter_Type : constant Iir := Get_Type (Iterator);
      Iter_Type_Info : constant Type_Info_Acc :=
        Get_Info (Get_Base_Type (Iter_Type));
      Constraint     : constant Iir := Get_Range_Constraint (Iter_Type);
      It_Info : Ortho_Info_Acc;
      Range_Type : O_Tnode;
   begin
      --  Iterator range.
      Chap3.Translate_Object_Subtype_Indication (Iterator, False);

      --  Iterator variable.
      It_Info := Add_Info (Iterator, Kind_Iterator);
      It_Info.Iterator_Var := Create_Var
        (Create_Var_Identifier (Iterator),
         Iter_Type_Info.Ortho_Type (Mode_Value),
         O_Storage_Local);

      if Get_Kind (Constraint) = Iir_Kind_Range_Expression then
         It_Info.Iterator_Right := Create_Var
           (Create_Var_Identifier ("IT_RIGHT"),
            Iter_Type_Info.Ortho_Type (Mode_Value),
            O_Storage_Local);
      else
         --  The range must be copied if:
         --  * the constraint is 'range or 'reverse_range, or 'subtype, or
         --    'element (ie any attribute ?)
         --  * the base name is a function_call returning an unbounded value,
         --    or a dereference.
         --  Note: in case of a dereference, the anonymous object can be
         --  deallocated within the loop.
         It_Info.Iterator_Range_Copy :=
           not Is_For_Loop_Iterator_Stable (Iterator);
         if It_Info.Iterator_Range_Copy then
            Range_Type := Iter_Type_Info.B.Range_Type;
         else
            Range_Type := Iter_Type_Info.B.Range_Ptr_Type;
         end if;
         It_Info.Iterator_Range := Create_Var
           (Create_Var_Identifier ("IT_RANGE"), Range_Type, O_Storage_Local);
      end if;
   end Translate_For_Loop_Statement_Declaration;

   procedure Start_For_Loop (Iterator : Iir_Iterator_Declaration;
                             Cond     : out O_Enode)
   is
      Iter_Type      : constant Iir := Get_Type (Iterator);
      Iter_Base_Type : constant Iir := Get_Base_Type (Iter_Type);
      Iter_Type_Info : constant Ortho_Info_Acc := Get_Info (Iter_Base_Type);
      It_Info        : constant Ortho_Info_Acc := Get_Info (Iterator);
      Constraint     : constant Iir := Get_Range_Constraint (Iter_Type);
      Dir            : Direction_Type;
      Op             : ON_Op_Kind;
      Rng            : O_Lnode;
   begin
      if Get_Kind (Constraint) = Iir_Kind_Range_Expression then
         New_Assign_Stmt
           (Get_Var (It_Info.Iterator_Var),
            Chap7.Translate_Range_Expression_Left (Constraint,
                                                   Iter_Base_Type));
         Dir := Get_Direction (Constraint);
         New_Assign_Stmt
           (Get_Var (It_Info.Iterator_Right),
            Chap7.Translate_Range_Expression_Right (Constraint,
                                                    Iter_Base_Type));
         case Dir is
            when Dir_To =>
               Op := ON_Le;
            when Dir_Downto =>
               Op := ON_Ge;
         end case;
         --  Check for at least one iteration.
         Cond := New_Compare_Op
           (Op, New_Value (Get_Var (It_Info.Iterator_Var)),
            New_Value (Get_Var (It_Info.Iterator_Right)),
            Ghdl_Bool_Type);
      else
         Rng := Chap7.Translate_Range (Constraint, Iter_Base_Type);
         if It_Info.Iterator_Range_Copy then
            Gen_Memcpy (M2Addr (Get_Iterator_Range_Var (Iterator)),
                        New_Address (Rng, Iter_Type_Info.B.Range_Ptr_Type),
                        New_Lit (New_Sizeof (Iter_Type_Info.B.Range_Type,
                                             Ghdl_Index_Type)));
         else
            New_Assign_Stmt
              (Get_Var (It_Info.Iterator_Range),
               New_Address (Rng, Iter_Type_Info.B.Range_Ptr_Type));
         end if;
         New_Assign_Stmt
           (Get_Var (It_Info.Iterator_Var),
            M2E (Chap3.Range_To_Left (Get_Iterator_Range_Var (Iterator))));
         --  Before starting the loop, check whether there will be at least
         --  one iteration.
         Cond := New_Compare_Op
           (ON_Gt,
            M2E (Chap3.Range_To_Length (Get_Iterator_Range_Var (Iterator))),
            New_Lit (Ghdl_Index_0),
            Ghdl_Bool_Type);
      end if;
   end Start_For_Loop;

   procedure Exit_Cond_For_Loop (Iterator : Iir; Cond : out O_Enode)
   is
      Iter_Type      : constant Iir := Get_Type (Iterator);
      It_Info        : constant Ortho_Info_Acc := Get_Info (Iterator);
      Constraint     : constant Iir := Get_Range_Constraint (Iter_Type);
      Val            : O_Enode;
   begin
      --  Check end of loop.
      --  Equality is necessary and enough.

      if Get_Kind (Constraint) = Iir_Kind_Range_Expression then
         Val := New_Value (Get_Var (It_Info.Iterator_Right));
      else
         Val := M2E (Chap3.Range_To_Right (Get_Iterator_Range_Var (Iterator)));
      end if;
      Cond := New_Compare_Op (ON_Eq,
                              New_Value (Get_Var (It_Info.Iterator_Var)), Val,
                              Ghdl_Bool_Type);
   end Exit_Cond_For_Loop;

   procedure Update_For_Loop (Iterator : Iir)
   is
      Iter_Type      : constant Iir := Get_Type (Iterator);
      Iter_Base_Type : constant Iir := Get_Base_Type (Iter_Type);
      It_Info        : constant Ortho_Info_Acc := Get_Info (Iterator);
      If_Blk1        : O_If_Block;
      Deep_Rng       : Iir;
      Deep_Reverse   : Boolean;
   begin
      --  Update the iterator.
      Chap6.Get_Deep_Range_Expression (Iter_Type, Deep_Rng, Deep_Reverse);
      if Deep_Rng /= Null_Iir then
         if Get_Direction (Deep_Rng) = Dir_To xor Deep_Reverse then
            Gen_Update_Iterator (It_Info.Iterator_Var,
                                 Dir_To, Iter_Base_Type);
         else
            Gen_Update_Iterator (It_Info.Iterator_Var,
                                 Dir_Downto, Iter_Base_Type);
         end if;
      else
         Start_If_Stmt
           (If_Blk1, New_Compare_Op
              (ON_Eq,
               M2E (Chap3.Range_To_Dir (Get_Iterator_Range_Var (Iterator))),
               New_Lit (Ghdl_Dir_To_Node),
               Ghdl_Bool_Type));
         Gen_Update_Iterator (It_Info.Iterator_Var,
                              Dir_To, Iter_Base_Type);
         New_Else_Stmt (If_Blk1);
         Gen_Update_Iterator (It_Info.Iterator_Var,
                              Dir_Downto, Iter_Base_Type);
         Finish_If_Stmt (If_Blk1);
      end if;
   end Update_For_Loop;

   Current_Loop : Iir := Null_Iir;

   procedure Translate_For_Loop_Statement_State
     (Stmt : Iir_For_Loop_Statement)
   is
      Iterator       : constant Iir := Get_Parameter_Specification (Stmt);
      It_Info        : constant Ortho_Info_Acc := Get_Info (Iterator);
      Info           : constant Loop_State_Info_Acc := Get_Info (Stmt);
      Loop_If : O_If_Block;
      Cond : O_Enode;
   begin
      pragma Assert (It_Info /= null);

      Info.Loop_State_Next := State_Allocate;
      Info.Loop_State_Exit := State_Allocate;
      Info.Loop_State_Body := State_Allocate;

      --  Loop header: initialize iterator, skip the whole body in case of
      --  null range.
      Open_Temp;
      Start_For_Loop (Iterator, Cond);
      Start_If_Stmt (Loop_If, Cond);
      State_Jump (Info.Loop_State_Body);
      New_Else_Stmt (Loop_If);
      State_Jump (Info.Loop_State_Exit);
      Finish_If_Stmt (Loop_If);
      Close_Temp;

      --  Loop body.
      State_Start (Info.Loop_State_Body);
      Translate_Statements_Chain (Get_Sequential_Statement_Chain (Stmt));
      State_Jump (Info.Loop_State_Next);

      --  Loop next.
      State_Start (Info.Loop_State_Next);
      Exit_Cond_For_Loop (Iterator, Cond);
      Start_If_Stmt (Loop_If, Cond);
      State_Jump (Info.Loop_State_Exit);
      New_Else_Stmt (Loop_If);
      Update_For_Loop (Iterator);
      State_Jump (Info.Loop_State_Body);
      Finish_If_Stmt (Loop_If);

      --  Exit state, after loop.
      State_Start (Info.Loop_State_Exit);

      Free_Info (Iterator);
   end Translate_For_Loop_Statement_State;

   procedure Translate_For_Loop_Statement_Direct
     (Stmt : Iir_For_Loop_Statement)
   is
      Iterator : constant Iir := Get_Parameter_Specification (Stmt);
      Loop_Info : Loop_Info_Acc;

      --  If around the loop, to check if the loop must be executed.
      Loop_If                 : O_If_Block;
      Cond : O_Enode;
   begin
      Start_Declare_Stmt;

      Open_Temp;

      Translate_For_Loop_Statement_Declaration (Stmt);

      --  Loop header: initialize iterator.
      Start_For_Loop (Iterator, Cond);

      --  Skip the whole loop in case of null range.
      Start_If_Stmt (Loop_If, Cond);

      --  Start loop.
      --  There are two blocks: one for the exit, one for the next.

      Loop_Info := Add_Info (Stmt, Kind_Loop);
      Start_Loop_Stmt (Loop_Info.Label_Exit);
      Start_Loop_Stmt (Loop_Info.Label_Next);

      --  Loop body.
      Translate_Statements_Chain (Get_Sequential_Statement_Chain (Stmt));

      --  Fake 'next' statement.
      New_Exit_Stmt (Loop_Info.Label_Next);
      Finish_Loop_Stmt (Loop_Info.Label_Next);

      --  Exit loop if right bound reached.
      Exit_Cond_For_Loop (Iterator, Cond);
      Gen_Exit_When (Loop_Info.Label_Exit, Cond);

      Update_For_Loop (Iterator);

      Finish_Loop_Stmt (Loop_Info.Label_Exit);
      Finish_If_Stmt (Loop_If);
      Close_Temp;

      Free_Info (Stmt);

      Finish_Declare_Stmt;

      Free_Info (Iterator);
   end Translate_For_Loop_Statement_Direct;

   procedure Translate_For_Loop_Statement (Stmt : Iir_For_Loop_Statement)
   is
      Prev_Loop      : Iir;
   begin
      Prev_Loop := Current_Loop;
      Current_Loop := Stmt;

      if Get_Suspend_Flag (Stmt) then
         Translate_For_Loop_Statement_State (Stmt);
      else
         Translate_For_Loop_Statement_Direct (Stmt);
      end if;

      Current_Loop := Prev_Loop;
   end Translate_For_Loop_Statement;

   procedure Translate_While_Loop_Statement (Stmt : Iir_While_Loop_Statement)
   is
      Cond : constant Iir := Get_Condition (Stmt);
      Prev_Loop : Iir;
   begin
      Prev_Loop := Current_Loop;
      Current_Loop := Stmt;

      if Get_Suspend_Flag (Stmt) then
         declare
            Info : constant Loop_State_Info_Acc := Get_Info (Stmt);
            Blk : O_If_Block;
         begin
            Info.Loop_State_Next := State_Allocate;
            Info.Loop_State_Exit := State_Allocate;

            --  NEXT_STATE:
            State_Jump (Info.Loop_State_Next);
            State_Start (Info.Loop_State_Next);

            if Cond /= Null_Iir then
               Info.Loop_State_Body := State_Allocate;

               --  if COND then
               --    goto BODY_STATE;
               --  else
               --    goto EXIT_STATE;
               --  end if;
               Open_Temp;
               Start_If_Stmt (Blk, Chap7.Translate_Expression (Cond));
               State_Jump (Info.Loop_State_Body);
               New_Else_Stmt (Blk);
               State_Jump (Info.Loop_State_Exit);
               Finish_If_Stmt (Blk);
               Close_Temp;

               --  BODY_STATE:
               State_Start (Info.Loop_State_Body);
            end if;

            Translate_Statements_Chain (Get_Sequential_Statement_Chain (Stmt));

            --  goto NEXT_STATE
            State_Jump (Info.Loop_State_Next);

            --  EXIT_STATE:
            State_Start (Info.Loop_State_Exit);
         end;
      else
         declare
            Info : Loop_Info_Acc;
         begin
            Info := Add_Info (Stmt, Kind_Loop);

            Start_Loop_Stmt (Info.Label_Exit);
            Info.Label_Next := O_Snode_Null;

            if Cond /= Null_Iir then
               Open_Temp;
               Gen_Exit_When
                 (Info.Label_Exit,
                  New_Monadic_Op (ON_Not, Translate_Condition (Cond)));
               Close_Temp;
            end if;

            Translate_Statements_Chain (Get_Sequential_Statement_Chain (Stmt));

            Finish_Loop_Stmt (Info.Label_Exit);
         end;
      end if;

      Free_Info (Stmt);
      Current_Loop := Prev_Loop;
   end Translate_While_Loop_Statement;

   procedure Translate_Exit_Next_Statement (Stmt : Iir)
   is
      Cond       : constant Iir := Get_Condition (Stmt);
      If_Blk     : O_If_Block;
      Info       : Ortho_Info_Acc;
      Loop_Label : Iir;
      Loop_Stmt  : Iir;
   begin
      Loop_Label := Get_Loop_Label (Stmt);
      if Loop_Label = Null_Iir then
         Loop_Stmt := Current_Loop;
      else
         Loop_Stmt := Get_Named_Entity (Loop_Label);
      end if;

      Info := Get_Info (Loop_Stmt);

      --  Common part.
      if Cond /= Null_Iir then
         Start_If_Stmt (If_Blk, Translate_Condition (Cond));
      end if;

      if Get_Suspend_Flag (Loop_Stmt) then
         --  The corresponding loop is state based.  Jump to the right state.
         case Get_Kind (Stmt) is
            when Iir_Kind_Exit_Statement =>
               State_Jump (Info.Loop_State_Exit);
            when Iir_Kind_Next_Statement =>
               State_Jump (Info.Loop_State_Next);
            when others =>
               raise Internal_Error;
         end case;

         --  Force the jump, so that it would work even if the next/exit is
         --  not immediately within a state construct.  Example:
         --    loop
         --      if cond then
         --        exit;
         --      else
         --        i := i + 1;
         --      end if;
         --      wait for 1 ns;
         --    end loop;
         --  A new state cannot be created here, as the outer construct is the
         --  if statement and not the case statement for the state machine.
         State_Jump_Force;
      else
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
      end if;

      if Cond /= Null_Iir then
         Finish_If_Stmt (If_Blk);
      end if;
   end Translate_Exit_Next_Statement;

   procedure Translate_Variable_Aggregate_Assignment
     (Targ : Iir; Targ_Type : Iir; Val : Mnode);

   procedure Translate_Variable_Array_Aggr_Final
     (Choice : Iir; Targ_Type : Iir; Val : Mnode; Index : O_Dnode)
   is
      Targ : constant Iir := Get_Associated_Expr (Choice);
      Sub_Aggr  : Mnode;
      Sub_Type  : Iir;
      Dest : Mnode;
   begin
      if Get_Element_Type_Flag (Choice) then
         Sub_Aggr := Chap3.Index_Base (Chap3.Get_Composite_Base (Val),
                                       Targ_Type, New_Obj_Value (Index));
         Sub_Type := Get_Element_Subtype (Targ_Type);
         Translate_Variable_Aggregate_Assignment (Targ, Sub_Type, Sub_Aggr);
         Inc_Var (Index);
      else
         Sub_Type := Get_Type (Targ);
         Sub_Aggr := Chap3.Slice_Base (Chap3.Get_Composite_Base (Val),
                                       Sub_Type, New_Obj_Value (Index),
                                       O_Enode_Null);
         Stabilize (Sub_Aggr);
         Dest := Chap6.Translate_Name (Targ, Mode_Value);
         Stabilize (Dest);
         Gen_Memcpy (M2Addr (Chap3.Get_Composite_Base (Dest)),
                     M2Addr (Sub_Aggr),
                     Chap3.Get_Object_Size (Dest, Sub_Type));
         New_Assign_Stmt
           (New_Obj (Index),
            New_Dyadic_Op (ON_Add_Ov,
                           New_Obj_Value (Index),
                           Chap3.Get_Array_Length (Dest, Sub_Type)));
      end if;
   end Translate_Variable_Array_Aggr_Final;

   procedure Translate_Variable_Array_Aggr (Targ      : Iir_Aggregate;
                                            Targ_Type : Iir;
                                            Val       : Mnode;
                                            Index     : O_Dnode;
                                            Dim       : Natural)
   is
      Choice  : Iir;
      Final   : Boolean;
   begin
      Final := Dim = Get_Nbr_Elements (Get_Index_Subtype_List (Targ_Type));
      Choice := Get_Association_Choices_Chain (Targ);
      while Choice /= Null_Iir loop
         case Get_Kind (Choice) is
            when Iir_Kind_Choice_By_None =>
               if Final then
                  Translate_Variable_Array_Aggr_Final
                    (Choice, Targ_Type, Val, Index);
               else
                  Translate_Variable_Array_Aggr
                    (Get_Associated_Expr (Choice),
                     Targ_Type, Val, Index, Dim + 1);
               end if;
            when others =>
               Error_Kind ("translate_variable_array_aggr", Choice);
         end case;
         Choice := Get_Chain (Choice);
      end loop;
   end Translate_Variable_Array_Aggr;

   procedure Translate_Variable_Rec_Aggr
     (Targ : Iir_Aggregate; Targ_Type : Iir; Val : Mnode)
   is
      El_List : constant Iir_Flist :=
        Get_Elements_Declaration_List (Get_Base_Type (Targ_Type));
      Aggr_El  : Iir;
      El_Index : Natural;
      Elem     : Iir;
   begin
      El_Index := 0;
      Aggr_El := Get_Association_Choices_Chain (Targ);
      while Aggr_El /= Null_Iir loop
         case Get_Kind (Aggr_El) is
            when Iir_Kind_Choice_By_None =>
               Elem := Get_Nth_Element (El_List, El_Index);
               El_Index := El_Index + 1;
            when Iir_Kind_Choice_By_Name =>
               Elem := Get_Named_Entity (Get_Choice_Name (Aggr_El));
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
     (Targ : Iir; Targ_Type : Iir; Val : Mnode) is
   begin
      if Get_Kind (Targ) = Iir_Kind_Aggregate then
         case Get_Kind (Targ_Type) is
            when Iir_Kinds_Array_Type_Definition =>
               declare
                  Index : O_Dnode;
               begin
                  Index := Create_Temp (Ghdl_Index_Type);
                  Init_Var (Index);
                  Translate_Variable_Array_Aggr
                    (Targ, Targ_Type, Val, Index, 1);
               end;
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
            Targ_Node := Chap6.Translate_Name (Targ, Mode_Value);
            Chap3.Translate_Object_Copy (Targ_Node, Val, Targ_Type);
         end;
      end if;
   end Translate_Variable_Aggregate_Assignment;

   function Aggregate_Overlap_Variable (Aggr : Iir; Name : Iir) return Boolean
   is
      Assoc : Iir;
      Expr : Iir;
   begin
      Assoc := Get_Association_Choices_Chain (Aggr);
      while Assoc /= Null_Iir loop
         Expr := Get_Associated_Expr (Assoc);
         if Get_Kind (Expr) = Iir_Kind_Aggregate then
            if Aggregate_Overlap_Variable (Expr, Name) then
               return True;
            end if;
         else
            Expr := Get_Base_Name (Expr);
            if Expr = Name then
               return True;
            end if;
         end if;
         Assoc := Get_Chain (Assoc);
      end loop;
      return False;
   end Aggregate_Overlap_Variable;

   function Aggregate_Overlap_Dereference (Aggr : Iir; Atype : Iir)
                                          return Boolean
   is
      Assoc : Iir;
      Expr : Iir;
   begin
      Assoc := Get_Association_Choices_Chain (Aggr);
      while Assoc /= Null_Iir loop
         Expr := Get_Associated_Expr (Assoc);
         if Get_Kind (Expr) = Iir_Kind_Aggregate then
            if Aggregate_Overlap_Dereference (Expr, Atype) then
               return True;
            end if;
         else
            Expr := Get_Base_Name (Expr);
            if Get_Kind (Expr) in Iir_Kinds_Dereference
              and then Get_Base_Type (Get_Type (Expr)) = Atype
            then
               return True;
            end if;
         end if;
         Assoc := Get_Chain (Assoc);
      end loop;
      return False;
   end Aggregate_Overlap_Dereference;

   --  Return true if there is a possible overlap between source and
   --  target in an assignment whose target is an aggregate.
   function Assignment_Overlap (Targ : Iir; Expr : Iir) return Boolean
   is
      Base : Iir;
   begin
      Base := Expr;

      --  Strip qualified expression/parenthesis/type conversion.  Although
      --  they are expression, code generation doesn't copy the value.
      loop
         case Get_Kind (Base) is
            when Iir_Kind_Qualified_Expression
              | Iir_Kind_Parenthesis_Expression
              | Iir_Kind_Type_Conversion =>
               Base := Get_Expression (Base);
            when others =>
               exit;
         end case;
      end loop;

      case Get_Kind (Base) is
         when Iir_Kinds_Name =>
            Base := Get_Base_Name (Base);
         when Iir_Kinds_Dereference =>
            null;
         when others =>
            --  An expression.
            return False;
      end case;

      case Get_Kind (Base) is
         when Iir_Kinds_Dereference =>
            --  FIXME: cannot overlap as aggregate is composed of locally
            --  static names that denote variables.
            return Aggregate_Overlap_Dereference
              (Targ, Get_Base_Type (Get_Type (Base)));
         when Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Variable_Declaration =>
            return Aggregate_Overlap_Variable (Targ, Base);
         when Iir_Kind_External_Variable_Name =>
            return True;
         when others =>
            return False;
      end case;
   end Assignment_Overlap;

   --  Return True if AGGR can be easily assigned.
   --  Currently: is of the form (others => VAL) where VAL is static.
   function Is_Aggregate_Loop (Aggr : Iir) return Boolean
   is
      Chain : Iir;
      Assoc : Iir;
   begin
      pragma Assert (Get_Kind (Aggr) = Iir_Kind_Aggregate);
      Chain := Get_Association_Choices_Chain (Aggr);
      if not Is_Chain_Length_One (Chain)
        or else Get_Kind (Chain) /= Iir_Kind_Choice_By_Others
      then
         return False;
      end if;
      Assoc := Get_Associated_Expr (Chain);
      return Get_Expr_Staticness (Assoc) >= Globally;
   end Is_Aggregate_Loop;

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
            E    : Mnode;
            Temp : Mnode;
         begin
            --  According to LRM08 9.3.3.3 Array aggregates, the expression
            --  cannot depend on the target aggregate, so it can be evaluated
            --  directly.  In other words, it shouldn't be an aggregate with
            --  'others'.
            --  TODO: Because the aggregate is composed only of locally static
            --  variable names, it is possible to compute the bounds and check
            --  matching constraints.
            Chap3.Translate_Anonymous_Subtype_Definition (Targ_Type, False);
            E := Chap7.Translate_Expression (Expr, Targ_Type);

            if Assignment_Overlap (Target, Expr) then
               --  Use a temporary variable, to avoid overlap.
               Temp := Create_Temp (Get_Info (Targ_Type));
               Chap4.Allocate_Complex_Object (Targ_Type, Alloc_Stack, Temp);

               Chap3.Translate_Object_Copy (Temp, E, Targ_Type);
               E := Temp;
            else
               --  FIXME: check bounds.
               Stabilize (E);
            end if;
            Translate_Variable_Aggregate_Assignment (Target, Targ_Type, E);
            return;
         end;
      else
         Targ_Node := Chap6.Translate_Name (Target, Mode_Value);
         if Get_Kind (Expr) = Iir_Kind_Aggregate then
            if Is_Aggregate_Loop (Expr) then
               Chap7.Translate_Aggregate (Targ_Node, Targ_Type, Expr);
            elsif Get_Constraint_State (Get_Type (Expr)) /= Fully_Constrained
            then
               declare
                  Expr_Type : constant Iir := Get_Type (Expr);
                  Expr_Tinfo : constant Type_Info_Acc := Get_Info (Expr_Type);
                  Val : Mnode;
               begin
                  --  Create a temp.
                  Val := Create_Temp (Expr_Tinfo);
                  --  Set bounds from target
                  Stabilize (Targ_Node);
                  New_Assign_Stmt
                    (M2Lp (Chap3.Get_Composite_Bounds (Val)),
                     M2Addr (Chap3.Get_Composite_Bounds (Targ_Node)));
                  --  Allocate target
                  Chap3.Allocate_Unbounded_Composite_Base
                    (Alloc_Stack, Val, Get_Base_Type (Expr_Type));
                  --  Translate aggregate
                  Chap7.Translate_Aggregate (Val, Targ_Type, Expr);
                  --  Assign
                  Chap3.Translate_Object_Copy (Targ_Node, Val, Targ_Type);
               end;
            else
               --  In case of overlap: be sure to use an intermediate variable.
               declare
                  E : Mnode;
               begin
                  E := Chap7.Translate_Expression (Expr, Targ_Type);
                  Chap3.Translate_Object_Copy (Targ_Node, E, Targ_Type);
               end;
            end if;
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
         Severity := New_Lit (Get_Ortho_Literal (Level));
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
      if Get_Expr_Staticness (Expr) = Locally
        and then not Is_Overflow_Literal (Expr)
      then
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
      Func_Info : Operator_Info_Acc;
   begin
      New_Assign_Stmt (New_Selected_Element (New_Obj (Val_Node),
                                             Tinfo.B.Base_Field (Mode_Value)),
                       New_Convert (Val, Tinfo.B.Base_Ptr_Type (Mode_Value)));
      Func_Info := Get_Info (Func);
      Start_Association (Assoc, Func_Info.Operator_Node);
      Subprgs.Add_Subprg_Instance_Assoc (Assoc, Func_Info.Operator_Instance);
      New_Association (Assoc, New_Obj_Value (Expr));
      New_Association (Assoc, New_Address (New_Obj (Val_Node),
                                           Tinfo.Ortho_Ptr_Type (Mode_Value)));
      return New_Function_Call (Assoc);
   end Translate_Simple_String_Choice;

   --  Helper to evaluate the selector and preparing a choice variable.
   --  LEN_TYPE is the type that contains the locally static bounds.  It is in
   --  general the type of the expression (selector) or of the first choice if
   --  the selector type is not locally static.
   procedure Translate_String_Case_Statement_Common
     (Stmt       : Iir_Case_Statement;
      Choices    : Iir;
      Len_Type   : out Iir;
      Base_Type  : out Iir;
      Expr_Node  : out O_Dnode;
      C_Node     : out O_Dnode)
   is
      Expr       : constant Iir := Get_Expression (Stmt);
      Expr_Type  : Iir;
      Tinfo      : Type_Info_Acc;
      Sel_Length : Int64;
      Cond       : O_Enode;
   begin
      --  Translate into if/elsif statements.
      --  FIXME: if the number of literals ** length of the array < 256,
      --   use a case statement.
      Expr_Type := Get_Type (Expr);
      Base_Type := Get_Base_Type (Expr_Type);
      Tinfo := Get_Info (Base_Type);
      Len_Type := Expr_Type;

      --  Translate selector.
      Expr_Node := Create_Temp_Init
        (Tinfo.Ortho_Ptr_Type (Mode_Value),
         Chap7.Translate_Expression (Expr, Base_Type));

      --  Copy the bounds for the choices.
      C_Node := Create_Temp (Tinfo.Ortho_Type (Mode_Value));
      New_Assign_Stmt
        (New_Selected_Element (New_Obj (C_Node),
         Tinfo.B.Bounds_Field (Mode_Value)),
         New_Value_Selected_Acc_Value
           (New_Obj (Expr_Node), Tinfo.B.Bounds_Field (Mode_Value)));

      --  LRM08 10.9 Case statement
      --  In all cases, it is an error if the value of the expression is not of
      --  the same length as the values of the choices.
      if Get_Type_Staticness (Len_Type) /= Locally
        and then Get_Kind (Choices) = Iir_Kind_Choice_By_Expression
      then
         Len_Type := Get_Type (Get_Choice_Expression (Choices));
         pragma Assert (Get_Base_Type (Len_Type) = Base_Type);
         Sel_Length := Eval_Discrete_Type_Length
           (Get_String_Type_Bound_Type (Len_Type));
         Cond := New_Compare_Op
           (ON_Neq,
            Chap3.Get_Array_Length
              (Dp2M (Expr_Node, Get_Info (Expr_Type), Mode_Value),
               Expr_Type),
            New_Lit (New_Index_Lit (Unsigned_64 (Sel_Length))),
            Ghdl_Bool_Type);
         Chap6.Check_Bound_Error (Cond, Expr);
      end if;
   end Translate_String_Case_Statement_Common;

   type Choice_Id is new Integer;
   No_Choice_Id : constant Choice_Id := -1;

   type Choice_Info_Type is record
      --  List of choices, used to sort them.
      Choice_Chain  : Choice_Id;
      --  Association index.
      Choice_Assoc  : Natural;
      --  Corresponding choice simple expression.
      Choice_Expr   : Iir;
      --  Corresponding choice.
      Choice_Parent : Iir;
   end record;

   type Choice_Info_Arr is array (Choice_Id range <>) of Choice_Info_Type;

   --  Translate a string case statement using a dichotomy.
   --  NBR_CHOICES is the number of non-others choices.
   procedure Translate_String_Case_Statement_Dichotomy
     (Stmt : Iir;
      Choices_Chain : Iir;
      Nbr_Choices : Positive;
      Choices_Info : in out Choice_Info_Arr;
      Handler : in out Case_Handler'Class)
   is
      First, Last : Choice_Id;
      El          : Choice_Id;
      Base_Type   : Iir;

      --  Selector.
      Tinfo     : Type_Info_Acc;
      Expr_Node : O_Dnode;
      C_Node    : O_Dnode;
      Var_Idx   : O_Dnode;
      Others_Lit : O_Cnode;

      Len_Type   : Iir;
      Choice     : Iir;
      Has_Others : Boolean;
      Func       : Iir;

      --  Number of associations.
      Nbr_Assocs  : Natural;

      Sel_Length  : Int64;

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
      --  Fill Choices_Info array, and count number of associations.
      Last := No_Choice_Id;
      Nbr_Assocs := 0;
      Has_Others := False;
      Choice := Choices_Chain;
      while Choice /= Null_Iir loop
         if Get_Kind (Choice) = Iir_Kind_Choice_By_Others then
            Has_Others := True;
            exit;
         end if;
         pragma Assert (Get_Kind (Choice) = Iir_Kind_Choice_By_Expression);
         if not Get_Same_Alternative_Flag (Choice) then
            Nbr_Assocs := Nbr_Assocs + 1;
         end if;
         Last := Last + 1;
         Choices_Info (Last) :=
           (Choice_Chain => Last + 1,
            Choice_Assoc => Nbr_Assocs - 1,
            Choice_Parent => Choice,
            Choice_Expr => Get_Choice_Expression (Choice));
         Choice := Get_Chain (Choice);
      end loop;

      --  There is at most one choice (otherwise the linear algorithm must
      --  have been used).
      pragma Assert (Last /= No_Choice_Id);
      First := 0;
      Choices_Info (Last).Choice_Chain := No_Choice_Id;

      --  Sort choices.
      declare
         procedure Merge_Sort (Head : Choice_Id;
                               Nbr  : Natural;
                               Res  : out Choice_Id;
                               Next : out Choice_Id)
         is
            L, R, L_End, R_End : Choice_Id;
            E, Last            : Choice_Id;
            Half               : constant Natural := Nbr / 2;
         begin
            --  Sorting less than 2 elements is easy!
            if Nbr < 2 then
               Res := Head;
               if Nbr = 0 then
                  Next := Head;
               else
                  Next := Choices_Info (Head).Choice_Chain;
               end if;
               return;
            end if;

            --  Split in two and sort.
            Merge_Sort (Head, Half, L, L_End);
            Merge_Sort (L_End, Nbr - Half, R, R_End);
            Next := R_End;

            --  Merge
            Last := No_Choice_Id;
            loop
               if L /= L_End
                 and then
                   (R = R_End
                    or else
                      Compare_String_Literals (Choices_Info (L).Choice_Expr,
                                               Choices_Info (R).Choice_Expr)
                      = Compare_Lt)
               then
                  --  Pick L.
                  E := L;
                  L := Choices_Info (L).Choice_Chain;
               elsif R /= R_End then
                  --  Pick R.
                  E := R;
                  R := Choices_Info (R).Choice_Chain;
               else
                  exit;
               end if;
               --  Append.
               if Last = No_Choice_Id then
                  Res := E;
               else
                  Choices_Info (Last).Choice_Chain := E;
               end if;
               Last := E;
            end loop;
            Choices_Info (Last).Choice_Chain := R_End;
         end Merge_Sort;
      begin
         Merge_Sort (First, Nbr_Choices, First, Last);
         pragma Assert (Last = No_Choice_Id);
      end;

      Open_Temp;
      Translate_String_Case_Statement_Common
        (Stmt, Choices_Chain, Len_Type, Base_Type, Expr_Node, C_Node);

      Tinfo := Get_Info (Base_Type);

      --  Generate the sorted array of choices.
      Sel_Length := Eval_Discrete_Type_Length
        (Get_String_Type_Bound_Type (Len_Type));

      String_Type := New_Array_Subtype
        (Tinfo.B.Base_Type (Mode_Value),
         Get_Ortho_Type (Get_Element_Subtype (Base_Type), Mode_Value),
         New_Index_Lit (Unsigned_64 (Sel_Length)));
      Table_Base_Type := New_Array_Type (String_Type, Ghdl_Index_Type);
      New_Type_Decl (Create_Uniq_Identifier, Table_Base_Type);
      Table_Type := New_Array_Subtype
        (Table_Base_Type,
         String_Type, New_Index_Lit (Unsigned_64 (Nbr_Choices)));
      New_Const_Decl (Table, Create_Uniq_Identifier, O_Storage_Private,
                      Table_Type);
      Start_Init_Value (Table);
      Start_Array_Aggr (List, Table_Type, Unsigned_32 (Nbr_Choices));

      El := First;
      while El /= No_Choice_Id loop
         New_Array_Aggr_El (List, Chap7.Translate_Static_Expression
                              (Choices_Info (El).Choice_Expr, Len_Type));
         El := Choices_Info (El).Choice_Chain;
      end loop;
      Finish_Array_Aggr (List, Table_Cst);
      Finish_Init_Value (Table, Table_Cst);

      --  Generate table from choice to statements block.
      Assoc_Table_Base_Type :=
        New_Array_Type (Ghdl_Index_Type, Ghdl_Index_Type);
      New_Type_Decl (Create_Uniq_Identifier, Assoc_Table_Base_Type);
      Assoc_Table_Type := New_Array_Subtype
        (Assoc_Table_Base_Type,
         Ghdl_Index_Type, New_Index_Lit (Unsigned_64 (Nbr_Choices)));
      New_Const_Decl (Assoc_Table, Create_Uniq_Identifier,
                      O_Storage_Private, Assoc_Table_Type);
      Start_Init_Value (Assoc_Table);
      Start_Array_Aggr
        (List, Assoc_Table_Type, Unsigned_32 (Nbr_Choices));
      El := First;
      while El /= No_Choice_Id loop
         New_Array_Aggr_El
           (List, New_Unsigned_Literal
              (Ghdl_Index_Type,
               Unsigned_64 (Choices_Info (El).Choice_Assoc)));
         El := Choices_Info (El).Choice_Chain;
      end loop;
      Finish_Array_Aggr (List, Table_Cst);
      Finish_Init_Value (Assoc_Table, Table_Cst);

      --  Generate dichotomy code.
      declare
         Var_Lo, Var_Hi, Var_Mid : O_Dnode;
         Var_Cmp                 : O_Dnode;
         Label                   : O_Snode;
         If_Blk1, If_Blk2        : O_If_Block;
      begin
         Var_Idx := Create_Temp (Ghdl_Index_Type);

         --  Declare Lo, Hi, Mid, Cmp.
         Start_Declare_Stmt;

         New_Var_Decl (Var_Lo, Wki_Lo, O_Storage_Local, Ghdl_Index_Type);
         New_Var_Decl (Var_Hi, Wki_Hi, O_Storage_Local, Ghdl_Index_Type);
         New_Var_Decl (Var_Mid, Wki_Mid, O_Storage_Local, Ghdl_Index_Type);
         New_Var_Decl (Var_Cmp, Wki_Cmp,
                       O_Storage_Local, Ghdl_Compare_Type);

         --  Generate:
         --    Lo := 0;
         --    Hi := Nbr_Choices - 1;
         New_Assign_Stmt (New_Obj (Var_Lo), New_Lit (Ghdl_Index_0));
         New_Assign_Stmt
           (New_Obj (Var_Hi),
            New_Lit (New_Unsigned_Literal (Ghdl_Index_Type,
                                           Unsigned_64 (Nbr_Choices - 1))));

         Func := Chap7.Find_Predefined_Function
           (Get_Base_Type (Len_Type), Iir_Predefined_Array_Greater);

         if Has_Others then
            Others_Lit := New_Unsigned_Literal
              (Ghdl_Index_Type, Unsigned_64 (Nbr_Assocs));
         end if;

         --  Generate:
         --    loop
         --       Mid := (Lo + Hi) / 2;
         --       Cmp := COMPARE (Expr, Table[Mid]);
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
                            Tinfo.B.Base_Ptr_Type (Mode_Value)),
               C_Node, Tinfo, Func));

         --  Generate:
         --       if Cmp = Eq then
         --         Idx := Mid;
         --         exit;
         --       end if;
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

         --  Generate:
         --       if Cmp = Lt then
         --         if Mid < Lo then
         --           Idx := others;
         --           exit;
         --         else
         --           Hi := Mid - 1;
         --         end if;
         --       else
         --         if Mid > Hi then
         --           Idx := others;
         --           exit;
         --         else
         --           Lo := Mid + 1;
         --         end if;
         --       end if;
         --    end loop;
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
      end;

      --  Generate:
      --    case Idx is
      --      when ch1
      --         | ch2 => stmt_list1;
      --      when ch3 => stmt_list2;
      --      ...
      --    end case;
      declare
         Case_Blk : O_Case_Block;
      begin
         Start_Case_Stmt (Case_Blk, New_Obj_Value (Var_Idx));

         Nbr_Assocs := 0;
         Choice := Choices_Chain;
         while Choice /= Null_Iir loop
            case Get_Kind (Choice) is
               when Iir_Kind_Choice_By_Others =>
                  Start_Choice (Case_Blk);
                  New_Expr_Choice (Case_Blk, Others_Lit);
                  Finish_Choice (Case_Blk);
                  Case_Association_Cb (Get_Associated_Chain (Choice), Handler);
               when Iir_Kind_Choice_By_Expression =>
                  if not Get_Same_Alternative_Flag (Choice) then
                     Start_Choice (Case_Blk);
                     New_Expr_Choice
                       (Case_Blk,
                        New_Unsigned_Literal
                          (Ghdl_Index_Type, Unsigned_64 (Nbr_Assocs)));
                     Finish_Choice (Case_Blk);
                     Case_Association_Cb
                       (Get_Associated_Chain (Choice), Handler);
                     if not Get_Same_Alternative_Flag (Choice) then
                        Nbr_Assocs := Nbr_Assocs + 1;
                     end if;
                  end if;
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
         Close_Temp;
      end;
   end Translate_String_Case_Statement_Dichotomy;

   --  Case statement whose expression is an unidim array.
   --  Translate into if/elsif statements (linear search).
   procedure Translate_String_Case_Statement_Linear
     (Stmt : Iir; Choices : Iir; Handler : in out Case_Handler'Class)
   is
      Len_Type  : Iir;
      --  Node containing the address of the selector.
      Expr_Node : O_Dnode;
      --  Node containing the current choice.
      Val_Node  : O_Dnode;
      Base_Type : Iir;
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
                  Case_Association_Cb (Stmt_Chain, Handler);
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
         Case_Association_Cb (Stmt_Chain, Handler);
         New_Else_Stmt (If_Blk);
         Translate_String_Choice (Ch);
         Finish_If_Stmt (If_Blk);
      end Translate_String_Choice;
   begin
      Open_Temp;
      Translate_String_Case_Statement_Common
        (Stmt, Choices, Len_Type, Base_Type, Expr_Node, Val_Node);
      Tinfo := Get_Info (Base_Type);

      Func := Chap7.Find_Predefined_Function
        (Get_Base_Type (Len_Type), Iir_Predefined_Array_Equality);

      Cond_Var := Create_Temp (Std_Boolean_Type_Node);

      Translate_String_Choice (Choices);
      Close_Temp;
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
               Expr := Get_Range_From_Discrete_Range (Expr);
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

   procedure Translate_Case (N : Iir; Handler : in out Case_Handler'Class)
   is
      Expr : constant Iir := Get_Expression (N);
      Expr_Type : constant Iir := Get_Type (Expr);
      Choices : Iir;
   begin
      --  Get the chain of choices.
      case Get_Kind (N) is
         when Iir_Kind_Case_Statement =>
            Choices := Get_Case_Statement_Alternative_Chain (N);
         when Iir_Kind_Selected_Waveform_Assignment_Statement =>
            Choices := Get_Selected_Waveform_Chain (N);
         when others =>
            Error_Kind ("translate_case", N);
      end case;

      if Get_Kind (Expr_Type) in Iir_Kinds_Array_Type_Definition then
         --  Expression is a one-dimensional array.
         declare
            Nbr_Choices : Natural := 0;
            Choice      : Iir;
         begin
            --  Count number of choices.
            Choice := Choices;
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

            --  Select the strategy according to the number of choices.
            if Nbr_Choices < 3 then
               Translate_String_Case_Statement_Linear (N, Choices, Handler);
            elsif Nbr_Choices <= 512 then
               --  Can allocate on the stack.
               declare
                  subtype Valid_Choice_Id is Choice_Id
                    range 0 .. Choice_Id (Nbr_Choices - 1);
                  Choices_Info : Choice_Info_Arr (Valid_Choice_Id);
               begin
                  Translate_String_Case_Statement_Dichotomy
                    (N, Choices, Nbr_Choices, Choices_Info, Handler);
               end;
            else
               --  Allocate on the heap.
               declare
                  type Choice_Info_Arr_Acc is access Choice_Info_Arr;
                  subtype Valid_Choice_Id is Choice_Id
                    range 0 .. Choice_Id (Nbr_Choices - 1);
                  Choices_Info : Choice_Info_Arr_Acc;
                  procedure Free is new Ada.Unchecked_Deallocation
                    (Choice_Info_Arr, Choice_Info_Arr_Acc);
               begin
                  Choices_Info := new Choice_Info_Arr (Valid_Choice_Id);
                  Translate_String_Case_Statement_Dichotomy
                    (N, Choices, Nbr_Choices, Choices_Info.all, Handler);
                  Free (Choices_Info);
               end;
            end if;
         end;
      else
         --  Normal case statement: expression is discrete.
         declare
            Case_Blk   : O_Case_Block;
            Choice     : Iir;
            Stmt_Chain : Iir;
         begin
            Start_Case_Stmt (Case_Blk, Chap7.Translate_Expression (Expr));
            Choice := Choices;
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
               Case_Association_Cb (Stmt_Chain, Handler);
            end loop;
            Finish_Case_Stmt (Case_Blk);
         end;
      end if;
   end Translate_Case;

   --  Handler for a case statement.
   type Case_Statement_Handler is new Case_Handler with record
      --  True if there is a suspend statement in the case statement.
      Has_Suspend : Boolean;

      --  State after the case statement.  Set only if Has_Suspend is true.
      Next_State : State_Type;
   end record;

   procedure Case_Association_Cb (Assoc : Iir;
                                  Handler : in out Case_Statement_Handler)
   is
      Choice_State  : State_Type;
   begin
      if Handler.Has_Suspend then
         --  Jump to the corresponding state.
         Choice_State := State_Allocate;
         State_Jump (Choice_State);
      else
         --  Execute the statements.
         Translate_Statements_Chain (Assoc);
      end if;
   end Case_Association_Cb;

   procedure Translate_Case_Statement (Stmt : Iir_Case_Statement)
   is
      Handler : Case_Statement_Handler;
   begin
      --  Initialize handler.
      Handler.Has_Suspend := Get_Suspend_Flag (Stmt);
      if Handler.Has_Suspend then
         Handler.Next_State := State_Allocate;
      end if;

      --  Translate the case statement.
      Translate_Case (Stmt, Handler);

      if Handler.Has_Suspend then
         --  Translate only the statements in choice.  The state after the
         --  whole case statement is NEXT_STATE, the state for the choices
         --  are NEXT_STATE + 1 .. NEXT_STATE + nbr_choices.
         declare
            Choice : Iir;
            Choice_State  : State_Type;
         begin
            Choice_State := Handler.Next_State;
            Choice := Get_Case_Statement_Alternative_Chain (Stmt);
            while Choice /= Null_Iir loop
               if not Get_Same_Alternative_Flag (Choice) then
                  Choice_State := Choice_State + 1;
                  State_Start (Choice_State);
                  Translate_Statements_Chain (Get_Associated_Chain (Choice));
                  State_Jump (Handler.Next_State);
               end if;
               Choice := Get_Chain (Choice);
            end loop;
            State_Start (Handler.Next_State);
         end;
      end if;
   end Translate_Case_Statement;

   procedure Translate_Write_Procedure_Call (Imp : Iir; Param_Chain : Iir)
   is
      Inter_Chain : constant Iir := Get_Interface_Declaration_Chain (Imp);
      F_Assoc     : constant Iir := Param_Chain;
      Value_Assoc : constant Iir := Get_Chain (Param_Chain);
      Value_Inter : constant Iir := Get_Chain (Inter_Chain);
      Formal_Type : constant Iir := Get_Type (Value_Inter);
      Tinfo       : constant Type_Info_Acc := Get_Info (Formal_Type);
      Value       : O_Dnode;
      Assocs      : O_Assoc_List;
      Subprg_Info : Operator_Info_Acc;
   begin
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
         when Type_Mode_Bounded_Arrays
           | Type_Mode_Bounded_Records
           | Type_Mode_Unbounded_Array =>
            Subprg_Info := Get_Info (Imp);
            Start_Association (Assocs, Subprg_Info.Operator_Node);
            Subprgs.Add_Subprg_Instance_Assoc
              (Assocs, Subprg_Info.Operator_Instance);
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
           | Type_Mode_Bounds_Acc
           | Type_Mode_Unbounded_Record
           | Type_Mode_Protected =>
            raise Internal_Error;
      end case;
   end Translate_Write_Procedure_Call;

   procedure Translate_Read_Procedure_Call (Imp : Iir; Param_Chain : Iir)
   is
      Inter_Chain : constant Iir := Get_Interface_Declaration_Chain (Imp);
      F_Assoc     : constant Iir := Param_Chain;
      Value_Assoc : constant Iir := Get_Chain (Param_Chain);
      Value_Inter : constant Iir := Get_Chain (Inter_Chain);
      Formal_Type : constant Iir := Get_Type (Value_Inter);
      Tinfo       : constant Type_Info_Acc := Get_Info (Formal_Type);
      Value       : Mnode;
      Assocs      : O_Assoc_List;
      Subprg_Info : Operator_Info_Acc;
   begin
      case Tinfo.Type_Mode is
         when Type_Mode_Scalar =>
            Open_Temp;
            Start_Association (Assocs, Ghdl_Read_Scalar);
            --    compute file parameter (get an index)
            New_Association
              (Assocs, Chap7.Translate_Expression (Get_Actual (F_Assoc)));
            --  value
            Value :=
              Chap6.Translate_Name (Get_Actual (Value_Assoc), Mode_Value);
            New_Association
              (Assocs, New_Convert_Ov (M2Addr (Value), Ghdl_Ptr_Type));
            --    length.
            New_Association
              (Assocs, New_Lit (New_Sizeof (Tinfo.Ortho_Type (Mode_Value),
               Ghdl_Index_Type)));
            --    call a predefined procedure
            New_Procedure_Call (Assocs);
            Close_Temp;
         when Type_Mode_Bounded_Arrays
            | Type_Mode_Bounded_Records =>
            Subprg_Info := Get_Info (Imp);
            Start_Association (Assocs, Subprg_Info.Operator_Node);
            Subprgs.Add_Subprg_Instance_Assoc
              (Assocs, Subprg_Info.Operator_Instance);
            New_Association
              (Assocs, Chap7.Translate_Expression (Get_Actual (F_Assoc)));
            New_Association
              (Assocs,
               Chap7.Translate_Expression (Get_Actual (Value_Assoc)));
            New_Procedure_Call (Assocs);
         when Type_Mode_Unbounded_Array =>
            declare
               Length_Assoc : Iir;
               Length       : Mnode;
            begin
               Length_Assoc := Get_Chain (Value_Assoc);
               Subprg_Info := Get_Info (Imp);
               Start_Association (Assocs, Subprg_Info.Operator_Node);
               Subprgs.Add_Subprg_Instance_Assoc
                 (Assocs, Subprg_Info.Operator_Instance);
               New_Association
                 (Assocs,
                  Chap7.Translate_Expression (Get_Actual (F_Assoc)));
               New_Association
                 (Assocs,
                  Chap7.Translate_Expression (Get_Actual (Value_Assoc),
                    Formal_Type));
               Length :=
                 Chap6.Translate_Name (Get_Actual (Length_Assoc), Mode_Value);
               New_Assign_Stmt (M2Lv (Length), New_Function_Call (Assocs));
            end;
         when Type_Mode_Unknown
           | Type_Mode_File
           | Type_Mode_Acc
           | Type_Mode_Bounds_Acc
           | Type_Mode_Unbounded_Record
           | Type_Mode_Protected =>
            raise Internal_Error;
      end case;
   end Translate_Read_Procedure_Call;

   procedure Translate_Implicit_Procedure_Call (Call : Iir_Procedure_Call)
   is
      Imp         : constant Iir := Get_Implementation (Call);
      Kind        : constant Iir_Predefined_Functions :=
        Get_Implicit_Definition (Imp);
      Assoc_Chain : constant Iir := Get_Parameter_Association_Chain (Call);
      Inter_Chain : constant Iir := Get_Interface_Declaration_Chain (Imp);
   begin
      case Kind is
         when Iir_Predefined_Write =>
            declare
               File_Assoc : constant Iir := Assoc_Chain;
               File_Param : constant Iir := Get_Actual (File_Assoc);
               Value_Assoc : constant Iir := Get_Chain (File_Assoc);
               Value_Param : constant Iir := Get_Actual (Value_Assoc);
               Assocs     : O_Assoc_List;
            begin
               --  Check whether text or not.
               if Get_Text_File_Flag (Get_Type (File_Param)) then
                  --  If text:
                  Start_Association (Assocs, Ghdl_Text_Write);
                  --    compute file parameter (get an index)
                  New_Association
                    (Assocs, Chap7.Translate_Expression (File_Param));
                  --    compute string parameter (get a fat array pointer)
                  New_Association
                    (Assocs, Chap7.Translate_Expression
                       (Value_Param, String_Type_Definition));
                  --    call a predefined procedure
                  New_Procedure_Call (Assocs);
               else
                  Translate_Write_Procedure_Call (Imp, Assoc_Chain);
               end if;
            end;

         when Iir_Predefined_Read_Length =>
            --  FIXME: works only for text read length.
            declare
               File_Assoc : constant Iir := Assoc_Chain;
               File_Param : constant Iir := Get_Actual (File_Assoc);
               N_Assoc    : Iir;
               Assocs     : O_Assoc_List;
               Str        : O_Enode;
               Res        : Mnode;
            begin
               if Get_Text_File_Flag (Get_Type (File_Param)) then
                  N_Assoc := Get_Chain (File_Assoc);
                  Str := Chap7.Translate_Expression
                    (Get_Actual (N_Assoc), String_Type_Definition);
                  N_Assoc := Get_Chain (N_Assoc);
                  Res :=
                    Chap6.Translate_Name (Get_Actual (N_Assoc), Mode_Value);
                  Start_Association (Assocs, Ghdl_Text_Read_Length);
                  --    compute file parameter (get an index)
                  New_Association
                    (Assocs, Chap7.Translate_Expression (File_Param));
                  --    compute string parameter (get a fat array pointer)
                  New_Association (Assocs, Str);
                  --    call a predefined procedure
                  New_Assign_Stmt (M2Lv (Res), New_Function_Call (Assocs));
               else
                  Translate_Read_Procedure_Call (Imp, Assoc_Chain);
               end if;
            end;

         when Iir_Predefined_Read =>
            Translate_Read_Procedure_Call (Imp, Assoc_Chain);

         when Iir_Predefined_Deallocate =>
            Chap3.Translate_Object_Deallocation (Get_Actual (Assoc_Chain));

         when Iir_Predefined_File_Open =>
            declare
               File_Param : constant Iir := Get_Actual (Assoc_Chain);
               Name_Inter : constant Iir := Get_Chain (Inter_Chain);
               Name_Assoc : constant Iir := Get_Chain (Assoc_Chain);
               Name_Param : constant Iir := Get_Actual (Name_Assoc);
               Kind_Inter : constant Iir := Get_Chain (Name_Inter);
               Kind_Assoc : constant Iir := Get_Chain (Name_Assoc);
               Kind_Param : constant Iir :=
                 Get_Actual_Or_Default (Kind_Assoc, Kind_Inter);
               Constr     : O_Assoc_List;
            begin
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
               Status_Param : constant Iir := Get_Actual (Assoc_Chain);
               File_Inter : constant Iir := Get_Chain (Inter_Chain);
               File_Assoc : constant Iir := Get_Chain (Assoc_Chain);
               File_Param : constant Iir := Get_Actual (File_Assoc);
               Name_Inter : constant Iir := Get_Chain (File_Inter);
               Name_Assoc : constant Iir := Get_Chain (File_Assoc);
               Name_Param : constant Iir := Get_Actual (Name_Assoc);
               Kind_Inter : constant Iir := Get_Chain (Name_Inter);
               Kind_Assoc : constant Iir := Get_Chain (Name_Assoc);
               Kind_Param : constant Iir :=
                 Get_Actual_Or_Default (Kind_Assoc, Kind_Inter);
               Constr       : O_Assoc_List;
               Status       : Mnode;
            begin
               Status := Chap6.Translate_Name (Status_Param, Mode_Value);
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
               File_Param : constant Iir := Get_Actual (Assoc_Chain);
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
               File_Param : constant Iir := Get_Actual (Assoc_Chain);
               Constr     : O_Assoc_List;
            begin
               Start_Association (Constr, Ghdl_File_Flush);
               New_Association
                 (Constr, Chap7.Translate_Expression (File_Param));
               New_Procedure_Call (Constr);
            end;

         when others =>
            Simple_IO.Put_Line_Err
              ("translate_implicit_procedure_call: cannot handle "
               & Iir_Predefined_Functions'Image (Kind));
            raise Internal_Error;
      end case;
   end Translate_Implicit_Procedure_Call;

   function Get_Interface_Kind (Formal : Iir) return Object_Kind_Type is
   begin
      if Get_Kind (Formal) = Iir_Kind_Interface_Signal_Declaration then
         return Mode_Signal;
      else
         return Mode_Value;
      end if;
   end Get_Interface_Kind;

   procedure Translate_Procedure_Call_State (Call : Iir)
   is
      Imp : constant Iir := Get_Implementation (Call);
      Info : constant Call_Info_Acc := Get_Info (Call);

      Assoc, Inter : Iir;
      Num : Natural;
   begin
      Push_Instance_Factory (Info.Call_State_Scope'Access);

      --  Variable for the frame.
      Info.Call_Params_Var := Create_Var (Create_Var_Identifier ("PARAMS"),
                                         Get_Info (Imp).Subprg_Params_Type,
                                         O_Storage_Local);
      Info.Call_State_Mark := Create_Var (Create_Var_Identifier ("MARK"),
                                          Ghdl_Ptr_Type, O_Storage_Local);

      Assoc := Get_Parameter_Association_Chain (Call);
      Inter := Get_Interface_Declaration_Chain (Imp);
      Num := 0;
      while Assoc /= Null_Iir loop
         declare
            Formal : constant Iir := Get_Association_Formal (Assoc, Inter);
            Ftype : constant Iir := Get_Type (Formal);
            Ftype_Info : constant Type_Info_Acc := Get_Info (Ftype);
            Call_Assoc_Info : Call_Assoc_Info_Acc;
            Actual : Iir;
            Act_Type : Iir;
            Has_Bounds_Field : Boolean;
            Has_Fat_Pointer_Field : Boolean;
            Has_Value_Field : Boolean;
            Has_Ref_Field : Boolean;
            Object_Kind : Object_Kind_Type;
            Val_Type : O_Tnode;
            Vident : Var_Ident_Type;

            --  For unconstrained interfaces:
            --  * create a field for the fat pointer, unless
            --    - the expression is statically built
            function Need_Fat_Pointer_Field return Boolean is
            begin
               return not Is_Fully_Constrained_Type (Ftype)
                 and then (Actual = Null_Iir
                             or else not Is_Static_Construct (Actual));
            end Need_Fat_Pointer_Field;

            --  For unconstrained interfaces:
            --  * create a field for the bounds, unless
            --    - the expression is statically built
            --    - the expression/name type is locally static
            --    - expression is a call to an unconstrained function
            --    - expression is an object name that is not a slice
            function Need_Bounds_Field return Boolean
            is
               Kind : Iir_Kind;
            begin
               if Is_Fully_Constrained_Type (Ftype) then
                  return False;
               end if;
               if Act_Type /= Null_Iir
                 and then Get_Type_Staticness (Act_Type) = Locally
               then
                  return False;
               end if;
               if Actual /= Null_Iir then
                  if Get_Expr_Staticness (Actual) = Locally then
                     return False;
                  end if;
                  Kind := Get_Kind (Actual);
                  if (Kind = Iir_Kind_Function_Call
                        or else Kind in Iir_Kinds_Dyadic_Operator
                        or else Kind in Iir_Kinds_Monadic_Operator)
                    and then Is_Fully_Constrained_Type (Get_Type (Actual))
                  then
                     return False;
                  end if;
                  if Is_Object_Name (Actual)
                    and then Kind /= Iir_Kind_Slice_Name
                  then
                     return False;
                  end if;
               end if;
               return True;
            end Need_Bounds_Field;

            --  Helper for Need_Value_Field.  Any expression whose result is
            --  on stack2 doesn't need to be copied (again) on stack2.  This is
            --  an optimization and the result can be conservative.
            --  FIXME: also consider attributes (like 'image) and implicit
            --   functions (like to_string).
            function Is_Result_On_Stack2_Expression (Expr : Iir) return Boolean
            is
               Info : Ortho_Info_Acc;
               Imp : Iir;
            begin
               case Get_Kind (Expr) is
                  when Iir_Kind_Function_Call =>
                     Imp := Get_Implementation (Expr);
                     Info := Get_Info (Imp);
                     --  Note: Implicit functions don't have info.  A few of
                     --  them (like to_string) return the result on stack2.
                     return Info /= null
                       and then Info.Use_Stack2;
                  when Iir_Kinds_Monadic_Operator
                    | Iir_Kinds_Dyadic_Operator =>
                     return False;
                  when others =>
                     return False;
               end case;
            end Is_Result_On_Stack2_Expression;

            --  If the associated expression is not a name of an object (never
            --  the case for a signal interface and variable interface):
            --  * create a field for the value, unless
            --    - expression is statically built
            --    - expression is scalar
            --    - expression is a call to an unconstrained function
            --  If the actual is a name of an object, create a field for the
            --  value only if the object is a signal and the interface is
            --  a constant (we need to capture the value of the signal).
            function Need_Value_Field return Boolean
            is
               pragma Assert (Actual /= Null_Iir);
               Act_Obj : constant Iir := Name_To_Object (Actual);
            begin
               if Act_Obj /= Null_Iir then
                  --  Actual is an object.
                  if (Get_Kind (Formal)
                        = Iir_Kind_Interface_Constant_Declaration)
                    and then Is_Signal_Object (Act_Obj)
                  then
                     --  The value of the signal needs to be captured.
                     return True;
                  end if;
                  return False;
               end if;

               if Is_Static_Construct (Actual)
                 or else (Get_Kind (Act_Type)
                            in Iir_Kinds_Scalar_Type_And_Subtype_Definition)
                 or else Get_Kind (Ftype) = Iir_Kind_File_Type_Definition
                 or else Is_Result_On_Stack2_Expression (Actual)
               then
                  return False;
               end if;
               return True;
            end Need_Value_Field;
         begin
            Inter := Get_Association_Interface (Assoc, Inter);

            Call_Assoc_Info := null;
            Has_Bounds_Field := False;
            Has_Fat_Pointer_Field := False;
            Has_Value_Field := False;
            Has_Ref_Field := False;

            case Iir_Kinds_Association_Element_Parameters (Get_Kind (Assoc)) is
               when Iir_Kind_Association_Element_By_Individual =>
                  --  Create a field for the whole formal.
                  Has_Value_Field := True;
                  Actual := Null_Iir;
                  Act_Type := Get_Actual_Type (Assoc);
               when Iir_Kind_Association_Element_By_Expression
                 | Iir_Kind_Association_Element_By_Name =>
                  Actual := Get_Actual (Assoc);
                  Act_Type := Get_Type (Actual);
               when Iir_Kind_Association_Element_Open =>
                  Actual := Get_Default_Value (Inter);
                  Act_Type := Get_Type (Actual);
            end case;

            --  If the actual is a slice, create the type early so that they
            --  could be used in different states.  If they are created too
            --  late, they could be created in a state but referenced in
            --  a different one.
            if Actual /= Null_Iir
              and then Get_Kind (Actual) = Iir_Kind_Slice_Name
            then
               Chap3.Create_Composite_Subtype (Act_Type, False);
            end if;

            --  For out or inout scalar variable, create a field for the
            --  actual value.
            if Actual /= Null_Iir
              and then (Get_Kind (Inter)
                          = Iir_Kind_Interface_Variable_Declaration)
              and then Get_Mode (Inter) /= Iir_In_Mode
              and then
              (Formal /= Inter
                 or else Ftype_Info.Type_Mode in Type_Mode_Call_By_Value)
            then
               Has_Ref_Field := True;
            end if;

            if Formal = Inter
              and then Ftype_Info.Type_Mode not in Type_Mode_Thin
            then
               --  For whole association: create field according to the above
               --  predicates.
               --  For thin modes, there is no bounds, no fat pointers and the
               --  value is directly passed in the parameters.
               Has_Bounds_Field := Need_Bounds_Field;
               Has_Fat_Pointer_Field := Need_Fat_Pointer_Field;
               Has_Value_Field := Has_Value_Field or else Need_Value_Field;
            end if;

            if Has_Bounds_Field
              or Has_Fat_Pointer_Field
              or Has_Value_Field
              or Has_Ref_Field
            then
               --  Create the info and the variables.
               Call_Assoc_Info := Add_Info (Assoc, Kind_Call_Assoc);
               Object_Kind := Get_Interface_Kind (Inter);
               if Has_Ref_Field then
                  --  Reference to the actual.  Therefore the type of the
                  --  actual must be used (due to a possible conversion or
                  --  function call).
                  pragma Assert (Object_Kind = Mode_Value);
                  declare
                     Atype_Info : constant Type_Info_Acc :=
                       Get_Info (Act_Type);
                     Atype_Binfo : Type_Info_Acc;
                     Ref_Type : O_Tnode;
                  begin
                     if Atype_Info /= null then
                        Ref_Type := Atype_Info.Ortho_Ptr_Type (Object_Kind);
                     else
                        --  Type of actual was not yet translated.  Possible
                        --  only for slice.  Do it manually.
                        Atype_Binfo := Get_Info (Get_Base_Type (Act_Type));
                        Ref_Type := Atype_Binfo.B.Base_Ptr_Type (Object_Kind);
                     end if;
                     Call_Assoc_Info.Call_Assoc_Ref := Create_Var
                       (Create_Var_Identifier (Inter, "__REF", Num),
                        Ref_Type, O_Storage_Local);
                  end;
               end if;

               if Has_Value_Field then
                  for Mode in Mode_Value .. Object_Kind loop
                     if Ftype_Info.Type_Mode in Type_Mode_Unbounded then
                        --  For unconstrained arrays/records:
                        --   - the array (if the actual is constrained and not
                        --                complex) - TODO
                        --   - a pointer to the base.
                        Val_Type := Ftype_Info.B.Base_Ptr_Type (Mode);
                     else
                        --  For constrained arrays/records:
                        --   - the base if not complex
                        --   - a pointer to the base, if complex
                        if Is_Complex_Type (Ftype_Info) then
                           Val_Type := Ftype_Info.Ortho_Ptr_Type (Mode);
                        else
                           Val_Type := Ftype_Info.Ortho_Type (Mode);
                        end if;
                     end if;
                     case Mode is
                        when Mode_Value =>
                           Vident :=
                             Create_Var_Identifier (Inter, "__VAL", Num);
                        when Mode_Signal =>
                           Vident :=
                             Create_Var_Identifier (Inter, "__SIG", Num);
                     end case;
                     Call_Assoc_Info.Call_Assoc_Value (Mode) := Create_Var
                       (Vident, Val_Type, O_Storage_Local);
                  end loop;
               end if;

               if Has_Bounds_Field then
                  Call_Assoc_Info.Call_Assoc_Bounds := Create_Var
                    (Create_Var_Identifier (Inter, "__BND", Num),
                     Ftype_Info.B.Bounds_Type, O_Storage_Local);
               end if;

               if Has_Fat_Pointer_Field then
                  Call_Assoc_Info.Call_Assoc_Fat (Mode_Value) := Create_Var
                    (Create_Var_Identifier (Inter, "__FATV", Num),
                     Ftype_Info.Ortho_Type (Mode_Value));
                  if Object_Kind = Mode_Signal then
                     Call_Assoc_Info.Call_Assoc_Fat (Mode_Signal) := Create_Var
                       (Create_Var_Identifier (Inter, "__FATS", Num),
                        Ftype_Info.Ortho_Type (Mode_Signal));
                  end if;
               end if;
               Num := Num + 1;

            elsif Formal /= Inter
              and then
              Get_Kind (Inter) = Iir_Kind_Interface_Signal_Declaration
            then
               --  The whole signal value is composed of parts and must be
               --  updated when it changes (at each cycle is a worst case
               --  approximation).  Keep pointer to the individual value.
               Call_Assoc_Info := Add_Info (Assoc, Kind_Call_Assoc);
               Call_Assoc_Info.Call_Assoc_Value (Mode_Value) := Create_Var
                 (Create_Var_Identifier (Inter, "__VALP", Num),
                  Ftype_Info.Ortho_Ptr_Type (Mode_Value));
               Num := Num + 1;
            end if;
         end;
         Next_Association_Interface (Assoc, Inter);
      end loop;

      Pop_Instance_Factory (Info.Call_State_Scope'Access);
      New_Type_Decl (Create_Identifier ("CALLERTYPE"),
                     Get_Scope_Type (Info.Call_State_Scope));
   end Translate_Procedure_Call_State;

   function Do_Conversion (Conv : Iir; Expr : Iir; Src : O_Enode)
                          return O_Enode is
   begin
      if Conv = Null_Iir then
         return Src;
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
                  Start_Association (Constr, Conv_Info.Subprg_Node);

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

                  New_Association (Constr, Src);

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
                 (Src, Get_Type (Expr), Get_Type (Conv), Conv);
            when others =>
               Error_Kind ("do_conversion", Conv);
         end case;
      end if;
   end Do_Conversion;

   --  Translate the formal name FORMAL_NAME of an individual association but
   --  replace the interface name by INTER_VAR.  FORMAL_INFO is the info of
   --  the interface.  This is used to access to a sub-element of the variable
   --  representing the whole actual.
   function Translate_Individual_Association_Formal
     (Formal_Name : Iir;
      Formal_Info : Ortho_Info_Acc;
      Inter_Var : Mnode;
      Mode : Object_Kind_Type)
     return Mnode
   is
      Prev_Decl : O_Dnode;
      Prev_Field : O_Fnode;
      Res : Mnode;
   begin
      --  Change the formal variable so that it is the local variable
      --  that will be passed to the subprogram.
      Prev_Decl := Formal_Info.Interface_Decl (Mode);
      Prev_Field := Formal_Info.Interface_Field (Mode);

      --  We need a pointer since the interface is by reference.
      Formal_Info.Interface_Decl (Mode) := M2Dp (Inter_Var);
      Formal_Info.Interface_Field (Mode) := O_Fnode_Null;

      Res := Chap6.Translate_Name (Formal_Name, Mode);

      Formal_Info.Interface_Decl (Mode) := Prev_Decl;
      Formal_Info.Interface_Field (Mode) := Prev_Field;

      return Res;
   end Translate_Individual_Association_Formal;

   function Translate_Subprogram_Call
     (Call : Iir; Assoc_Chain : Iir; Obj : Iir) return O_Enode
   is
      Imp : constant Iir := Get_Implementation (Call);
      Inter_Chain : constant Iir := Get_Interface_Declaration_Chain (Imp);

      Is_Procedure : constant Boolean :=
        Get_Kind (Imp) = Iir_Kind_Procedure_Declaration;
      Is_Function : constant Boolean := not Is_Procedure;
      Is_Foreign : constant Boolean := Get_Foreign_Flag (Imp);
      Info : constant Subprg_Info_Acc := Get_Info (Imp);

      --  True if the callee is suspendable.
      Does_Callee_Suspend : constant Boolean := Is_Procedure
        and then Get_Suspend_Flag (Imp);

      Call_Info : constant Ortho_Info_Acc := Get_Info (Call);

      --  True if the caller is suspendable.  The callee can still be
      --  suspendable, but cannot suspend.
      Is_Suspendable : constant Boolean := Call_Info /= null;

      --  Where to allocate to store parameters (return stack for suspendable
      --  procedure, stack otherwise).
      Alloc : Allocation_Kind;

      type Mnode_Array is array (Natural range <>) of Mnode;
      type O_Enode_Array is array (Natural range <>) of O_Enode;
      Nbr_Assoc : constant Natural :=
        Vhdl.Nodes_Utils.Get_Chain_Length (Assoc_Chain);

      --  References to the formals (for copy-out), and variables for whole
      --  actual of individual associations.
      Params : Mnode_Array (0 .. Nbr_Assoc - 1);

      --  The values of actuals.
      E_Params : O_Enode_Array (0 .. Nbr_Assoc - 1);
      E_Sig_Params : O_Enode_Array (0 .. Nbr_Assoc - 1);

      --  Only for inout/out variables passed by copy of foreign procedures:
      --  the copy of the scalar.
      Inout_Params : Mnode_Array (0 .. Nbr_Assoc - 1);

      --  Variable containing the frame (state, parameters, local variables).
      --  Exists only for procedures.
      Params_Var : Var_Type;

      --  Index of the last individual association (needed because it holds
      --  the actual).
      Last_Individual : Natural;

      Dynamic_Individual_Assoc : Iir;
      Saved_Val : Mnode_Array (0 .. Nbr_Assoc - 1);
      Saved_Sig : Mnode_Array (0 .. Nbr_Assoc - 1);

      --  Individual association: assign the individual actual of
      --  the whole actual.
      procedure Trans_Individual_Assign (Assoc : Iir; Val : Mnode; Sig : Mnode)
      is
         Formal : constant Iir := Get_Formal (Assoc);
         Formal_Type : constant Iir := Get_Type (Formal);
         Base_Formal : constant Iir := Get_Interface_Of_Formal (Formal);
         Formal_Info : constant Interface_Info_Acc := Get_Info (Base_Formal);
         Formal_Object_Kind : constant Object_Kind_Type :=
           Get_Interface_Kind (Base_Formal);
         Act : constant Iir := Get_Actual (Assoc);
         Assoc_Info : Call_Assoc_Info_Acc;
         Param : Mnode;
      begin
         Param := Translate_Individual_Association_Formal
           (Formal, Formal_Info, Params (Last_Individual),
            Formal_Object_Kind);
         if Formal_Object_Kind = Mode_Value then
            Chap7.Translate_Assign (Param, M2E (Val), Act, Formal_Type, Assoc);
         else
            Chap3.Translate_Object_Copy (Param, Sig, Formal_Type);
            if Is_Suspendable then
               --  Keep reference to the value to update the whole object
               --  at each call.
               Assoc_Info := Get_Info (Assoc);
               New_Assign_Stmt
                 (Get_Var (Assoc_Info.Call_Assoc_Value (Mode_Value)),
                  M2E (Val));
            else
               --  Assign the value to the whole object, as there is
               --  only one call.
               Param := Translate_Individual_Association_Formal
                 (Formal, Formal_Info, Params (Last_Individual),
                  Mode_Value);
               Chap3.Translate_Object_Copy (Param, Val, Formal_Type);
            end if;
         end if;
      end Trans_Individual_Assign;

      --  Evaluate the actual of ASSOC/INTER (whose index is POS), do the
      --  actual conversion and save the result (either copy it to a variable
      --  or field, or just keep the value to pass it while calling the
      --  subprogram).
      procedure Trans_Actual (Assoc : Iir; Inter : Iir; Pos : Natural)
      is
         Formal : constant Iir := Get_Association_Formal (Assoc, Inter);
         Formal_Type : constant Iir := Get_Type (Formal);
         Ftype_Info : constant Type_Info_Acc := Get_Info (Formal_Type);
         Base_Formal : constant Iir := Get_Interface_Of_Formal (Formal);
         Formal_Info : constant Interface_Info_Acc := Get_Info (Base_Formal);
         Formal_Object_Kind : constant Object_Kind_Type :=
           Get_Interface_Kind (Base_Formal);
         Assoc_Info : Call_Assoc_Info_Acc;
         Act : Iir;
         Actual_Type : Iir;
         In_Conv : Iir;
         Param : Mnode;
         Param_Sig : Mnode;
         Param_Type : Iir;
         Val : O_Enode;
         Sig : O_Enode;
         Mval : Mnode;
         Msig : Mnode;
         Mode : Iir_Mode;
         Bounds : Mnode;
         Next_Assoc : Iir;

         --  Assign PARAMS field for formal to V.
         procedure Assign_Params_Field (V : O_Enode; Mode : Object_Kind_Type)
         is
            Ptr : O_Lnode;
         begin
            Ptr := New_Selected_Element
              (Get_Var (Params_Var), Formal_Info.Interface_Field (Mode));
               New_Assign_Stmt (Ptr, V);
         end Assign_Params_Field;
      begin
         --  To translate user redefined operators,
         --  translate_operator_function_call creates associations, that
         --  have not corresponding infos.  Do not try to get assoc info
         --  for non-suspendable procedures.
         --  FIXME: either transform operator to a function call in canon,
         --    or directly translate function call.
         if Does_Callee_Suspend then
            Assoc_Info := Get_Info (Assoc);
         else
            Assoc_Info := null;
         end if;

         case Get_Kind (Assoc) is
            when Iir_Kind_Association_Element_Open =>
               Act := Get_Default_Value (Base_Formal);
               In_Conv := Null_Iir;
            when Iir_Kind_Association_Element_By_Expression =>
               Act := Get_Actual (Assoc);
               In_Conv := Get_Actual_Conversion (Assoc);
            when Iir_Kind_Association_Element_By_Individual =>
               Actual_Type := Get_Actual_Type (Assoc);

               --  Save the object as it will be used by the following
               --  associations.
               Last_Individual := Pos;

               for Mode in Mode_Value .. Formal_Object_Kind loop
                  --  For individual associations, create a variable
                  --  containing the whole actual.  Each individual
                  --  association (to the same formal) will set a part of
                  --  this variable.
                  if Assoc_Info = null then
                     Param := Create_Temp (Ftype_Info, Mode);
                  else
                     declare
                        Param_Var : Var_Type;
                     begin
                        if Ftype_Info.Type_Mode in Type_Mode_Unbounded then
                           Param_Var := Assoc_Info.Call_Assoc_Fat (Mode);
                        else
                           Param_Var := Assoc_Info.Call_Assoc_Value (Mode);
                        end if;
                        Param := Stabilize
                          (Get_Var (Param_Var, Ftype_Info, Mode));
                     end;
                  end if;

                  if Ftype_Info.Type_Mode in Type_Mode_Unbounded then
                     --  Create the constraints and then the object.
                     --  FIXME: do not allocate bounds if static.
                     if Mode = Mode_Value then
                        if Get_Type_Staticness (Actual_Type) >= Globally then
                           Chap3.Create_Composite_Subtype (Actual_Type);
                           Bounds :=
                             Chap3.Get_Composite_Type_Bounds (Actual_Type);
                           Chap3.Translate_Object_Allocation
                             (Param, Alloc, Formal_Type, Bounds);
                        else
                           --  The bounds of the formal are not known (will be
                           --  determined by the actuals).  Just allocate the
                           --  bounds.
                           Chap3.Allocate_Unbounded_Composite_Bounds
                             (Alloc, Param, Formal_Type);

                           Saved_Val (Pos) := Param;

                           pragma Assert (Dynamic_Individual_Assoc = Null_Iir);
                           Dynamic_Individual_Assoc := Assoc;
                        end if;
                     else
                        --  Use the bounds of the value for the signal.
                        New_Assign_Stmt
                          (M2Lp (Chap3.Get_Composite_Bounds (Param)),
                           M2Addr (Chap3.Get_Composite_Bounds (Params (Pos))));

                        if Get_Type_Staticness (Actual_Type) >= Globally then
                           --  Allocate the base (only if the bounds are
                           --  known).
                           Chap3.Allocate_Unbounded_Composite_Base
                             (Alloc, Param, Formal_Type);
                        end if;

                        Saved_Sig (Pos) := Param;
                     end if;
                  else
                     --  Create the object.
                     Chap4.Allocate_Complex_Object (Formal_Type, Alloc, Param);
                  end if;

                  --  In case of signals, don't keep value, only keep
                  --  signal (so override the value).
                  Params (Pos) := Param;

                  if Formal_Info.Interface_Field (Mode) /= O_Fnode_Null then
                     --  Set the PARAMS field.
                     Assign_Params_Field (M2E (Param), Mode);
                  end if;
               end loop;

               goto Continue;
            when others =>
               Error_Kind ("translate_procedure_call", Assoc);
         end case;
         Actual_Type := Get_Type (Act);

         --  For individual associations, be sure the type is translated.
         --  That's required for slices in case of array conversion.
         if Formal /= Base_Formal then
            Chap3.Translate_Anonymous_Subtype_Definition (Formal_Type, False);
         end if;

         --  Evaluate the actual.
         Param_Type := Actual_Type;
         case Get_Kind (Base_Formal) is
            when Iir_Kind_Interface_Constant_Declaration
              | Iir_Kind_Interface_File_Declaration =>
               --  No conversion here.
               pragma Assert (In_Conv = Null_Iir);
               Val := Chap7.Translate_Expression (Act, Formal_Type);
               Sig := O_Enode_Null;
               Param_Type := Formal_Type;
            when Iir_Kind_Interface_Signal_Declaration =>
               --  No conversion.
               Chap6.Translate_Signal_Name (Act, Param_Sig, Param);
               case Formal_Info.Interface_Mechanism (Mode_Value) is
                  when Pass_By_Copy =>
                     Val := M2E (Param);
                  when Pass_By_Address =>
                     Val := M2Addr (Param);
               end case;
               Sig := M2E (Param_Sig);
            when Iir_Kind_Interface_Variable_Declaration =>
               Mode := Get_Mode (Base_Formal);
               Sig := O_Enode_Null;
               if Mode = Iir_In_Mode then
                  Val := Chap7.Translate_Expression (Act);
               else
                  Param := Chap6.Translate_Name (Act, Mode_Value);
                  if Base_Formal /= Formal
                    or else Ftype_Info.Type_Mode in Type_Mode_Call_By_Value
                  then
                     --  For out/inout, we need to keep the reference
                     --  for the copy-out.
                     Stabilize (Param);
                     Params (Pos) := Param;

                     if Assoc_Info /= null then
                        --  Save reference in local frame.
                        New_Assign_Stmt (Get_Var (Assoc_Info.Call_Assoc_Ref),
                                         M2Addr (Param));
                     end if;
                  end if;
                  if In_Conv = Null_Iir
                    and then Mode = Iir_Out_Mode
                    and then Ftype_Info.Type_Mode in Type_Mode_Thin
                    and then Ftype_Info.Type_Mode /= Type_Mode_File
                  then
                     --  Scalar OUT interface.  Just give an initial value.
                     --  FIXME: individual association ??
                     Val := Chap4.Get_Scalar_Initial_Value (Formal_Type);
                     Param_Type := Formal_Type;
                  else
                     Val := M2E (Param);
                  end if;
                  if Is_Foreign
                    and then Ftype_Info.Type_Mode in Type_Mode_Pass_By_Copy
                  then
                     --  Scalar parameters of foreign procedures (of mode
                     --  out or inout) are passed by address, create a copy
                     --  of the value.
                     Inout_Params (Pos) :=
                       Create_Temp (Ftype_Info, Mode_Value);
                  end if;
               end if;
               if In_Conv /= Null_Iir then
                  Val := Do_Conversion (In_Conv, Act, Val);
                  Act := In_Conv;
                  Param_Type := Get_Type (In_Conv);
               end if;
            when others =>
               Error_Kind ("translate_procedure_call(2)", Formal);
         end case;

         --  Implicit conversion to formal type.
         if Param_Type /= Formal_Type then
            --  Implicit array conversion or subtype check.
            Val := Chap7.Translate_Implicit_Conv
              (Val, Param_Type, Formal_Type, Mode_Value, Act);
            if Sig /= O_Enode_Null then
               --  FIXME: convert without checking.
               Sig := Chap7.Translate_Implicit_Conv
                 (Sig, Param_Type, Formal_Type, Mode_Signal, Act);
            end if;
         end if;
         if Get_Kind (Base_Formal) /= Iir_Kind_Interface_Signal_Declaration
         then
            Val := Chap3.Maybe_Insert_Scalar_Check (Val, Act, Formal_Type);
         end if;

         --  Assign actual, if needed.
         if Base_Formal /= Formal then
            --  Individual association.
            if Dynamic_Individual_Assoc /= Null_Iir then
               --  With dynamic bounds.
               --  FIXME: only records are supported.
               pragma Assert (Get_Kind (Formal) = Iir_Kind_Selected_Element);

               --  Save the actual.
               Saved_Val (Pos) := E2M (Val, Ftype_Info, Mode_Value);
               if Formal_Object_Kind = Mode_Signal then
                  Saved_Sig (Pos) := E2M (Sig, Ftype_Info, Mode_Signal);
               end if;

               --  If the record element is dynamic, copy the bounds.
               if Is_Unbounded_Type (Ftype_Info) then
                  Stabilize (Saved_Val (Pos));

                  Chap3.Copy_Bounds
                    (Chap3.Record_Bounds_To_Element_Bounds
                       (Chap3.Get_Composite_Bounds
                          (Params (Last_Individual)),
                        Get_Named_Entity (Formal)),
                     Chap3.Get_Composite_Bounds (Saved_Val (Pos)),
                     Formal_Type);
               end if;

               --  If this is the last association for the interface:
               Next_Assoc := Get_Chain (Assoc);
               if Next_Assoc = Null_Iir
                 or else Get_Formal (Next_Assoc) = Null_Iir
                 or else (Get_Interface_Of_Formal (Get_Formal (Next_Assoc))
                            /= Base_Formal)
               then
                  --  * compute the size of the object
                  Chap3.Gen_Call_Type_Builder
                    (Chap3.Get_Composite_Bounds (Params (Last_Individual)),
                     Get_Type (Base_Formal), Mode_Value);
                  if Formal_Object_Kind = Mode_Signal then
                     Chap3.Gen_Call_Type_Builder
                       (Chap3.Get_Composite_Bounds (Params (Last_Individual)),
                        Get_Type (Base_Formal), Mode_Signal);
                  end if;

                  --  * allocate base
                  Chap3.Allocate_Unbounded_Composite_Base
                    (Alloc, Saved_Val (Last_Individual),
                     Get_Type (Base_Formal));
                  if Formal_Object_Kind = Mode_Signal then
                     Chap3.Allocate_Unbounded_Composite_Base
                       (Alloc, Saved_Sig (Last_Individual),
                        Get_Type (Base_Formal));
                  end if;

                  --  * copy all elements
                  Next_Assoc := Dynamic_Individual_Assoc;
                  for I in Last_Individual + 1 .. Pos loop
                     Next_Assoc := Get_Chain (Next_Assoc);
                     if Formal_Object_Kind = Mode_Signal then
                        Trans_Individual_Assign
                          (Next_Assoc, Saved_Val (I), Saved_Sig (I));
                     else
                        Trans_Individual_Assign
                          (Next_Assoc, Saved_Val (I), Mnode_Null);
                     end if;
                  end loop;

                  --  * clear the flag.
                  Dynamic_Individual_Assoc := Null_Iir;
               end if;
            else
               --  Individual association: assign the individual actual of
               --  the whole actual.
               if Sig = O_Enode_Null then
                  --  Arghh..
                  Msig := Mnode_Null;
               else
                  Msig := E2M (Sig, Get_Info (Formal_Type), Mode_Signal);
               end if;
               --  Note: Ftype_Info may be null (if the formal is a slice).
               Trans_Individual_Assign
                 (Assoc, E2M (Val, Get_Info (Formal_Type), Mode_Value), Msig);
            end if;
         elsif Assoc_Info /= null then
            --  For suspendable caller, write the actual to the state
            --  record.  In some cases (like expressions), the value has
            --  to be copied (it may be the result of a computation).

            --  Only for whole association.
            pragma Assert (Base_Formal = Formal);

            for Mode in Mode_Value .. Formal_Object_Kind loop
               if Mode = Mode_Value then
                  Mval := Stabilize (E2M (Val, Ftype_Info, Mode_Value), True);
               else
                  Mval := Stabilize (E2M (Sig, Ftype_Info, Mode_Signal), True);
               end if;

               declare
                  Fat : Mnode;
                  Bnd : Mnode;
               begin
                  if Assoc_Info.Call_Assoc_Fat (Mode) /= Null_Var then
                     -- pragma Assert (Sig = O_Enode_Null); --  TODO
                     --  Fat pointer.  VAL is a pointer to a fat pointer, so
                     --  copy the fat pointer to the FAT field, and set the
                     --  PARAM field to FAT field.
                     Fat := Stabilize
                       (Get_Var (Assoc_Info.Call_Assoc_Fat (Mode),
                                 Ftype_Info, Mode));

                     --  Set PARAM field to the address of the FAT field.
                     pragma Assert (Formal_Info.Interface_Field (Mode)
                                      /= O_Fnode_Null);
                     Assign_Params_Field (M2E (Fat), Mode);

                     if Assoc_Info.Call_Assoc_Bounds /= Null_Var then
                        --  Copy the bounds.
                        Bnd := Stabilize
                          (Lv2M (Get_Var (Assoc_Info.Call_Assoc_Bounds),
                                 Ftype_Info, Mode_Value,
                                 Ftype_Info.B.Bounds_Type,
                                 Ftype_Info.B.Bounds_Ptr_Type));
                        Chap3.Copy_Bounds
                          (Bnd,
                           Chap3.Get_Composite_Bounds (Mval), Formal_Type);
                        New_Assign_Stmt
                          (M2Lp (Chap3.Get_Composite_Bounds (Fat)),
                           M2Addr (Bnd));
                        New_Assign_Stmt
                          (M2Lp (Chap3.Get_Composite_Base (Fat)),
                           M2Addr (Chap3.Get_Composite_Base (Mval)));
                     else
                        --  No need to copy the bounds.
                        Copy_Fat_Pointer (Fat, Mval);
                     end if;
                  end if;

                  if Mode = Mode_Value
                    and then
                    Assoc_Info.Call_Assoc_Value (Mode_Value) /= Null_Var
                  then
                     pragma Assert (Sig = O_Enode_Null); --  TODO

                     if Ftype_Info.Type_Mode in Type_Mode_Unbounded then
                        pragma Assert
                          (Assoc_Info.Call_Assoc_Fat (Mode) /= Null_Var);
                        --  Allocate array base
                        Param := Fat;
                        Chap3.Allocate_Unbounded_Composite_Base
                          (Alloc_Return, Fat, Formal_Type);
                        --  NOTE: Call_Assoc_Value is not used, the base is
                        --  directly allocated in the fat pointer.
                     else
                        Param := Get_Var
                          (Assoc_Info.Call_Assoc_Value (Mode_Value),
                           Ftype_Info, Mode_Value);
                        Stabilize (Param);
                        Chap4.Allocate_Complex_Object
                          (Formal_Type, Alloc_Return, Param);
                        Assign_Params_Field (M2Addr (Param), Mode);
                     end if;
                     Chap3.Translate_Object_Copy (Param, Mval, Formal_Type);
                  end if;
               end;
            end loop;

            if Assoc_Info.Call_Assoc_Value (Mode_Value) = Null_Var
              and then Assoc_Info.Call_Assoc_Fat (Mode_Value) = Null_Var
            then
               pragma Assert (Sig = O_Enode_Null); --  Not possible.
                                                   --  Set the PARAMS field.
               Assign_Params_Field (M2E (Mval), Mode_Value);
            end if;
         elsif Formal_Info.Interface_Field (Mode_Value) /= O_Fnode_Null then
            Assign_Params_Field (Val, Mode_Value);

            if Sig /= O_Enode_Null then
               Assign_Params_Field (Sig, Mode_Signal);
            end if;
         elsif Inout_Params (Pos) /= Mnode_Null then
            --  Not for signals.
            pragma Assert (Sig = O_Enode_Null);

            Chap3.Translate_Object_Copy
              (Inout_Params (Pos),
               E2M (Val, Get_Info (Formal_Type), Mode_Value),
               Formal_Type);
            E_Params (Pos) := M2Addr (Inout_Params (Pos));
         else
            E_Params (Pos) := Val;
            E_Sig_Params (Pos) := Sig;
         end if;

         << Continue >> null;
      end Trans_Actual;

      Res : Mnode;
      El : Iir;
      Inter : Iir;
      Pos : Natural;
      Constr : O_Assoc_List;
      Mark_Var : Var_Type;

      Call_State : State_Type;
      Next_State : State_Type;
      If_Blk : O_If_Block;
   begin
      --  For functions returning an unconstrained object: save the mark.
      if Is_Function and then Info.Use_Stack2 then
         Create_Temp_Stack2_Mark;
      end if;

      if Is_Function and then Info.Res_Interface /= O_Dnode_Null then
         --  Composite result.
         --  If we need to allocate, do it before starting the call!
         --  TODO: could be eliminated if the value is also returned (RVO).
         declare
            Res_Type : constant Iir := Get_Return_Type (Imp);
            Res_Info : constant Type_Info_Acc := Get_Info (Res_Type);
         begin
            Res := Create_Temp (Res_Info);
            if Res_Info.Type_Mode not in Type_Mode_Unbounded then
               Chap4.Allocate_Complex_Object (Res_Type, Alloc_Stack, Res);
            end if;
         end;
      end if;

      if Is_Function or else Info.Subprg_Params_Type = O_Tnode_Null then
         --  Standard call, like a C function (no parameters struct).
         pragma Assert (not Does_Callee_Suspend);
         Params_Var := Null_Var;
         Mark_Var := Null_Var;
      else
         --  Create the variable containing the parameters.
         --  Save Stack2 mark.  Callee allocate its frame on stack2.
         if Is_Suspendable then
            --  The caller is suspendable.
            Params_Var := Call_Info.Call_Params_Var;
            Mark_Var := Call_Info.Call_State_Mark;
            --  There might be temporary variables created before the
            --  suspension, eg for range checks.
            --  Create a scope that will be closed just before the suspension.
            Open_Temp;
            Disable_Stack2_Release;
         else
            --  Caller does not suspend; create the frame variable.
            Start_Declare_Stmt;
            Open_Local_Temp;
            Mark_Var := Create_Var (Create_Var_Identifier ("CMARK"),
                                    Ghdl_Ptr_Type, O_Storage_Local);
            Params_Var := Create_Var (Create_Var_Identifier ("CPARAMS"),
                                      Info.Subprg_Params_Type,
                                      O_Storage_Local);
         end if;
         Set_Stack2_Mark (Get_Var (Mark_Var));
      end if;

      --  Set Alloc.
      if Does_Callee_Suspend then
         Alloc := Alloc_Return;
      else
         Alloc := Alloc_Stack;
      end if;

      --  Evaluate in-out parameters and parameters passed by ref, since
      --  they can add declarations.
      --  Non-composite in-out parameters address are saved in order to
      --  be able to assignate the result.
      Dynamic_Individual_Assoc := Null_Iir;
      El := Assoc_Chain;
      Inter := Inter_Chain;
      Pos := 0;
      while El /= Null_Iir loop
         Params (Pos) := Mnode_Null;
         E_Params (Pos) := O_Enode_Null;
         E_Sig_Params (Pos) := O_Enode_Null;
         Inout_Params (Pos) := Mnode_Null;

         Trans_Actual (El, Inter, Pos);

         Next_Association_Interface (El, Inter);
         Pos := Pos + 1;
      end loop;

      --  Second stage:  really perform the call.
      if Does_Callee_Suspend then
         --  Set initial state.
         New_Assign_Stmt
           (New_Selected_Element (Get_Var (Params_Var),
                                  Info.Subprg_State_Field),
            New_Lit (Ghdl_Index_0));
      end if;
      if Is_Suspendable then
         --  Close the scope created at the beginning.
         Close_Temp;

         Call_State := State_Allocate;
         Next_State := State_Allocate;

         --  Call state.
         State_Jump (Call_State);
         State_Start (Call_State);

         --  Update signals value in case of individual association.
         declare
            Base_Formal : Iir;
            Formal : Iir;
            Formal_Info : Interface_Info_Acc;
            Formal_Type : Iir;
            Assoc_Info : Call_Assoc_Info_Acc;
            Base_Param : Mnode;
            Param : Mnode;
            Val : Mnode;
         begin
            Open_Temp;
            El := Assoc_Chain;
            Inter := Inter_Chain;
            while El /= Null_Iir loop
               Base_Formal := Get_Association_Interface (El, Inter);
               case Get_Kind (El) is
                  when Iir_Kind_Association_Element_By_Individual =>
                     if Get_Kind (Base_Formal)
                       = Iir_Kind_Interface_Signal_Declaration
                     then
                        --  Get the whole value.
                        Formal_Info := Get_Info (Base_Formal);
                        Base_Param := Lp2M
                          (New_Selected_Element
                             (Get_Var (Params_Var),
                              Formal_Info.Interface_Field (Mode_Value)),
                           Get_Info (Get_Type (Base_Formal)), Mode_Value);
                        Stabilize (Base_Param);
                     end if;
                  when Iir_Kind_Association_Element_By_Expression =>
                     if not Get_Whole_Association_Flag (El)
                       and then (Get_Kind (Base_Formal)
                                   = Iir_Kind_Interface_Signal_Declaration)
                     then
                        Formal := Strip_Denoting_Name (Get_Formal (El));
                        Formal_Info := Get_Info (Base_Formal);
                        Formal_Type := Get_Type (Formal);
                        Assoc_Info := Get_Info (El);
                        --  Reference the individual sub-elements of the
                        --  whole value.
                        Param := Translate_Individual_Association_Formal
                          (Formal, Formal_Info, Base_Param, Mode_Value);
                        Val := Get_Varp
                          (Assoc_Info.Call_Assoc_Value (Mode_Value),
                           Get_Info (Formal_Type), Mode_Value);
                        --  Update.
                        Chap7.Translate_Assign
                          (Param, M2E (Val), Get_Actual (El),
                           Formal_Type, El);
                     end if;
                  when others =>
                     null;
               end case;
               Next_Association_Interface (El, Inter);
            end loop;
            Close_Temp;
         end;
      end if;

      Start_Association (Constr, Info.Subprg_Node);

      if Is_Function and then Info.Res_Interface /= O_Dnode_Null then
         --  Composite result.
         New_Association (Constr, M2E (Res));
      end if;

      if Params_Var /= Null_Var then
         --  Parameters record (for procedures).
         New_Association
           (Constr, New_Address (Get_Var (Params_Var),
                                 Info.Subprg_Params_Ptr));
      end if;

      if Obj /= Null_Iir then
         --  Protected object.
         New_Association
           (Constr, M2E (Chap6.Translate_Name (Obj, Mode_Value)));
      else
         --  Instance.
         Subprgs.Add_Subprg_Instance_Assoc (Constr, Info.Subprg_Instance);
      end if;

      --  Parameters.
      El := Assoc_Chain;
      Inter := Inter_Chain;
      Pos := 0;
      while El /= Null_Iir loop
         declare
            Formal : constant Iir := Get_Association_Formal (El, Inter);
            Base_Formal : constant Iir :=
              Get_Association_Interface (El, Inter);
            Formal_Info : constant Ortho_Info_Acc := Get_Info (Base_Formal);
         begin
            if Formal_Info.Interface_Field (Mode_Value) = O_Fnode_Null then
               --  Not a PARAMS field.
               if Get_Kind (El) = Iir_Kind_Association_Element_By_Individual
               then
                  --  Pass the whole data for an individual association.
                  New_Association (Constr, M2E (Params (Pos)));
               elsif Base_Formal = Formal then
                  --  Whole association.
                  New_Association (Constr, E_Params (Pos));
                  if E_Sig_Params (Pos) /= O_Enode_Null then
                     New_Association (Constr, E_Sig_Params (Pos));
                  end if;
               end if;
            end if;

            if Get_Kind (El) = Iir_Kind_Association_Element_Open then
               --  Do not share nodes for default values: clean them.
               Chap9.Destroy_Types (Get_Default_Value (Base_Formal));
            end if;
         end;

         Next_Association_Interface (El, Inter);
         Pos := Pos + 1;
      end loop;

      --  Subprogram call.
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

      if Is_Suspendable then
         Start_If_Stmt
           (If_Blk,
            New_Compare_Op (ON_Neq,
                            New_Value (New_Selected_Element
                                         (Get_Var (Params_Var),
                                          Info.Subprg_State_Field)),
                            New_Lit (Ghdl_Index_1),
                            Ghdl_Bool_Type));
         State_Suspend (Call_State);
         New_Else_Stmt (If_Blk);
         --  Return state.
         Open_Temp;
      end if;

      --  Copy-out non-composite parameters.
      El := Assoc_Chain;
      Inter := Inter_Chain;
      Pos := 0;
      while El /= Null_Iir loop
         if Get_Kind (El) = Iir_Kind_Association_Element_By_Individual then
            Last_Individual := Pos;
            declare
               Assoc_Info : constant Call_Assoc_Info_Acc := Get_Info (El);
               Base_Formal : constant Iir :=
                 Get_Association_Interface (El, Inter);
               Formal_Type : Iir;
               Ftype_Info : Type_Info_Acc;
            begin
               if Assoc_Info /= null
                 and then (Get_Kind (Base_Formal)
                             = Iir_Kind_Interface_Variable_Declaration)
               then
                  Formal_Type := Get_Type (Get_Named_Entity (Get_Formal (El)));
                  Ftype_Info := Get_Info (Formal_Type);
                  pragma Assert
                    (Get_Interface_Kind (Base_Formal) = Mode_Value);
                  declare
                     Param_Var : Var_Type;
                  begin
                     if Ftype_Info.Type_Mode = Type_Mode_Fat_Array then
                        Param_Var := Assoc_Info.Call_Assoc_Fat (Mode_Value);
                     else
                        Param_Var := Assoc_Info.Call_Assoc_Value (Mode_Value);
                     end if;
                     Params (Pos) := Stabilize
                       (Get_Var (Param_Var, Ftype_Info, Mode_Value));
                  end;
               end if;
            end;
         elsif Params (Pos) /= Mnode_Null then
            declare
               Assoc_Info : constant Call_Assoc_Info_Acc := Get_Info (El);
               Formal : constant Iir := Get_Association_Formal (El, Inter);
               Base_Formal : constant Iir := Get_Interface_Of_Formal (Formal);
               Formal_Type : constant Iir := Get_Type (Formal);
               Ftype_Info : constant Type_Info_Acc := Get_Info (Formal_Type);
               Formal_Info : constant Ortho_Info_Acc := Get_Info (Base_Formal);
               Act : Iir;
               Actual_Type : Iir;
               Param : Mnode;
               Val : O_Enode;
               Ptr : O_Lnode;
               Out_Conv : Iir;
               Out_Expr : Iir;
            begin
               pragma Assert (Get_Kind (Base_Formal)
                                = Iir_Kind_Interface_Variable_Declaration);
               pragma Assert (Get_Mode (Base_Formal) in Iir_Out_Modes);

               --  Extract the value
               if Base_Formal /= Formal then
                  --  By individual, copy back.
                  Param := Translate_Individual_Association_Formal
                    (Formal, Formal_Info, Params (Last_Individual),
                     Mode_Value);
               elsif Inout_Params (Pos) /= Mnode_Null then
                  Param := Inout_Params (Pos);
               else
                  pragma Assert
                    (Formal_Info.Interface_Field (Mode_Value) /= O_Fnode_Null);
                  Ptr := New_Selected_Element
                    (Get_Var (Params_Var),
                     Formal_Info.Interface_Field (Mode_Value));
                  case Type_Mode_Valid (Ftype_Info.Type_Mode) is
                     when Type_Mode_Pass_By_Copy =>
                        Param := Lv2M (Ptr, Ftype_Info, Mode_Value);
                     when Type_Mode_Pass_By_Address =>
                        Param := Lp2M (Ptr, Ftype_Info, Mode_Value);
                  end case;
               end if;

               Out_Conv := Get_Formal_Conversion (El);
               if Out_Conv = Null_Iir then
                  Out_Expr := Formal;
                  Val := M2E (Param);
               else
                  Out_Expr := Out_Conv;
                  Val := Do_Conversion (Out_Conv, Formal, M2E (Param));
               end if;

               Act := Get_Actual (El);
               Actual_Type := Get_Type (Act);
               if Assoc_Info = null then
                  Param := Params (Pos);
               else
                  Param := Lp2M (Get_Var (Assoc_Info.Call_Assoc_Ref),
                                 Get_Info (Actual_Type), Mode_Value);
               end if;
               --  FIXME: scalar check ?
               Chap7.Translate_Assign (Param, Val, Out_Expr, Actual_Type, El);
            end;
         end if;
         Next_Association_Interface (El, Inter);
         Pos := Pos + 1;
      end loop;

      if Is_Function or else Info.Subprg_Params_Type = O_Tnode_Null then
         null;
      else
         if Is_Suspendable then
            Close_Temp;

            --  Release stack2 memory.
            Release_Stack2 (Get_Var (Call_Info.Call_State_Mark));

            --  End of call.
            State_Jump (Next_State);
            Finish_If_Stmt (If_Blk);
            State_Start (Next_State);
         else
            Release_Stack2 (Get_Var (Mark_Var));
            Close_Local_Temp;
            Finish_Declare_Stmt;
         end if;
      end if;

      return O_Enode_Null;
   end Translate_Subprogram_Call;

   procedure Translate_Procedure_Call (Stmt : Iir_Procedure_Call)
   is
      Assoc_Chain : constant Iir := Get_Parameter_Association_Chain (Stmt);
      Obj : constant Iir := Get_Method_Object (Stmt);
      Res : O_Enode;
   begin
      Res := Translate_Subprogram_Call (Stmt, Assoc_Chain, Obj);
      pragma Assert (Res = O_Enode_Null);
   end Translate_Procedure_Call;

   procedure Translate_Wait_Statement (Stmt : Iir)
   is
      Cond        : constant Iir := Get_Condition_Clause (Stmt);
      Timeout     : constant Iir := Get_Timeout_Clause (Stmt);
      Sensitivity : Iir_List;
      Constr      : O_Assoc_List;
      Resume_State : State_Type;
   begin
      Sensitivity := Get_Sensitivity_List (Stmt);
      if Sensitivity = Null_Iir_List and Cond /= Null_Iir then
         --  Extract sensitivity from condition.
         Sensitivity := Create_Iir_List;
         Vhdl.Canon.Canon_Extract_Sensitivity_Expression (Cond, Sensitivity);
         Set_Sensitivity_List (Stmt, Sensitivity);
      end if;

      --  The wait statement must be within a suspendable process/subprogram.
      pragma Assert (State_Enabled);

      Resume_State := State_Allocate;

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
            Open_Temp;
            Start_Association (Constr, Ghdl_Process_Wait_Timeout);
            New_Association (Constr, Chap7.Translate_Expression
                             (Timeout, Time_Type_Definition));
            Assoc_Filename_Line (Constr, Get_Line_Number (Stmt));
            New_Procedure_Call (Constr);
            Close_Temp;
         end if;

         --  Suspend.
         State_Suspend (Resume_State);

         --  Resume point.
         State_Start (Resume_State);

         if State_Debug and then Timeout = Null_Iir then
            --  A process exit must not resume!
            Chap6.Gen_Program_Error (Stmt, Chap6.Prg_Err_Unreach_State);
         end if;

         --  End of simple cases.
         return;
      end if;

      --  Evaluate the timeout (if any) and register it,
      if Timeout /= Null_Iir then
         Start_Association (Constr, Ghdl_Process_Wait_Set_Timeout);
         New_Association (Constr, Chap7.Translate_Expression
                          (Timeout, Time_Type_Definition));
         Assoc_Filename_Line (Constr, Get_Line_Number (Stmt));
         New_Procedure_Call (Constr);
      end if;

      --  Evaluate the sensitivity list and register it.
      if Sensitivity /= Null_Iir_List then
         Register_Signal_List
           (Sensitivity, Ghdl_Process_Wait_Add_Sensitivity);
         Chap9.Destroy_Types_In_List (Sensitivity);
      end if;

      --  suspend ();
      --  FIXME: this just sets the state, could be done in Add_Sensitivity
      --  or Set_Timeout.
      Start_Association (Constr, Ghdl_Process_Wait_Suspend);
      New_Procedure_Call (Constr);

      if Cond = Null_Iir then
         State_Suspend (Resume_State);
      else
         declare
            Eval_State : State_Type;
            If_Blk1, If_Blk2 : O_If_Block;
         begin
            Eval_State := State_Allocate;

            State_Suspend (Eval_State);

            --  EVAL_STATE:
            State_Start (Eval_State);

            --    if timed_out() then
            --      GOTO RESUME_STATE;
            --    else
            Start_Association (Constr, Ghdl_Process_Wait_Timed_Out);
            Start_If_Stmt (If_Blk1, New_Function_Call (Constr));
            State_Jump (Resume_State);
            New_Else_Stmt (If_Blk1);

            --      if condition then
            --        GOTO RESUME_STATE;
            --      else
            --        SUSPEND EVAL_STATE;
            --      end if;
            Open_Temp;
            Start_If_Stmt
              (If_Blk2,
               Chap7.Translate_Expression (Cond, Boolean_Type_Definition));
            State_Jump (Resume_State);
            New_Else_Stmt (If_Blk2);
            State_Suspend (Eval_State);
            Finish_If_Stmt (If_Blk2);
            Close_Temp;

            --    end if;
            Finish_If_Stmt (If_Blk1);
         end;
      end if;

      --  RESUME_STATE:
      --    wait_close;
      State_Start (Resume_State);
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
         when Type_Mode_Arrays =>
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
            Val2 := Create_Temp_Init (Type_Info.Ortho_Type (Mode_Value), Val);
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
            New_Association (Assoc,
                             New_Convert_Ov (New_Obj_Value (Val2), Conv));
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
         when Type_Mode_Arrays =>
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
        (Expr => Chap3.Index_Base (Chap3.Get_Composite_Base (Val.Expr),
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

   procedure Gen_Start_Signal_Assign is new Foreach_Non_Composite
     (Data_Type => Signal_Assign_Data,
      Composite_Data_Type => Signal_Assign_Data,
      Do_Non_Composite => Gen_Start_Signal_Assign_Non_Composite,
      Prepare_Data_Array => Gen_Signal_Prepare_Data_Composite,
      Update_Data_Array => Gen_Signal_Update_Data_Array,
      Prepare_Data_Record => Gen_Signal_Prepare_Data_Record,
      Update_Data_Record => Gen_Signal_Update_Data_Record);

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
         when Type_Mode_Arrays =>
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
      Prepare_Data_Record => Gen_Signal_Prepare_Data_Record,
      Update_Data_Record => Gen_Signal_Update_Data_Record);

   procedure Translate_Signal_Target_Aggr
     (Aggr : Mnode; Target : Iir; Target_Type : Iir);

   procedure Translate_Signal_Target_Array_Aggr
     (Aggr        : Mnode;
      Target      : Iir;
      Target_Type : Iir;
      Idx         : O_Dnode;
      Dim         : Natural)
   is
      Index_List : constant Iir_Flist :=
        Get_Index_Subtype_List (Target_Type);
      Nbr_Dim    : constant Natural := Get_Nbr_Elements (Index_List);
      Sub_Aggr   : Mnode;
      Sub_Type   : Iir;
      El         : Iir;
      Expr       : Iir;
   begin
      El := Get_Association_Choices_Chain (Target);
      while El /= Null_Iir loop
         Expr := Get_Associated_Expr (El);
         case Get_Kind (El) is
            when Iir_Kind_Choice_By_None =>
               if Get_Element_Type_Flag (El) then
                  Sub_Aggr := Chap3.Index_Base
                    (Aggr, Target_Type, New_Obj_Value (Idx));
                  Sub_Type := Get_Element_Subtype (Target_Type);
               else
                  Sub_Type := Get_Type (Expr);
                  if Get_Kind (Expr) = Iir_Kind_Slice_Name then
                     Chap3.Create_Composite_Subtype (Sub_Type, False);
                  end if;
                  Sub_Aggr := Chap3.Slice_Base
                    (Aggr, Sub_Type, New_Obj_Value (Idx), O_Enode_Null);
               end if;
            when others =>
               Error_Kind ("translate_signal_target_array_aggr", El);
         end case;
         if Dim = Nbr_Dim then
            Translate_Signal_Target_Aggr (Sub_Aggr, Expr, Sub_Type);
            if Get_Kind (El) = Iir_Kind_Choice_By_None then
               if Get_Element_Type_Flag (El) then
                  Inc_Var (Idx);
               else
                  New_Assign_Stmt
                    (New_Obj (Idx),
                     New_Dyadic_Op
                       (ON_Add_Ov,
                        New_Obj_Value (Idx),
                        Chap3.Get_Array_Length (Sub_Aggr, Sub_Type)));
               end if;
            else
               --  TODO
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
      El_List : constant Iir_Flist :=
        Get_Elements_Declaration_List (Get_Base_Type (Target_Type));
      Aggr_El  : Iir;
      El_Index : Natural;
      Element  : Iir_Element_Declaration;
   begin
      El_Index := 0;
      Aggr_El := Get_Association_Choices_Chain (Target);
      while Aggr_El /= Null_Iir loop
         case Get_Kind (Aggr_El) is
            when Iir_Kind_Choice_By_None =>
               Element := Get_Nth_Element (El_List, El_Index);
               El_Index := El_Index + 1;
            when Iir_Kind_Choice_By_Name =>
               Element := Get_Named_Entity (Get_Choice_Name (Aggr_El));
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
         Src := Chap6.Translate_Name (Target, Mode_Signal);
         if Get_Type_Info (Src).Type_Mode in Type_Mode_Unbounded then
            Src := Chap3.Get_Composite_Base (Src);
         end if;
         Chap3.Translate_Object_Copy (Aggr, Src, Target_Type);
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
         New_Compare_Op
           (ON_Neq,
            M2E (Chap7.Translate_Signal_Driving_Value (Targ_Sig, Targ_Type)),
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

   function Gen_Signal_Direct_Prepare_Data_Stabilize
     (Targ : Mnode; Targ_Type : Iir; Val : Signal_Direct_Assign_Data)
     return Signal_Direct_Assign_Data
   is
      pragma Unreferenced (Targ, Targ_Type);
   begin
      return Signal_Direct_Assign_Data'
        (Drv => Stabilize (Val.Drv),
         Expr => Stabilize (Val.Expr),
         Expr_Node => Val.Expr_Node);
   end Gen_Signal_Direct_Prepare_Data_Stabilize;

   function Gen_Signal_Direct_Prepare_Data_Array
     (Targ : Mnode; Targ_Type : Iir; Val : Signal_Direct_Assign_Data)
     return Signal_Direct_Assign_Data is
   begin
      if Is_Unbounded_Type (Get_Info (Targ_Type)) then
         return Gen_Signal_Direct_Prepare_Data_Stabilize
           (Targ, Targ_Type, Val);
      else
         return Val;
      end if;
   end Gen_Signal_Direct_Prepare_Data_Array;

   function Gen_Signal_Direct_Update_Data_Array
     (Val       : Signal_Direct_Assign_Data;
      Targ_Type : Iir;
      Index     : O_Dnode)
     return Signal_Direct_Assign_Data is
   begin
      return Signal_Direct_Assign_Data'
        (Drv => Chap6.Translate_Indexed_Name_By_Offset
           (Val.Drv, Targ_Type, Index),
         Expr => Chap6.Translate_Indexed_Name_By_Offset
           (Val.Expr, Targ_Type, Index),
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

   procedure Gen_Signal_Direct_Assign is new Foreach_Non_Composite
     (Data_Type => Signal_Direct_Assign_Data,
      Composite_Data_Type => Signal_Direct_Assign_Data,
      Do_Non_Composite => Gen_Signal_Direct_Assign_Non_Composite,
      Prepare_Data_Array => Gen_Signal_Direct_Prepare_Data_Array,
      Update_Data_Array => Gen_Signal_Direct_Update_Data_Array,
      Prepare_Data_Record => Gen_Signal_Direct_Prepare_Data_Stabilize,
      Update_Data_Record => Gen_Signal_Direct_Update_Data_Record);

   procedure Translate_Waveform_Expression
     (Expr : Iir; Target_Type : Iir; Targ : in out Mnode; Res : out Mnode)
   is
      Expr_Type : constant Iir := Get_Type (Expr);
   begin
      if Get_Kind (Expr) = Iir_Kind_Aggregate
        and then Get_Constraint_State (Expr_Type) /= Fully_Constrained
      then
         declare
            Expr_Tinfo : constant Type_Info_Acc := Get_Info (Expr_Type);
         begin
            --  Create a temp.
            Res := Create_Temp (Expr_Tinfo);
            --  Set bounds from target
            Stabilize (Targ);
            New_Assign_Stmt
              (M2Lp (Chap3.Get_Composite_Bounds (Res)),
               M2Addr (Chap3.Get_Composite_Bounds (Targ)));
            --  Allocate target
            Chap3.Allocate_Unbounded_Composite_Base
              (Alloc_Stack, Res, Get_Base_Type (Expr_Type));
            --  Translate aggregate
            Chap7.Translate_Aggregate (Res, Target_Type, Expr);
         end;
      else
         Res := Chap7.Translate_Expression (Expr, Target_Type);
      end if;
   end Translate_Waveform_Expression;

   procedure Translate_Direct_Signal_Assignment
     (Target : Iir; Targ : Mnode; Drv : Mnode; We : Iir)
   is
      Target_Type  : constant Iir := Get_Type (Target);
      Target_Tinfo : constant Type_Info_Acc := Get_Info (Target_Type);
      Arg          : Signal_Direct_Assign_Data;
      Val          : Mnode;
      Stable_Targ  : Mnode;
   begin
      Stable_Targ := Targ;
      Translate_Waveform_Expression (We, Target_Type, Stable_Targ, Val);
      if Is_Composite (Target_Tinfo) then
         Stabilize (Val);
         Stabilize (Stable_Targ);
         Chap3.Check_Composite_Match
           (Target_Type, Stable_Targ, Get_Type (We), Val, We);
      end if;
      Arg := (Drv => Drv,
              Expr => Val,
              Expr_Node => We);
      Gen_Signal_Direct_Assign (Stable_Targ, Target_Type, Arg);
   end Translate_Direct_Signal_Assignment;

   --  Return True iff signal assignment statement STMT has a delay mechanism:
   --  either transport or a reject delay.
   function Is_Reject_Signal_Assignment (Stmt : Iir) return Boolean is
   begin
      return Get_Delay_Mechanism (Stmt) /= Iir_Inertial_Delay
        or else Get_Reject_Time_Expression (Stmt) /= Null_Iir;
   end Is_Reject_Signal_Assignment;

   --  Return True if waveform chain WE has only one expression, ie:
   --   * no time expression
   --   * one element
   --   * not a null
   --  which corresponds to:
   --   ... <= EXPR
   function Is_Simple_Waveform (We : Iir) return Boolean is
   begin
      if We /= Null_Iir
        and then Get_Chain (We) = Null_Iir
        and then Get_Time (We) = Null_Iir
      then
         return Get_Kind (Get_We_Value (We)) /= Iir_Kind_Null_Literal;
      else
         return False;
      end if;
   end Is_Simple_Waveform;

   --  Valid only for single_signal_assignment.
   --  True iff direct assignment can be used.
   function Is_Direct_Signal_Assignment (Target : Iir) return Boolean is
   begin
      return Flag_Direct_Drivers
        and then Get_Kind (Target) /= Iir_Kind_Aggregate
        and then Chap4.Has_Direct_Driver (Target);
   end Is_Direct_Signal_Assignment;

   type Signal_Assignment_Mechanism is
     (Signal_Assignment_Direct,
      Signal_Assignment_Simple,
      Signal_Assignment_General);

   procedure Translate_Signal_Assignment_Target
     (Target : Iir;
      Mechanism : Signal_Assignment_Mechanism;
      Targ : out Mnode;
      Drv : out Mnode)
   is
      Target_Type : constant Iir := Get_Type (Target);

      Target_Tinfo : Type_Info_Acc;
      Bounds : Mnode;
   begin
      if Get_Kind (Target) = Iir_Kind_Aggregate then
         --  The target is an aggregate.
         Chap3.Translate_Anonymous_Subtype_Definition (Target_Type, False);
         Target_Tinfo := Get_Info (Target_Type);
         Targ := Create_Temp (Target_Tinfo, Mode_Signal);
         if Target_Tinfo.Type_Mode in Type_Mode_Unbounded then
            --  Unbounded array, allocate bounds.
            Bounds := Dv2M (Create_Temp (Target_Tinfo.B.Bounds_Type),
                            Target_Tinfo,
                            Mode_Value,
                            Target_Tinfo.B.Bounds_Type,
                            Target_Tinfo.B.Bounds_Ptr_Type);
            New_Assign_Stmt (M2Lp (Chap3.Get_Composite_Bounds (Targ)),
                             M2Addr (Bounds));
            --  Build bounds from aggregate.
            Chap7.Translate_Aggregate_Bounds (Bounds, Target, Mode_Signal);
            Chap3.Allocate_Unbounded_Composite_Base
              (Alloc_Stack, Targ, Target_Type);
            Translate_Signal_Target_Aggr
              (Chap3.Get_Composite_Base (Targ), Target, Target_Type);
         else
            Chap4.Allocate_Complex_Object (Target_Type, Alloc_Stack, Targ);
            Translate_Signal_Target_Aggr (Targ, Target, Target_Type);
         end if;
      else
         if Mechanism = Signal_Assignment_Direct then
            Chap6.Translate_Direct_Driver (Target, Targ, Drv);
         else
            Targ := Chap6.Translate_Name (Target, Mode_Signal);
         end if;
      end if;
   end Translate_Signal_Assignment_Target;

   procedure Translate_Waveform_Assignment
     (Stmt : Iir;
      Mechanism : Signal_Assignment_Mechanism;
      Wf_Chain : Iir;
      Targ : Mnode;
      Drv : Mnode)
   is
      Target      : constant Iir := Strip_Reference_Name (Get_Target (Stmt));
      Target_Type : constant Iir := Get_Type (Target);
      We          : Iir_Waveform_Element;
      Value       : Iir;
   begin
      if Mechanism = Signal_Assignment_Direct then
         Translate_Direct_Signal_Assignment
           (Target, Targ, Drv, Get_We_Value (Wf_Chain));
         return;
      end if;

      if Wf_Chain = Null_Iir then
         --  Implicit disconnect statment.
         Register_Signal (Targ, Target_Type, Ghdl_Signal_Disconnect);
         return;
      end if;

      --  Handle a simple and common case: only one waveform, inertial,
      --  and no time (eg: sig <= expr).
      Value := Get_We_Value (Wf_Chain);
      Signal_Assign_Line := Get_Line_Number (Value);
      if Mechanism = Signal_Assignment_Simple then
         declare
            Targ_Tinfo : constant Type_Info_Acc := Get_Info (Target_Type);
            Val : Mnode;
            Targ2 : Mnode;
         begin
            Open_Temp;
            Targ2 := Targ;
            Translate_Waveform_Expression (Value, Target_Type, Targ2, Val);
            if Is_Composite (Targ_Tinfo)
              and then Get_Constraint_State (Target_Type) /= Fully_Constrained
            then
               Stabilize (Targ2);
               Stabilize (Val);
               Chap3.Check_Composite_Match
                 (Target_Type, Targ2, Get_Type (Value), Val, Wf_Chain);
            end if;
            Gen_Simple_Signal_Assign (Targ2, Target_Type, M2E (Val));
            Close_Temp;
         end;
         return;
      end if;

      --  General case.
      declare
         Targ_Tinfo : constant Type_Info_Acc := Get_Info (Target_Type);
         Var_Targ   : Mnode;
      begin
         Open_Temp;
         Var_Targ := Stabilize (Targ, True);

         --  Translate the first waveform element.
         We := Wf_Chain;
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
               Translate_Waveform_Expression
                 (Value, Target_Type, Var_Targ, Val);
               Val := Stabilize (Val, True);
               Chap3.Check_Composite_Match
                 (Target_Type, Var_Targ, Get_Type (Value), Val, We);
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
                  Val := Chap7.Translate_Expression (Value, Target_Type);
                  if Is_Composite (Targ_Tinfo) then
                     Stabilize (Val);
                     Chap3.Check_Composite_Match
                       (Target_Type, Var_Targ, Get_Type (Value), Val, We);
                  end if;
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
   end Translate_Waveform_Assignment;

   procedure Translate_Inertial_Assignment
     (Targ : Mnode; Targ_Type : Iir; Val : Mnode; Assoc : Iir) is
   begin
      Signal_Assign_Line := Get_Line_Number (Assoc);

      Gen_Simple_Signal_Assign (Targ, Targ_Type, M2E (Val));
   end Translate_Inertial_Assignment;

   procedure Translate_Simple_Signal_Assignment_Statement (Stmt : Iir)
   is
      Target : constant Iir := Strip_Reference_Name (Get_Target (Stmt));
      Wf_Chain : constant Iir := Get_Waveform_Chain (Stmt);
      Mechanism : Signal_Assignment_Mechanism;
      Targ : Mnode;
      Drv : Mnode;
   begin
      if Is_Valid (Wf_Chain)
        and then Get_Kind (Wf_Chain) = Iir_Kind_Unaffected_Waveform
      then
         --  Unaffected, like a null statement.
         return;
      end if;

      if Is_Reject_Signal_Assignment (Stmt)
        or else not Is_Simple_Waveform (Wf_Chain)
      then
         Mechanism := Signal_Assignment_General;
      else
         if Is_Direct_Signal_Assignment (Target) then
            Mechanism := Signal_Assignment_Direct;
         else
            Mechanism := Signal_Assignment_Simple;
         end if;
      end if;

      Translate_Signal_Assignment_Target (Target, Mechanism, Targ, Drv);

      Translate_Waveform_Assignment (Stmt, Mechanism, Wf_Chain, Targ, Drv);

      Chap9.Destroy_Types (Target);
   end Translate_Simple_Signal_Assignment_Statement;

   type Selected_Assignment_Handler is new Case_Handler with record
      Stmt : Iir;
      Mechanism : Signal_Assignment_Mechanism;
      Targ : Mnode;
      Drv : Mnode;
   end record;

   procedure Case_Association_Cb
     (Assoc : Iir; Handler : in out Selected_Assignment_Handler) is
   begin
      Open_Temp;
      Translate_Waveform_Assignment
        (Handler.Stmt, Handler.Mechanism, Assoc, Handler.Targ, Handler.Drv);
      Close_Temp;
   end Case_Association_Cb;

   procedure Translate_Selected_Waveform_Assignment_Statement (Stmt : Iir)
   is
      Target : constant Iir := Get_Target (Stmt);
      Swf_Chain : constant Iir := Get_Selected_Waveform_Chain (Stmt);
      Swf : Iir;
      Wf : Iir;
      Handler : Selected_Assignment_Handler;
   begin
      Handler.Stmt := Stmt;

      --  Compute the mechanism used.
      if Is_Reject_Signal_Assignment (Stmt) then
         Handler.Mechanism := Signal_Assignment_General;
      else
         if Is_Direct_Signal_Assignment (Target) then
            Handler.Mechanism := Signal_Assignment_Direct;
         else
            Handler.Mechanism := Signal_Assignment_Simple;
         end if;
         Swf := Swf_Chain;
         while Swf /= Null_Iir loop
            Wf := Get_Associated_Chain (Swf);
            if Wf /= Null_Iir then
               if not Is_Simple_Waveform (Wf) then
                  Handler.Mechanism := Signal_Assignment_General;
                  exit;
               end if;
            end if;
            Swf := Get_Chain (Swf);
         end loop;
      end if;

      Open_Temp;

      Translate_Signal_Assignment_Target
        (Target, Handler.Mechanism, Handler.Targ, Handler.Drv);

      Handler.Targ := Stabilize (Handler.Targ, True);
      if Handler.Mechanism = Signal_Assignment_Direct then
         Handler.Drv := Stabilize (Handler.Drv, True);
      end if;

      Translate_Case (Stmt, Handler);

      Close_Temp;
   end Translate_Selected_Waveform_Assignment_Statement;

   procedure Translate_Signal_Release_Assignment_Statement (Stmt : Iir)
   is
      Target : constant Iir := Get_Target (Stmt);
      Targ : Mnode;
      Proc : O_Dnode;
   begin
      Targ := Chap6.Translate_Name (Target, Mode_Signal);
      case Get_Force_Mode (Stmt) is
         when Iir_Force_In =>
            Proc := Ghdl_Signal_Release_Eff;
         when Iir_Force_Out =>
            Proc := Ghdl_Signal_Release_Drv;
      end case;
      Register_Signal (Targ, Get_Type (Target), Proc);
   end Translate_Signal_Release_Assignment_Statement;

   Signal_Force_Stmt : Iir;
   procedure Gen_Signal_Force_Non_Composite (Targ      : Mnode;
                                             Targ_Type : Iir;
                                             Val       : O_Enode)
   is
      Type_Info : constant Type_Info_Acc := Get_Info (Targ_Type);
      Subprg    : O_Dnode;
      Conv      : O_Tnode;
      Assoc     : O_Assoc_List;
      Val2      : O_Enode;
   begin
      case Type_Mode_Scalar (Type_Info.Type_Mode) is
         when Type_Mode_B1 =>
            case Get_Force_Mode (Signal_Force_Stmt) is
               when Iir_Force_In =>
                  Subprg := Ghdl_Signal_Force_Eff_B1;
               when Iir_Force_Out =>
                  Subprg := Ghdl_Signal_Force_Drv_B1;
            end case;
            Conv := Ghdl_Bool_Type;
         when Type_Mode_E8 =>
            case Get_Force_Mode (Signal_Force_Stmt) is
               when Iir_Force_In =>
                  Subprg := Ghdl_Signal_Force_Eff_E8;
               when Iir_Force_Out =>
                  Subprg := Ghdl_Signal_Force_Drv_E8;
            end case;
            Conv := Ghdl_I32_Type;
         when Type_Mode_E32 =>
            case Get_Force_Mode (Signal_Force_Stmt) is
               when Iir_Force_In =>
                  Subprg := Ghdl_Signal_Force_Eff_E32;
               when Iir_Force_Out =>
                  Subprg := Ghdl_Signal_Force_Drv_E32;
            end case;
            Conv := Ghdl_I32_Type;
         when Type_Mode_I32
            | Type_Mode_P32 =>
            case Get_Force_Mode (Signal_Force_Stmt) is
               when Iir_Force_In =>
                  Subprg := Ghdl_Signal_Force_Eff_I32;
               when Iir_Force_Out =>
                  Subprg := Ghdl_Signal_Force_Drv_I32;
            end case;
            Conv := Ghdl_I32_Type;
         when Type_Mode_P64
            | Type_Mode_I64 =>
            case Get_Force_Mode (Signal_Force_Stmt) is
               when Iir_Force_In =>
                  Subprg := Ghdl_Signal_Force_Eff_I64;
               when Iir_Force_Out =>
                  Subprg := Ghdl_Signal_Force_Drv_I64;
            end case;
            Conv := Ghdl_I64_Type;
         when Type_Mode_F64 =>
            case Get_Force_Mode (Signal_Force_Stmt) is
               when Iir_Force_In =>
                  Subprg := Ghdl_Signal_Force_Eff_F64;
               when Iir_Force_Out =>
                  Subprg := Ghdl_Signal_Force_Drv_F64;
            end case;
            Conv := Ghdl_Real_Type;
      end case;
      Val2 := Chap3.Insert_Scalar_Check
        (Val, Null_Iir, Targ_Type, Signal_Force_Stmt);
      Start_Association (Assoc, Subprg);
      New_Association (Assoc, New_Convert_Ov (New_Value (M2Lv (Targ)),
                                              Ghdl_Signal_Ptr));
      New_Association (Assoc, New_Convert_Ov (Val2, Conv));
      New_Procedure_Call (Assoc);
   end Gen_Signal_Force_Non_Composite;

   procedure Gen_Signal_Force is new Foreach_Non_Composite
     (Data_Type => O_Enode,
      Composite_Data_Type => Mnode,
      Do_Non_Composite => Gen_Signal_Force_Non_Composite,
      Prepare_Data_Array => Gen_Oenode_Prepare_Data_Composite,
      Update_Data_Array => Gen_Oenode_Update_Data_Array,
      Finish_Data_Array => Gen_Oenode_Finish_Data_Composite,
      Prepare_Data_Record => Gen_Oenode_Prepare_Data_Composite,
      Update_Data_Record => Gen_Oenode_Update_Data_Record,
      Finish_Data_Record => Gen_Oenode_Finish_Data_Composite);

   procedure Translate_Signal_Force_Assignment_Statement (Stmt : Iir)
   is
      Target : constant Iir := Get_Target (Stmt);
      Target_Type : constant Iir := Get_Type (Target);
      Targ_Tinfo : constant Type_Info_Acc := Get_Info (Target_Type);
      Expr : constant Iir := Get_Expression (Stmt);
      Value : Mnode;
      Targ  : Mnode;
   begin
      Targ := Chap6.Translate_Name (Target, Mode_Signal);
      Value := Chap7.Translate_Expression (Expr, Target_Type);

      if Is_Composite (Targ_Tinfo)
        and then Get_Constraint_State (Target_Type) /= Fully_Constrained
      then
         Stabilize (Targ);
         Stabilize (Value);
         Chap3.Check_Composite_Match
           (Target_Type, Targ, Get_Type (Expr), Value, Stmt);
      end if;

      Signal_Force_Stmt := Stmt;
      Gen_Signal_Force (Targ, Target_Type, M2E (Value));
   end Translate_Signal_Force_Assignment_Statement;

   procedure Translate_Statement (Stmt : Iir) is
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

         when Iir_Kind_Simple_Signal_Assignment_Statement =>
            Translate_Simple_Signal_Assignment_Statement (Stmt);
         when Iir_Kind_Selected_Waveform_Assignment_Statement =>
            Translate_Selected_Waveform_Assignment_Statement (Stmt);
         when Iir_Kind_Variable_Assignment_Statement =>
            Translate_Variable_Assignment_Statement (Stmt);
         when Iir_Kind_Conditional_Variable_Assignment_Statement =>
            declare
               C_Stmt : Iir;
            begin
               C_Stmt :=
                 Vhdl.Canon.Canon_Conditional_Variable_Assignment_Statement
                 (Stmt);
               Trans.Update_Node_Infos;
               Translate_If_Statement (C_Stmt);
            end;
         when Iir_Kind_Conditional_Signal_Assignment_Statement =>
            declare
               C_Stmt : Iir;
            begin
               C_Stmt :=
                 Vhdl.Canon.Canon_Conditional_Signal_Assignment_Statement
                 (Stmt);
               Trans.Update_Node_Infos;
               Translate_If_Statement (C_Stmt);
            end;
         when Iir_Kind_Signal_Release_Assignment_Statement =>
            Translate_Signal_Release_Assignment_Statement (Stmt);
         when Iir_Kind_Signal_Force_Assignment_Statement =>
            Translate_Signal_Force_Assignment_Statement (Stmt);

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
               if not Get_Suspend_Flag (Stmt) then
                  --  Suspendable calls were already canonicalized.
                  Vhdl.Canon.Canon_Subprogram_Call (Call);
                  Trans.Update_Node_Infos;
               end if;

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
