--  Verilog semantic analyzer (statements)
--  Copyright (C) 2023 Tristan Gingold
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
--  along with this program.  If not, see <gnu.org/licenses>.

with Types; use Types;
with Std_Names;
with Errorout; use Errorout;
with Verilog.Nutils; use Verilog.Nutils;
with Verilog.Errors; use Verilog.Errors;
with Verilog.Standard; use Verilog.Standard;
with Verilog.Sem; use Verilog.Sem;
with Verilog.Sem_Utils;
with Verilog.Sem_Scopes; use Verilog.Sem_Scopes;
with Verilog.Sem_Expr; use Verilog.Sem_Expr;
with Verilog.Sem_Types; use Verilog.Sem_Types;
with Verilog.Sem_Names; use Verilog.Sem_Names;
with Verilog.Vpi;

package body Verilog.Sem_Stmts is
   --  The current task/function.  Used to analyze return statements.
   Current_Tf : Node := Null_Node;

   procedure Sem_Systf_Argument (Arg : Node; Err : in out Boolean)
   is
      Expr : Node;
      Decl : Node;
   begin
      Expr := Get_Expression (Arg);
      if Expr = Null_Node then
         --  Argument is optional.
         return;
      end if;

      --  Note: arguments may not be expressions (can be hierarchical names)
      case Get_Kind (Expr) is
         when N_Name
           | N_Scoped_Name
           | N_Hierarchical =>
            Expr := Sem_Name (Expr);
            Set_Expression (Arg, Expr);
            Decl := Get_Declaration (Expr);
            if Decl = Null_Node then
               Err := True;
               return;
            end if;

            case Get_Kind (Decl) is
               when N_Typedef
                 | N_Type_Parameter =>
                  return;
               when others =>
                  --  Analyze as expression.
                  null;
            end case;
         when others =>
            --  Analyze as expression.
            null;
      end case;

      Expr := Sem_Expression (Expr, Null_Node);
      Set_Expression (Arg, Expr);
      if Get_Expr_Type (Expr) = Null_Node then
         Err := True;
      end if;
   end Sem_Systf_Argument;

   --  Analyze arguments of a system task enable or of a system function call.
   procedure Sem_Systf_Arguments (Call : Node; Err : out Boolean)
   is
      Arg : Node;
   begin
      Err := False;
      Arg := Get_Arguments (Call);
      while Arg /= Null_Node loop
         Sem_Systf_Argument (Arg, Err);
         Arg := Get_Chain (Arg);
      end loop;
   end Sem_Systf_Arguments;

   procedure Sem_System_Function_Call (Expr : Node; Etype : Node)
   is
      pragma Unreferenced (Etype);
      Id : constant Name_Id := Get_Identifier (Expr);
      Sys_Id : Sys_Tf_Id;
      Ftype : Node;
      Func_Type : Vpi.PLI_INT32_FuncType;
      Err : Boolean;
   begin
      Ftype := Error_Type;

      --  Analyze arguments.
      Sem_Systf_Arguments (Expr, Err);

      Sys_Id := Verilog.Vpi.Find_Sysfunc (Id);
      Set_Sys_Tf_Id (Expr, Sys_Id);
      if Sys_Id = No_Sys_Tf_Id then
         Error_Msg_Sem (+Expr, "system function %i is not known", +Id);
      elsif Sys_Id = Bad_Sys_Tf_Id then
         Error_Msg_Sem (+Expr, "%i is registered as a task", +Id);
      elsif Err then
         --  Do not try to validate the call in case of error in an argument.
         null;
      elsif Sys_Id < Sys_Tf_User_Id then
         case Sys_Id is
            when Sys_Tf_Signed_Id =>
               Sem_Sign_System_Function_Call (Expr, True);
            when Sys_Tf_Unsigned_Id =>
               Sem_Sign_System_Function_Call (Expr, False);
            when Sys_Tf_Cast_Id =>
               Sem_Cast_System_Function_Call (Expr);
            when Sys_Tf_Typename_Id =>
               Sem_Typename_System_Function_Call (Expr);
            when Sys_Tf_Left_Id
              | Sys_Tf_Right_Id
              | Sys_Tf_Low_Id
              | Sys_Tf_High_Id
              | Sys_Tf_Size_Id =>
               Sem_Array_Dimension_System_Function_Call (Expr);
            when others =>
               raise Internal_Error;
         end case;
         return;
      else
         Vpi.Call_Systf_Compiletf (Sys_Id, Expr);

         Func_Type := Vpi.Get_Sysfunc_Type (Sys_Id);
         case Func_Type is
            when Vpi.VpiIntFunc =>
               Ftype := Signed_Integer_Type;
            when Vpi.VpiTimeFunc =>
               Ftype := Unsigned_Time_Type;
            when Vpi.VpiRealFunc =>
               Ftype := Real_Type;
            when Vpi.VpiSizedFunc =>
               declare
                  Width : Int32;
               begin
                  Width := Int32 (Vpi.Call_Systf_Sizetf (Sys_Id));
                  Ftype := Get_Packed_Array_Type
                    (Width - 1, 0, Unsigned_Logic_Type, False);
               end;
            when Vpi.VpiStringFunc =>
               Ftype := String_Type;
            when others =>
               raise Program_Error;
         end case;
      end if;
      Set_Expr_Type (Expr, Ftype);
   end Sem_System_Function_Call;

   procedure Sem_System_Task (Stmt : Node)
   is
      Sys_Id : Sys_Tf_Id;
      Id : constant Name_Id := Get_Identifier (Stmt);
      Err : Boolean;
   begin
      --  Analyze arguments.
      Sem_Systf_Arguments (Stmt, Err);

      --  Check result.
      Sys_Id := Verilog.Vpi.Find_Systask (Id);

      if Sys_Id = No_Sys_Tf_Id then
         Error_Msg_Sem (+Stmt, "task %i is not known", +Id);
      elsif Sys_Id = Bad_Sys_Tf_Id then
         Error_Msg_Sem (+Stmt, "%i is registered as a function", +Id);
      elsif Err then
         --  Do not try to validate the call in case of error in an argument.
         null;
      else
         Set_Sys_Tf_Id (Stmt, Sys_Id);

         if Sys_Id = Sys_Tf_Cast_Id then
            Sem_Cast_System_Function_Call (Stmt);
         else
            pragma Assert (Sys_Id >= Sys_Tf_User_Id);
            Verilog.Vpi.Call_Systf_Compiletf (Sys_Id, Stmt);
         end if;
      end if;
   end Sem_System_Task;

   procedure Sem_Subroutine_Call_Stmt (Stmt : Node)
   is
      Call : constant Node := Get_Call (Stmt);
   begin
      case Get_Kind (Call) is
         when N_System_Call =>
            if Get_Has_Void_Cast (Stmt) then
               Sem_System_Function_Call (Call, Void_Type);
            else
               Sem_System_Task (Call);
            end if;
         when N_Call =>
            declare
               Sub_Name : Node;
               Sub : Node;
               Call1 : Node;
            begin
               Sem_Subroutine_Call_Name (Call);
               Sub_Name := Get_Subroutine (Call);
               Sub := Get_Declaration (Sub_Name);
               --  Handle overload for rand_mode...
               if Sub = Rand_Mode_Func_Method then
                  Sub := Rand_Mode_Task_Method;
                  Set_Declaration (Sub_Name, Sub);
               end if;
               Call1 := Sem_Subroutine_Call_Suffix (Call);
               pragma Assert (Call1 = Call);
               if Sub = Null_Node then
                  null;
               else
                  case Get_Kind (Sub) is
                     when N_Task
                       | N_Extern_Task =>
                        if Get_Has_Void_Cast (Stmt) then
                           Error_Msg_Sem (+Call, "cast to void for task call");
                        end if;
                     when N_Function
                       | N_Extern_Function
                       | N_Import_DPI_Function =>
                        if Get_Has_Void_Cast (Stmt) then
                           if Get_Expr_Type (Call) = Void_Type then
                              Warning_Msg_Sem
                                (+Call,
                                 "unneeded cast to void for a function call");
                           end if;
                        else
                           --  1800-2017 Return values and void functions
                           --  Calling a nonvoid function as if it has no
                           --  return value shall be legal, but shall issue a
                           --  warning.
                           if Get_Expr_Type (Call) /= Void_Type then
                              Warning_Msg_Sem
                                (+Call,
                                 "missing cast to void for a function call");
                           end if;
                        end if;
                     when others =>
                        raise Internal_Error;
                  end case;
               end if;
            end;
         when N_Array_Method_Call =>
            Sem_Array_Method_Call_With (Call);
            --  TODO: check for missing cast ?
         when others =>
            Error_Kind ("sem_subroutine_call_stmt", Call);
      end case;
   end Sem_Subroutine_Call_Stmt;

   procedure Sem_Delay_Or_Event_Control (Ctrl : Node)
   is
      Expr : Node;
      Expr_Type : Node;
   begin
      if Ctrl = Null_Node then
         return;
      end if;

      case Get_Kind (Ctrl) is
         when N_Delay_Control =>
            Expr := Get_Expression (Ctrl);
            Expr := Sem_Expression (Expr, Null_Node);
            Set_Expression (Ctrl, Expr);
         when N_Event_Control =>
            Expr := Get_Expression (Ctrl);
            Expr := Sem_Event_Expression (Expr);
            Set_Expression (Ctrl, Expr);
         when N_Repeat_Control =>
            Expr := Sem_Expression (Get_Expression (Ctrl), Null_Node);
            if Expr /= Null_Node then
               Expr_Type := Get_Expr_Type (Expr);
               if Expr_Type /= Null_Node
                 and then not Is_Integral_Type (Expr_Type)
               then
                  Error_Msg_Sem
                    (+Expr, "integral expected for repeat control");
               end if;
               Set_Expression (Ctrl, Expr);
            end if;
            Sem_Delay_Or_Event_Control (Get_Control (Ctrl));
         when others =>
            Error_Kind ("sem_delay_or_event_control", Ctrl);
      end case;
   end Sem_Delay_Or_Event_Control;

   --  Handle blocking and non-blocking assignments.
   procedure Sem_Procedural_Assign (Stmt : Node)
   is
      Lval : Node;
      Lval_Type : Node;
      Expr : Node;
   begin
      Lval := Get_Lvalue (Stmt);
      Lval := Sem_Lvalue (Lval, Allow_Var => True);
      Set_Lvalue (Stmt, Lval);
      Lval_Type := Get_Expr_Type (Lval);
      if Lval_Type = Null_Node then
         --  Error.
         return;
      end if;

      --  IEEE 1800-2012 10.7
      --  The size of the left-hand size of an assignment forms the context for
      --  the right-hand expression.
      Expr := Sem_Expression (Get_Expression (Stmt), Lval_Type);
      Expr := Insert_Assignment_Compatible (Lval_Type, Expr, Stmt);
      Set_Expression (Stmt, Expr);

      Sem_Delay_Or_Event_Control (Get_Control (Stmt));
   end Sem_Procedural_Assign;

   procedure Sem_Contribution (Stmt : Node)
   is
      Lval : Node;
      Lval_Type : Node;
      Expr : Node;
   begin
      Lval := Get_Lvalue (Stmt);
      Lval := Sem_Branch_Lvalue (Lval);
      Set_Lvalue (Stmt, Lval);
      Lval_Type := Get_Expr_Type (Lval);
      if Lval_Type = Null_Node then
         --  Error.
         return;
      end if;

      Expr := Sem_Expression (Get_Expression (Stmt), Lval_Type);
      Set_Expression (Stmt, Expr);
   end Sem_Contribution;

   procedure Sem_Statement_Or_Null (Stmt : Node) is
   begin
      if Stmt /= Null_Node then
         Sem_Statement (Stmt);
      end if;
   end Sem_Statement_Or_Null;

   procedure Sem_Conditional_Statement (Stmt : Node)
   is
      Br : Node;
   begin
      Sem_Cond_Expression (Stmt);

      Br := Get_True_Stmt (Stmt);
      Sem_Statement (Br);

      Br := Get_False_Stmt (Stmt);
      if Br /= Null_Node then
         Sem_Statement (Br);
      end if;
   end Sem_Conditional_Statement;

   procedure Sem_For_Statement (Stmt : Node)
   is
      Init : Node;
   begin
      Init := Get_For_Initialization (Stmt);
      if Init /= Null_Node then
         if Get_Kind (Init) = N_Var then
            --  1800-2017 12.7.1 The for-loop
            --  The variables used to control a for-loop can also be declared
            --  within the loop, as part of the for_initialization assignments.
            --  This creates an implicit begin-end block around the loop,
            --  containing declarations of the loop variables with automatic
            --  lifetime.
            Set_Scope_Flag (Stmt, True);
            Open_Name_Space;
            while Init /= Null_Node loop
               Sem_Var (Init);
               Set_Is_Automatic (Init, True);
               Init := Get_Chain (Init);
            end loop;
         else
            Sem_Statement (Init);
         end if;
      end if;

      Sem_Cond_Expression (Stmt);
      Sem_Statement (Get_Step_Assign (Stmt));
      Sem_Statement (Get_Statement (Stmt));

      if Get_Scope_Flag (Stmt) then
         Close_Name_Space;
      end if;
   end Sem_For_Statement;

   --  1800-2017 12.7.3 The foreach-loop
   --  Also use for N_Constraint_Foreach
   procedure Sem_Foreach_Variables (Stmt : Node)
   is
      Vars : constant Node := Get_Foreach_Variables (Stmt);
      Var : Node;
      Arr : Node;
      Atype : Node;
   begin
      Arr := Get_Foreach_Array (Stmt);
      Arr := Sem_Name (Arr);
      Set_Foreach_Array (Stmt, Arr);
      --  FIXME: check is variable/net ?

      Atype := Get_Expr_Type (Arr);
      Var := Vars;
      loop
         if Atype /= Null_Node then
            case Get_Kind (Atype) is
               when N_Log_Packed_Array_Cst
                 | N_Bit_Packed_Array_Cst
                 | N_Array_Cst
                 | N_Dynamic_Array_Cst
                 | N_Queue_Cst
                 | N_String_Type =>
                  --  1800-2017 12.7.3 The foreach-loop
                  --  For fixed-size and dynamic arrays, the auto-cast type is
                  --  INT.
                  --  FIXME: also for queues ?
                  Set_Expr_Type (Var, Signed_Int_Type);
               when N_Associative_Array_Cst =>
                  --  1800-2017 12.7.3 The foreach-loop
                  --  For associative arrays indexed by a specific index type,
                  --  the auto-cast type is the same as the index type.
                  Set_Expr_Type (Var, Get_Type_Index_Type (Atype));
               when others =>
                  Error_Msg_Sem
                    (+Var, "identifier does not designate an array "
                       & "type for loop variable %i", +Var);
                  exit;
            end case;
         end if;

         Set_Is_Automatic (Var, True);

         Var := Get_Chain (Var);
         exit when Var = Null_Node;
         --  FIXME: string has no element.
         Arr := Get_Type_Element_Type (Arr);
      end loop;
   end Sem_Foreach_Variables;

   --  1800-2017 12.7.3 The foreach-loop
   procedure Sem_Foreach_Statement (Stmt : Node) is
   begin
      Sem_Foreach_Variables (Stmt);
      Sem_Statement (Get_Statement (Stmt));
   end Sem_Foreach_Statement;

   --  1800-2017 12.5 Case statement
   procedure Sem_Case_Statement (Stmt : Node)
   is
      Expr : Node;
      Expr_Type : Node;
      Item : Node;
      Item_Type : Node;
   begin
      Expr := Sem_Sub_Expression (Get_Expression (Stmt), Null_Node);
      Set_Expression (Stmt, Expr);
      Expr_Type := Get_Expr_Type (Expr);
      if Expr_Type /= Null_Node then
         if Get_Kind (Expr_Type) = N_String_Type then
            Expr_Type := Unsigned_Logic_Type;
         elsif not Is_Integral_Type (Expr_Type) then
            --  FIXME: ref ?
            --  Note: uvm uses case statements with strings.
            Error_Msg_Sem (+Expr, "case expression must be an integeral type");
            Expr_Type := Unsigned_Logic_Type;
         end if;
      else
         Expr_Type := Unsigned_Logic_Type;
      end if;

      Item := Get_Case_Items (Stmt);
      while Item /= Null_Node loop
         case Nkinds_Case_Item (Get_Kind (Item)) is
            when N_Default_Case_Item =>
               null;
            when N_Case_Item =>
               --  FIXME: use expression type ?
               Expr := Sem_Sub_Expression (Get_Expression (Item), Null_Node);
               Set_Expression (Item, Expr);
               Item_Type := Get_Expr_Type (Expr);
               if Item_Type /= Null_Node then
                  if Is_Integral_Type (Item_Type) then
                     Expr_Type :=
                       Sem_Binary_Expression_Type (Expr_Type, Item_Type);
                  else
                     --  FIXME: ref ?
                     Error_Msg_Sem
                       (+Expr,
                        "case item expression must be an integral type");
                  end if;
               end if;
         end case;
         Sem_Statement_Or_Null (Get_Statement (Item));
         Item := Get_Chain (Item);
         exit when Item = Null_Node;
      end loop;

      --  1800-2017 12.5 Case statement
      --  Therefore, the length of all the case_item_expressions, as well as
      --  the case_expression, shall be made equal to the length of the
      --  longest case_expression and case_item_expressions.  If any of these
      --  expressions is unsigned, then all of them shall be treated as
      --  unsigned.  If all of these expressions are signed, then they shall
      --  be treated as signed.
      Expr := Get_Expression (Stmt);
      Expr := Sem_Propagate_Length (Expr, Expr_Type);
      Set_Expression (Stmt, Expr);

      Item := Get_Case_Items (Stmt);
      while Item /= Null_Node loop
         case Nkinds_Case_Item (Get_Kind (Item)) is
            when N_Default_Case_Item =>
               null;
            when N_Case_Item =>
               Expr := Get_Expression (Item);
               Expr := Sem_Propagate_Length (Expr, Expr_Type);
               Set_Expression (Item, Expr);
         end case;
         Item := Get_Chain (Item);
         exit when Item = Null_Node;
      end loop;
   end Sem_Case_Statement;

   --  Return True iff STMT is contained in a loop statement.  Used to
   --  implement:
   --
   --  1800-2017 12.8 Jump statements
   --  The CONTINUE and BREAK statements can only be used in a loop.
   function Is_In_Loop (Stmt : Node) return Boolean
   is
      Parent : Node;
   begin
      Parent := Get_Parent (Stmt);
      loop
         case Get_Kind (Parent) is
            when N_Forever
              | N_Repeat
              | N_While
              | N_For
              | N_Do_While
              | N_Foreach =>
               --  1800-2017 12.7 Loop statements
               --  loop_statements ::=
               --      forever ...
               --    | repeat ...
               --    | while ...
               --    | for ...
               --    | do .. while ..
               --    | foreach ...
               return True;
            when N_Seq_Block
              | N_If
              | Nkinds_Case
              | N_Wait =>
               --  Complex statements.
               null;
            when N_Task
              | N_Function
              | Nkinds_Process =>
               return False;
            when N_Par_Block =>
               --  1800-2017 12.8 Jump statements
               --  The CONTINUE and BREAK statement cannot be used inside a
               --  fork-join block to control a loop outside the fork-join
               --  block.
               Error_Msg_Sem (+Stmt, "cannot jump outside a fork-join block");
               return True;
            when others =>
               Error_Kind ("is_in_loop", Parent);
         end case;
         Parent := Get_Parent (Parent);
      end loop;
   end Is_In_Loop;

   procedure Sem_Return_Statement (Stmt : Node)
   is
      Expr : Node;
      Func_Type : Node;
   begin
      --  1800-2017 12.8 Jump statements
      --  The return statement can only be used in a subroutine.
      if Current_Tf = Null_Node then
         Error_Msg_Sem (+Stmt, "return statement outside of function/task");
         return;
      end if;

      Expr := Get_Expression (Stmt);
      case Nkinds_Any_Tf (Get_Kind (Current_Tf)) is
         when N_Task
           | N_OOB_Task
           | N_Extern_Task =>
            if Expr /= Null_Node then
               Error_Msg_Sem
                 (+Stmt, "return statement in task cannot have expression");
            end if;
         when N_Function
           | N_OOB_Function
           | N_Extern_Function =>
            if Get_Identifier (Current_Tf) = Std_Names.Name_New then
               if Expr /= Null_Node then
                  Error_Msg_Sem
                    (+Stmt, "return in a constructor cannot have expression");
               end if;
            else
               Set_Return_Variable_Ref
                 (Stmt, Get_Return_Variable (Current_Tf));
               Func_Type := Get_Type_Data_Type (Current_Tf);
               if Func_Type = Void_Type then
                  if Expr /= Null_Node then
                     Error_Msg_Sem (+Stmt,
                                    "return statement in void function "
                                      & "cannot have expression");
                  end if;
               else
                  if Expr = Null_Node then
                     Error_Msg_Sem
                       (+Stmt, "expression expected in return statement");
                  else
                     Expr := Sem_Expression (Expr, Func_Type);
                     Expr := Insert_Assignment_Compatible
                       (Func_Type, Expr, Stmt);
                     Set_Expression (Stmt, Expr);
                  end if;
               end if;
            end if;
      end case;
   end Sem_Return_Statement;

   procedure Sem_Statement_Lifetime (Stmt : Node)
   is
      Parent : constant Node := Get_Parent (Stmt);
   begin
      case Get_Kind (Parent) is
         when Nkinds_Process =>
            Set_Is_Automatic (Stmt, False);
            Set_Lifetime (Stmt, Life_Static);
         when Nkinds_Any_Tf
            | N_Seq_Block
            | N_Par_Block
            | N_If
            | N_For
            | N_While
            | N_Do_While
            | N_Foreach
            | N_Forever
            | N_Repeat
            | N_Simple_Immediate_Assert
            | Nkinds_Case
            | N_Label_Stmt =>
            Set_Lifetime (Stmt, Get_Lifetime (Parent));
            Set_Is_Automatic (Stmt, Get_Is_Automatic (Parent));
         when N_Delay_Control
           | N_Event_Control
           | N_Wait =>
            Set_Lifetime (Stmt, Life_Static);
            Set_Is_Automatic (Stmt, False);
         when others =>
            Error_Kind ("sem_statement_lifetime", Parent);
      end case;
   end Sem_Statement_Lifetime;

   procedure Sem_Statements_Chain (Head : Node)
   is
      Stmt : Node;
   begin
      Stmt := Head;
      while Stmt /= Null_Node loop
         Sem_Statement (Stmt);
         Stmt := Get_Chain (Stmt);
      end loop;
   end Sem_Statements_Chain;

   procedure Sem_Statements (Parent : Node) is
   begin
      Sem_Statements_Chain (Get_Statements_Chain (Parent));
   end Sem_Statements;

   procedure Sem_Subroutine_Statements (Rtn : Node)
   is
      Stmts : Node;
   begin
      pragma Assert (Current_Tf = Null_Node);
      Current_Tf := Rtn;
      Stmts := Get_Statements_Chain (Rtn);

      if Get_Identifier (Rtn) = Std_Names.Name_New then
         --  A constructor.  The first statement can be a call to the parent
         --  constructor.
         if Sem_Utils.Is_Call_To_Super_New (Stmts) then
            Sem_Subroutine_Call_Stmt (Stmts);
            Stmts := Get_Chain (Stmts);
         end if;
      end if;
      Sem_Statements_Chain (Stmts);
      Current_Tf := Null_Node;
   end Sem_Subroutine_Statements;

   procedure Sem_Immediate_Assert (Stmt : Node) is
   begin
      Sem_Statement_Lifetime (Stmt);
      Sem_Cond_Expression (Stmt);
      Sem_Statement_Or_Null (Get_Pass_Stmt (Stmt));
      Sem_Statement_Or_Null (Get_Else_Stmt (Stmt));
   end Sem_Immediate_Assert;

   procedure Sem_Statement (Stmt : Node) is
   begin
      case Get_Kind (Stmt) is
         when N_For =>
            Sem_Statement_Lifetime (Stmt);
            Sem_For_Statement (Stmt);
         when N_While
           | N_Wait =>
            Sem_Statement_Lifetime (Stmt);
            Sem_Cond_Expression (Stmt);
            Sem_Statement_Or_Null (Get_Statement (Stmt));
         when N_Do_While =>
            Sem_Statement_Lifetime (Stmt);
            Sem_Statement_Or_Null (Get_Statement (Stmt));
            Sem_Cond_Expression (Stmt);
         when N_Foreach =>
            Sem_Statement_Lifetime (Stmt);
            Sem_Foreach_Statement (Stmt);
         when N_Repeat =>
            Sem_Statement_Lifetime (Stmt);
            Set_Is_Automatic (Stmt, Get_Lifetime (Stmt) = Life_Automatic);
            Set_Data_Type (Stmt, Unsigned_Int_Type);
            Set_Expression
              (Stmt, Sem_Expression (Get_Expression (Stmt), Null_Node));
            Sem_Statement_Or_Null (Get_Statement (Stmt));
         when N_Forever =>
            Sem_Statement_Lifetime (Stmt);
            Sem_Statement_Or_Null (Get_Statement (Stmt));
         when N_If =>
            Sem_Statement_Lifetime (Stmt);
            Sem_Conditional_Statement (Stmt);
         when Nkinds_Case =>
            Sem_Statement_Lifetime (Stmt);
            Sem_Case_Statement (Stmt);
         when N_Blocking_Assign =>
            Sem_Procedural_Assign (Stmt);
         when N_Pack_Assign =>
            declare
               Lval : Node;
            begin
               Lval := Sem_Lvalue (Get_Lvalue (Stmt), Allow_Var => True);
               Set_Lvalue (Stmt, Lval);

               Sem_Streaming_Concatenation (Get_Expression (Stmt));
               Sem_Delay_Or_Event_Control (Get_Control (Stmt));
            end;
         when N_Unpack_Assign =>
            --  TODO
            Sem_Streaming_Concatenation (Get_Lvalue (Stmt));
         when N_Pack_Unpack_Assign =>
            --  TODO
            Sem_Streaming_Concatenation (Get_Lvalue (Stmt));
            Sem_Streaming_Concatenation (Get_Expression (Stmt));
         when N_Noblk_Assign =>
            Sem_Procedural_Assign (Stmt);
         when N_Subroutine_Call_Stmt =>
            Sem_Subroutine_Call_Stmt (Stmt);
         when N_Seq_Block
           | N_Par_Block =>
            --  1800-2017 9.3.4 Block names
            --  An unnamed block creates a new hierarchy scope only if it
            --  directly contains a block iem declaration.
            declare
               Decls : constant Node :=
                 Get_Block_Item_Declaration_Chain (Stmt);
               Has_Scope : Boolean;
            begin
               Has_Scope := Get_Identifier (Stmt) /= Null_Identifier
                 or else Decls /= Null_Node;
               if Has_Scope then
                  Open_Name_Space;
               end if;
               Sem_Statement_Lifetime (Stmt);
               Sem_Block_Items_Declaration (Decls);
               Sem_Statements (Stmt);
               if Has_Scope then
                  Close_Name_Space;
               end if;
            end;
         when Nkinds_Inc_Dec
           | N_Assign_Operator =>
            declare
               Expr : Node;
               pragma Unreferenced (Expr);
            begin
               Expr := Sem_Expression (Stmt, Null_Node);
            end;
         when N_Continue_Stmt =>
            if not Is_In_Loop (Stmt) then
               Error_Msg_Sem (+Stmt, "continue statement not in a loop");
            end if;
         when N_Break_Stmt =>
            if not Is_In_Loop (Stmt) then
               Error_Msg_Sem (+Stmt, "break statement not in a loop");
            end if;
         when N_Return_Stmt =>
            Sem_Return_Statement (Stmt);
         when N_Repeat_Control
           | N_Delay_Control
           | N_Event_Control =>
            Sem_Delay_Or_Event_Control (Stmt);
            Sem_Statement_Or_Null (Get_Statement (Stmt));
         when N_Trigger =>
            declare
               Name : Node;
            begin
               Name := Sem_Name (Get_Event (Stmt));
               if Name /= Null_Node
                 and then Get_Expr_Type (Name) /= Event_Type
               then
                  Error_Msg_Sem (+Stmt, "only events can be triggered");
               end if;
            end;
         when N_Simple_Immediate_Assert =>
            Sem_Immediate_Assert (Stmt);
         when N_Label_Stmt =>
            Sem_Statements (Stmt);
         when N_Disable_Fork =>
            --  Nothing to do ?
            null;
         when N_Wait_Fork =>
            --  Nothing to do ?
            null;
         when N_Contribution =>
            Sem_Contribution (Stmt);
         when others =>
            Error_Kind ("sem_statement", Stmt);
      end case;
   end Sem_Statement;
end Verilog.Sem_Stmts;
