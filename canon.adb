--  Canonicalization pass
--  Copyright (C) 2002, 2003, 2004, 2005, 2008 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Errorout; use Errorout;
with Iirs_Utils; use Iirs_Utils;
with Types; use Types;
with Name_Table;
with Sem;
with Std_Names;
with Iir_Chains; use Iir_Chains;
with Flags; use Flags;
with PSL.Nodes;
with PSL.Rewrites;
with PSL.Build;

package body Canon is
   --  Canonicalize a list of declarations.  LIST can be null.
   --  PARENT must be the parent of the current statements chain for LIST,
   --  or NULL_IIR if LIST has no corresponding current statments.
   procedure Canon_Declarations (Top : Iir_Design_Unit;
                                 Decl_Parent : Iir;
                                 Parent : Iir);
   procedure Canon_Declaration (Top : Iir_Design_Unit;
                                Decl : Iir;
                                Parent : Iir;
                                Decl_Parent : Iir);

   --  Canonicalize an association list.
   --  If ASSOCIATION_LIST is not null, then it is re-ordored and returned.
   --  If ASSOCIATION_LIST is null then:
   --    if INTERFACE_LIST is null then returns null.
   --    if INTERFACE_LIST is not null, a default list is created.
   function Canon_Association_Chain
     (Interface_Chain: Iir; Association_Chain: Iir; Loc : Iir)
     return Iir;

   function Canon_Association_Chain_And_Actuals
     (Interface_Chain : Iir; Association_Chain : Iir; Loc : Iir)
     return Iir;

   --  Canonicalize block configuration CONF.
   --  TOP is used to added dependences to the design unit which CONF
   --  belongs to.
   procedure Canon_Block_Configuration (Top : Iir_Design_Unit;
                                        Conf : Iir_Block_Configuration);

   procedure Canon_Extract_Sensitivity_Aggregate
     (Aggr : Iir;
      Sensitivity_List : Iir_List;
      Is_Target : Boolean;
      Aggr_Type : Iir;
      Dim : Natural)
   is
      Assoc : Iir;
   begin
      Assoc := Get_Association_Choices_Chain (Aggr);
      if Get_Nbr_Elements (Get_Index_Subtype_List (Aggr_Type)) = Dim then
         while Assoc /= Null_Iir loop
            Canon_Extract_Sensitivity
              (Get_Associated (Assoc), Sensitivity_List, Is_Target);
            Assoc := Get_Chain (Assoc);
         end loop;
      else
         while Assoc /= Null_Iir loop
            Canon_Extract_Sensitivity_Aggregate
              (Get_Associated (Assoc), Sensitivity_List, Is_Target, Aggr_Type,
               Dim + 1);
            Assoc := Get_Chain (Assoc);
         end loop;
      end if;
   end Canon_Extract_Sensitivity_Aggregate;

   procedure Canon_Extract_Sensitivity
     (Expr: Iir; Sensitivity_List: Iir_List; Is_Target: Boolean := False)
   is
      El : Iir;
      List: Iir_List;
   begin
      if Get_Expr_Staticness (Expr) /= None then
         return;
      end if;

      case Get_Kind (Expr) is
         when Iir_Kind_Slice_Name =>
            if not Is_Target and then
              Get_Name_Staticness (Expr) >= Globally
            then
               if Is_Signal_Object (Expr) then
                  Add_Element (Sensitivity_List, Expr);
               end if;
            else
               declare
                  Suff : Iir;
               begin
                  Canon_Extract_Sensitivity
                    (Get_Prefix (Expr), Sensitivity_List, Is_Target);
                  Suff := Get_Suffix (Expr);
                  if Get_Kind (Suff) not in Iir_Kinds_Scalar_Type_Definition
                  then
                     Canon_Extract_Sensitivity
                       (Suff, Sensitivity_List, False);
                  end if;
               end;
            end if;

         when Iir_Kind_Selected_Element =>
            if not Is_Target and then
              Get_Name_Staticness (Expr) >= Globally
            then
               if Is_Signal_Object (Expr) then
                  Add_Element (Sensitivity_List, Expr);
               end if;
            else
               Canon_Extract_Sensitivity (Get_Prefix (Expr),
                                          Sensitivity_List,
                                          Is_Target);
            end if;

         when Iir_Kind_Indexed_Name =>
            if not Is_Target
              and then Get_Name_Staticness (Expr) >= Globally
            then
               if Is_Signal_Object (Expr) then
                  Add_Element (Sensitivity_List, Expr);
               end if;
            else
               Canon_Extract_Sensitivity (Get_Prefix (Expr),
                                          Sensitivity_List,
                                          Is_Target);
               List := Get_Index_List (Expr);
               for I in Natural loop
                  El := Get_Nth_Element (List, I);
                  exit when El = Null_Iir;
                  Canon_Extract_Sensitivity (El, Sensitivity_List, False);
               end loop;
            end if;

         when Iir_Kind_Function_Call =>
            El := Get_Parameter_Association_Chain (Expr);
            while El /= Null_Iir loop
               case Get_Kind (El) is
                  when Iir_Kind_Association_Element_By_Expression =>
                     Canon_Extract_Sensitivity
                       (Get_Actual (El), Sensitivity_List, False);
                  when Iir_Kind_Association_Element_Open =>
                     null;
                  when others =>
                     Error_Kind ("canon_extract_sensitivity(call)", El);
               end case;
               El := Get_Chain (El);
            end loop;

         when Iir_Kind_Qualified_Expression
           | Iir_Kind_Type_Conversion
           | Iir_Kind_Allocator_By_Expression =>
            Canon_Extract_Sensitivity
              (Get_Expression (Expr), Sensitivity_List, False);

         when Iir_Kind_Allocator_By_Subtype =>
            null;

         when Iir_Kinds_Monadic_Operator =>
            Canon_Extract_Sensitivity
              (Get_Operand (Expr), Sensitivity_List, False);
         when Iir_Kinds_Dyadic_Operator =>
            Canon_Extract_Sensitivity
              (Get_Left (Expr), Sensitivity_List, False);
            Canon_Extract_Sensitivity
              (Get_Right (Expr), Sensitivity_List, False);

         when Iir_Kind_Range_Expression =>
            Canon_Extract_Sensitivity
              (Get_Left_Limit (Expr), Sensitivity_List, False);
            Canon_Extract_Sensitivity
              (Get_Right_Limit (Expr), Sensitivity_List, False);

         when Iir_Kinds_Type_Attribute =>
            null;
         when Iir_Kind_Event_Attribute =>
            --  LRM 8.1
            --  An attribute name: [...]; otherwise, apply this rule to the
            --  prefix of the attribute name.
            Canon_Extract_Sensitivity
              (Get_Prefix (Expr), Sensitivity_List, False);


         when Iir_Kind_Last_Value_Attribute =>
            null;

         when Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute =>
            --  LRM 8.1
            --  A simple name that denotes a signal, add the longuest static
            --  prefix of the name to the sensitivity set;
            --
            --  An attribute name: if the designator denotes a signal
            --  attribute, add the longuest static prefix of the name of the
            --  implicit signal denoted by the attribute name to the
            --  sensitivity set; [...]
            if not Is_Target then
               Add_Element (Sensitivity_List, Expr);
            end if;

         when Iir_Kind_Object_Alias_Declaration =>
            Canon_Extract_Sensitivity
              (Get_Name (Expr), Sensitivity_List, Is_Target);

         when Iir_Kind_Constant_Declaration
           | Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_File_Declaration =>
            null;

         when Iir_Kind_Left_Array_Attribute
           | Iir_Kind_Right_Array_Attribute
           | Iir_Kind_Length_Array_Attribute
           | Iir_Kind_Low_Array_Attribute
           | Iir_Kind_High_Array_Attribute =>
            null;
            --Canon_Extract_Sensitivity
            --  (Get_Prefix (Expr), Sensitivity_List, Is_Target);

         when Iir_Kinds_Scalar_Type_Attribute =>
            Canon_Extract_Sensitivity
              (Get_Parameter (Expr), Sensitivity_List, Is_Target);

         when Iir_Kind_Aggregate =>
            declare
               Aggr_Type : Iir;
            begin
               Aggr_Type := Get_Base_Type (Get_Type (Expr));
               case Get_Kind (Aggr_Type) is
                  when Iir_Kind_Array_Type_Definition =>
                     Canon_Extract_Sensitivity_Aggregate
                       (Expr, Sensitivity_List, Is_Target, Aggr_Type, 1);
                  when Iir_Kind_Record_Type_Definition =>
                     El := Get_Association_Choices_Chain (Expr);
                     while El /= Null_Iir loop
                        Canon_Extract_Sensitivity
                          (Get_Associated (El), Sensitivity_List, Is_Target);
                        El := Get_Chain (El);
                     end loop;
                  when others =>
                     Error_Kind ("canon_extract_sensitivity(aggr)", Aggr_Type);
               end case;
            end;

         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            Canon_Extract_Sensitivity
              (Get_Named_Entity (Expr), Sensitivity_List, Is_Target);

         when others =>
            Error_Kind ("canon_extract_sensitivity", Expr);
      end case;
   end Canon_Extract_Sensitivity;

   procedure Canon_Extract_Sensitivity_If_Not_Null
     (Expr: Iir; Sensitivity_List: Iir_List; Is_Target: Boolean := False) is
   begin
      if Expr /= Null_Iir then
         Canon_Extract_Sensitivity (Expr, Sensitivity_List, Is_Target);
      end if;
   end Canon_Extract_Sensitivity_If_Not_Null;

   procedure Canon_Extract_Sequential_Statement_Chain_Sensitivity
     (Chain : Iir; List : Iir_List)
   is
      Stmt : Iir;
   begin
      Stmt := Chain;
      while Stmt /= Null_Iir loop
         case Get_Kind (Stmt) is
            when Iir_Kind_Assertion_Statement =>
               --  LRM08 11.3
               --  * For each assertion, report, next, exit or return
               --    statement, apply the rule of 10.2 to each expression
               --    in the statement, and construct the union of the
               --    resulting sets.
               Canon_Extract_Sensitivity
                 (Get_Assertion_Condition (Stmt), List);
               Canon_Extract_Sensitivity
                 (Get_Severity_Expression (Stmt), List);
               Canon_Extract_Sensitivity
                 (Get_Report_Expression (Stmt), List);
            when Iir_Kind_Report_Statement =>
               --  LRM08 11.3
               --  See assertion_statement case.
               Canon_Extract_Sensitivity
                 (Get_Severity_Expression (Stmt), List);
               Canon_Extract_Sensitivity
                 (Get_Report_Expression (Stmt), List);
            when Iir_Kind_Next_Statement
              | Iir_Kind_Exit_Statement =>
               --  LRM08 11.3
               --  See assertion_statement case.
               Canon_Extract_Sensitivity
                 (Get_Condition (Stmt), List);
            when Iir_Kind_Return_Statement =>
               --  LRM08 11.3
               --  See assertion_statement case.
               Canon_Extract_Sensitivity_If_Not_Null
                 (Get_Expression (Stmt), List);
            when Iir_Kind_Variable_Assignment_Statement =>
               --  LRM08 11.3
               --  * For each assignment statement, apply the rule of 10.2 to
               --    each expression occuring in the assignment, including any
               --    expressions occuring in the index names or slice names in
               --    the target, and construct the union of the resulting sets.
               Canon_Extract_Sensitivity (Get_Target (Stmt), List, True);
               Canon_Extract_Sensitivity (Get_Expression (Stmt), List, False);
            when Iir_Kind_Signal_Assignment_Statement =>
               --  LRM08 11.3
               --  See variable assignment statement case.
               Canon_Extract_Sensitivity (Get_Target (Stmt), List, True);
               Canon_Extract_Sensitivity_If_Not_Null
                 (Get_Reject_Time_Expression (Stmt), List);
               declare
                  We: Iir_Waveform_Element;
               begin
                  We := Get_Waveform_Chain (Stmt);
                  while We /= Null_Iir loop
                     Canon_Extract_Sensitivity (Get_We_Value (We), List);
                     We := Get_Chain (We);
                  end loop;
               end;
            when Iir_Kind_If_Statement =>
               --  LRM08 11.3
               --  * For each if statement, apply the rule of 10.2 to the
               --    condition and apply this rule recursively to each
               --    sequence of statements within the if statement, and
               --    construct the union of the resuling sets.
               declare
                  El1 : Iir := Stmt;
                  Cond : Iir;
               begin
                  loop
                     Cond := Get_Condition (El1);
                     if Cond /= Null_Iir then
                        Canon_Extract_Sensitivity (Cond, List);
                     end if;
                     Canon_Extract_Sequential_Statement_Chain_Sensitivity
                       (Get_Sequential_Statement_Chain (El1), List);
                     El1 := Get_Else_Clause (El1);
                     exit when El1 = Null_Iir;
                  end loop;
               end;
            when Iir_Kind_Case_Statement =>
               --  LRM08 11.3
               --  * For each case statement, apply the rule of 10.2 to the
               --    expression and apply this rule recursively to each
               --    sequence of statements within the case statement, and
               --    construct the union of the resulting sets.
               Canon_Extract_Sensitivity (Get_Expression (Stmt), List);
               declare
                  Choice: Iir;
               begin
                  Choice := Get_Case_Statement_Alternative_Chain (Stmt);
                  while Choice /= Null_Iir loop
                     Canon_Extract_Sequential_Statement_Chain_Sensitivity
                       (Get_Associated (Choice), List);
                     Choice := Get_Chain (Choice);
                  end loop;
               end;
            when Iir_Kind_While_Loop_Statement =>
               --  LRM08 11.3
               --  * For each loop statement, apply the rule of 10.2 to each
               --    expression in the iteration scheme, if present, and apply
               --    this rule recursively to the sequence of statements within
               --    the loop statement, and construct the union of the
               --    resulting sets.
               Canon_Extract_Sensitivity_If_Not_Null
                 (Get_Condition (Stmt), List);
               Canon_Extract_Sequential_Statement_Chain_Sensitivity
                 (Get_Sequential_Statement_Chain (Stmt), List);
            when Iir_Kind_For_Loop_Statement =>
               --  LRM08 11.3
               --  See loop statement case.
               declare
                  It : constant Iir := Get_Iterator_Scheme (Stmt);
                  It_Type : constant Iir := Get_Type (It);
                  Rng : constant Iir := Get_Range_Constraint (It_Type);
               begin
                  if Get_Kind (Rng) = Iir_Kind_Range_Expression then
                     Canon_Extract_Sensitivity (Rng, List);
                  end if;
               end;
               Canon_Extract_Sequential_Statement_Chain_Sensitivity
                 (Get_Sequential_Statement_Chain (Stmt), List);
            when Iir_Kind_Null_Statement =>
               --  LRM08 11.3
               --  ?
               null;
            when Iir_Kind_Procedure_Call_Statement =>
               --  LRM08 11.3
               --  * For each procedure call statement, apply the rule of 10.2
               --    to each actual designator (other than OPEN) associated
               --    with each formal parameter of mode IN or INOUT, and
               --    construct the union of the resulting sets.
               declare
                  Param : Iir;
               begin
                  Param := Get_Parameter_Association_Chain
                    (Get_Procedure_Call (Stmt));
                  while Param /= Null_Iir loop
                     if (Get_Kind (Param)
                           = Iir_Kind_Association_Element_By_Expression)
                       and then (Get_Mode (Get_Base_Name (Get_Formal (Param)))
                                   /= Iir_Out_Mode)
                     then
                        Canon_Extract_Sensitivity (Get_Actual (Param), List);
                     end if;
                     Param := Get_Chain (Param);
                  end loop;
               end;
            when others =>
               Error_Kind
                 ("canon_extract_sequential_statement_chain_sensitivity",
                  Stmt);
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Canon_Extract_Sequential_Statement_Chain_Sensitivity;

   procedure Canon_Extract_Sensitivity_From_Callees
     (Callees_List : Iir_List; Sensitivity_List : Iir_List)
   is
      Callee : Iir;
   begin
      --  LRM08 11.3
      --  Moreover, for each subprogram for which the process is a parent
      --  (see 4.3), the sensitivity list includes members of the set
      --  constructed by apply the preceding rule to the statements of the
      --  subprogram, but excluding the members that denote formal signal
      --  parameters or members of formal signal parameters of the subprogram
      --  or any of its parents.
      if Callees_List = Null_Iir_List then
         return;
      end if;
      for I in Natural loop
         Callee := Get_Nth_Element (Callees_List, I);
         exit when Callee = Null_Iir;
         if not Get_Seen_Flag (Callee) then
            Set_Seen_Flag (Callee, True);
            case Get_All_Sensitized_State (Callee) is
               when Read_Signal =>
                  Canon_Extract_Sequential_Statement_Chain_Sensitivity
                    (Get_Sequential_Statement_Chain
                       (Get_Subprogram_Body (Callee)),
                     Sensitivity_List);
                  Canon_Extract_Sensitivity_From_Callees
                    (Get_Callees_List (Callee), Sensitivity_List);
               when No_Signal =>
                  null;
               when Unknown | Invalid_Signal =>
                  raise Internal_Error;
            end case;
         end if;
      end loop;
   end Canon_Extract_Sensitivity_From_Callees;

   function Canon_Extract_Process_Sensitivity
     (Proc : Iir_Sensitized_Process_Statement)
     return Iir_List
   is
      Res : Iir_List;
   begin
      Res := Create_Iir_List;
      Canon_Extract_Sequential_Statement_Chain_Sensitivity
        (Get_Sequential_Statement_Chain (Proc), Res);
      Canon_Extract_Sensitivity_From_Callees
        (Get_Callees_List (Proc), Res);
      Set_Seen_Flag (Proc, True);
      Clear_Seen_Flag (Proc);
      return Res;
   end Canon_Extract_Process_Sensitivity;

--   function Make_Aggregate (Array_Type : Iir_Array_Type_Definition; El : Iir)
--      return Iir_Aggregate
--    is
--       Res : Iir_Aggregate;
--       Choice : Iir;
--    begin
--       Res := Create_Iir (Iir_Kind_Aggregate);
--       Location_Copy (Res, El);
--       Choice := Create_Iir (Iir_Kind_Association_Choice_By_None);
--       Set_Associated (Choice, El);
--       Append_Element (Get_Association_Choices_List (Res), Choice);

--       --  will call sem_aggregate
--       return Sem_Expr.Sem_Expression (Res, Array_Type);
--    end Make_Aggregate;

--    procedure Canon_Concatenation_Operator (Expr : Iir)
--    is
--       Array_Type : Iir_Array_Type_Definition;
--       El_Type : Iir;
--       Left, Right : Iir;
--       Func_List : Iir_Implicit_Functions_List;
--       Func : Iir_Implicit_Function_Declaration;
--    begin
--       Array_Type := Get_Type (Expr);
--       El_Type := Get_Base_Type (Get_Element_Subtype (Array_Type));
--       Left := Get_Left (Expr);
--       if Get_Type (Left) = El_Type then
--          Set_Left (Expr, Make_Aggregate (Array_Type, Left));
--       end if;
--       Right := Get_Right (Expr);
--       if Get_Type (Right) = El_Type then
--          Set_Right (Expr, Make_Aggregate (Array_Type, Right));
--       end if;

--       --  FIXME: must convert the implementation.
--       --  Use implicit declaration list from the array_type ?
--       Func_List := Get_Implicit_Functions_List
--         (Get_Type_Declarator (Array_Type));
--       for I in Natural loop
--          Func := Get_Nth_Element (Func_List, I);
--          if Get_Implicit_Definition (Func)
--            = Iir_Predefined_Array_Array_Concat
--          then
--             Set_Implementation (Expr, Func);
--             exit;
--          end if;
--       end loop;
--    end Canon_Concatenation_Operator;

   -- canon on expressions, mainly for function calls.
   procedure Canon_Expression (Expr: Iir)
   is
      El : Iir;
      List: Iir_List;
   begin
      if Expr = Null_Iir then
         return;
      end if;
      case Get_Kind (Expr) is
         when Iir_Kind_Range_Expression =>
            Canon_Expression (Get_Left_Limit (Expr));
            Canon_Expression (Get_Right_Limit (Expr));

         when Iir_Kind_Slice_Name =>
            declare
               Suffix : Iir;
            begin
               Suffix := Get_Suffix (Expr);
               if Get_Kind (Suffix) not in Iir_Kinds_Discrete_Type_Definition
               then
                  Canon_Expression (Suffix);
               end if;
               Canon_Expression (Get_Prefix (Expr));
            end;

         when Iir_Kind_Indexed_Name =>
            Canon_Expression (Get_Prefix (Expr));
            List := Get_Index_List (Expr);
            for I in Natural loop
               El := Get_Nth_Element (List, I);
               exit when El = Null_Iir;
               Canon_Expression (El);
            end loop;

--          when Iir_Kind_Selected_Name =>
--             -- Use this order to allow tail recursion optimisation.
--             Canon_Expression (Get_Suffix (Expr));
--             Canon_Expression (Get_Prefix (Expr));
         when Iir_Kind_Selected_Element =>
            Canon_Expression (Get_Prefix (Expr));
         when Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference =>
            Canon_Expression (Get_Prefix (Expr));

         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            Canon_Expression (Get_Named_Entity (Expr));

         when Iir_Kinds_Monadic_Operator =>
            Canon_Expression (Get_Operand (Expr));
         when Iir_Kinds_Dyadic_Operator =>
            Canon_Expression (Get_Left (Expr));
            Canon_Expression (Get_Right (Expr));
            if Get_Kind (Expr) = Iir_Kind_Concatenation_Operator
              and then Canon_Concatenation
              and then Get_Kind (Get_Implementation (Expr)) =
              Iir_Kind_Implicit_Function_Declaration
            then
               --Canon_Concatenation_Operator (Expr);
               raise Internal_Error;
            end if;

         when Iir_Kind_Function_Call =>
            declare
               Imp : Iir;
               Assoc_Chain : Iir;
            begin
               Imp := Get_Implementation (Expr);
               if Get_Kind (Imp) /= Iir_Kind_Implicit_Function_Declaration then
                  Assoc_Chain := Canon_Association_Chain_And_Actuals
                    (Get_Interface_Declaration_Chain (Imp),
                     Get_Parameter_Association_Chain (Expr),
                     Expr);
                  Set_Parameter_Association_Chain (Expr, Assoc_Chain);
               else
                  -- FIXME:
                  -- should canon concatenation.
                  null;
               end if;
            end;
         when Iir_Kind_Type_Conversion
           | Iir_Kind_Qualified_Expression =>
            Canon_Expression (Get_Expression (Expr));
         when Iir_Kind_Aggregate =>
            --  FIXME
            null;
         when Iir_Kind_Allocator_By_Expression =>
            Canon_Expression (Get_Expression (Expr));
         when Iir_Kind_Allocator_By_Subtype =>
            null;

         when Iir_Kinds_Literal
           | Iir_Kind_Simple_Aggregate
           | Iir_Kind_Unit_Declaration =>
            null;

         when Iir_Kinds_Array_Attribute =>
            -- No need to canon parameter, since it is a locally static
            -- expression.
            declare
               Prefix : Iir;
            begin
               Prefix := Get_Prefix (Expr);
               case Get_Kind (Prefix) is
                  when Iir_Kind_Type_Declaration
                    | Iir_Kind_Subtype_Declaration =>
                     null;
                  when others =>
                     Canon_Expression (Prefix);
               end case;
            end;

         when Iir_Kinds_Type_Attribute =>
            null;
         when Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Transaction_Attribute =>
            --  FIXME: add the default parameter ?
            Canon_Expression (Get_Prefix (Expr));
         when Iir_Kind_Event_Attribute
           | Iir_Kind_Last_Value_Attribute
           | Iir_Kind_Active_Attribute
           | Iir_Kind_Last_Event_Attribute
           | Iir_Kind_Last_Active_Attribute
           | Iir_Kind_Driving_Attribute
           | Iir_Kind_Driving_Value_Attribute =>
            Canon_Expression (Get_Prefix (Expr));

         when Iir_Kinds_Scalar_Type_Attribute
           | Iir_Kind_Image_Attribute
           | Iir_Kind_Value_Attribute =>
            Canon_Expression (Get_Parameter (Expr));

         when Iir_Kind_Simple_Name_Attribute
           | Iir_Kind_Path_Name_Attribute
           | Iir_Kind_Instance_Name_Attribute =>
            null;

         when Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_File_Interface_Declaration
           | Iir_Kind_Object_Alias_Declaration =>
            null;

         when Iir_Kind_Enumeration_Literal =>
            null;

         when Iir_Kind_Element_Declaration =>
            null;

         when Iir_Kind_Attribute_Value =>
            null;

         when others =>
            Error_Kind ("canon_expression", Expr);
            null;
      end case;
   end Canon_Expression;

   procedure Canon_Waveform_Chain
     (Chain : Iir_Waveform_Element; Sensitivity_List: Iir_List)
   is
      We: Iir_Waveform_Element;
   begin
      We := Chain;
      while We /= Null_Iir loop
         if Sensitivity_List /= Null_Iir_List then
            Canon_Extract_Sensitivity
              (Get_We_Value (We), Sensitivity_List, False);
         end if;
         if Canon_Flag_Expressions then
            Canon_Expression (Get_We_Value (We));
            if Get_Time (We) /= Null_Iir then
               Canon_Expression (Get_Time (We));
            end if;
         end if;
         We := Get_Chain (We);
      end loop;
   end Canon_Waveform_Chain;

   -- Names associations by position,
   -- reorder associations by name,
   -- create omitted association,
   function Canon_Association_Chain
     (Interface_Chain : Iir; Association_Chain : Iir; Loc : Iir)
     return Iir
   is
      -- The canon list of association.
      N_Chain, Last : Iir;
      Inter : Iir;
      Assoc_El, Prev_Assoc_El, Next_Assoc_El : Iir;
      Assoc_Chain : Iir;

      Found : Boolean;
   begin
      --  No argument, so return now.
      if Interface_Chain = Null_Iir then
         if Association_Chain /= Null_Iir then
            raise Internal_Error;
         end if;
         return Null_Iir;
      end if;

      Sub_Chain_Init (N_Chain, Last);
      Assoc_Chain := Association_Chain;

      -- Reorder the list of association in the interface order.
      -- Add missing associations.
      Inter := Interface_Chain;
      while Inter /= Null_Iir loop
         --  Search associations with INTERFACE.
         Found := False;
         Assoc_El := Assoc_Chain;
         Prev_Assoc_El := Null_Iir;
         while Assoc_El /= Null_Iir loop
            Next_Assoc_El := Get_Chain (Assoc_El);
            if Get_Formal (Assoc_El) = Null_Iir then
               Set_Formal (Assoc_El, Inter);
            end if;
            if Get_Associated_Formal (Assoc_El) = Inter then

               --  Remove ASSOC_EL from ASSOC_CHAIN
               if Prev_Assoc_El /= Null_Iir then
                  Set_Chain (Prev_Assoc_El, Next_Assoc_El);
               else
                  Assoc_Chain := Next_Assoc_El;
               end if;

               --  Append ASSOC_EL in N_CHAIN.
               Set_Chain (Assoc_El, Null_Iir);
               Sub_Chain_Append (N_Chain, Last, Assoc_El);

               case Get_Kind (Assoc_El) is
                  when Iir_Kind_Association_Element_Open =>
                     goto Done;
                  when Iir_Kind_Association_Element_By_Expression =>
                     if Get_Whole_Association_Flag (Assoc_El) then
                        goto Done;
                     end if;
                  when Iir_Kind_Association_Element_By_Individual =>
                     Found := True;
                  when others =>
                     Error_Kind ("canon_association_list", Assoc_El);
               end case;
            elsif Found then
               --  No more associations.
               goto Done;
            else
               Prev_Assoc_El := Assoc_El;
            end if;
            Assoc_El := Next_Assoc_El;
         end loop;
         if Found then
            goto Done;
         end if;

         -- No association, use default expr.
         Assoc_El := Create_Iir (Iir_Kind_Association_Element_Open);
         Set_Artificial_Flag (Assoc_El, True);
         Location_Copy (Assoc_El, Loc);
         Set_Formal (Assoc_El, Inter);
         Sub_Chain_Append (N_Chain, Last, Assoc_El);

         << Done >> null;
         Inter := Get_Chain (Inter);
      end loop;
      pragma Assert (Assoc_Chain = Null_Iir);

      return N_Chain;
   end Canon_Association_Chain;

   procedure Canon_Association_Chain_Actuals (Association_Chain : Iir)
   is
      Assoc_El : Iir;
   begin
      --  Canon actuals.
      Assoc_El := Association_Chain;
      while Assoc_El /= Null_Iir loop
         if Get_Kind (Assoc_El) = Iir_Kind_Association_Element_By_Expression
         then
            Canon_Expression (Get_Actual (Assoc_El));
         end if;
         Assoc_El := Get_Chain (Assoc_El);
      end loop;
   end Canon_Association_Chain_Actuals;

   function Canon_Association_Chain_And_Actuals
     (Interface_Chain : Iir; Association_Chain : Iir; Loc : Iir)
     return Iir
   is
      Res : Iir;
   begin
      Res := Canon_Association_Chain
        (Interface_Chain, Association_Chain, Loc);
      Canon_Association_Chain_Actuals (Res);
      return Res;
   end Canon_Association_Chain_And_Actuals;

   function Canon_Subprogram_Call (Call : Iir) return Iir
   is
      Imp : Iir;
      Assoc_Chain : Iir;
      Inter_Chain : Iir;
   begin
      Imp := Get_Implementation (Call);
      Inter_Chain := Get_Interface_Declaration_Chain (Imp);
      Assoc_Chain := Get_Parameter_Association_Chain (Call);
      Assoc_Chain := Canon_Association_Chain (Inter_Chain, Assoc_Chain, Call);
      Set_Parameter_Association_Chain (Call, Assoc_Chain);
      return Assoc_Chain;
   end Canon_Subprogram_Call;

   --  Create a default association list for INTERFACE_LIST.
   --  The default is a list of interfaces associated with open.
   function Canon_Default_Association_Chain (Interface_Chain : Iir)
     return Iir
   is
      Res : Iir;
      Last : Iir;
      Assoc, El : Iir;
   begin
      El := Interface_Chain;
      Sub_Chain_Init (Res, Last);
      while El /= Null_Iir loop
         Assoc := Create_Iir (Iir_Kind_Association_Element_Open);
         Set_Artificial_Flag (Assoc, True);
         Set_Formal (Assoc, El);
         Location_Copy (Assoc, El);
         Sub_Chain_Append (Res, Last, Assoc);
         El := Get_Chain (El);
      end loop;
      return Res;
   end Canon_Default_Association_Chain;

--    function Canon_Default_Map_Association_List
--      (Formal_List, Actual_List : Iir_List; Loc : Location_Type)
--      return Iir_Association_List
--    is
--       Res : Iir_Association_List;
--       Formal, Actual : Iir;
--       Assoc : Iir;
--       Nbr_Assoc : Natural;
--    begin
--       --  formal is the entity port/generic.
--       if Formal_List = Null_Iir_List then
--          if Actual_List /= Null_Iir_List then
--             raise Internal_Error;
--          end if;
--          return Null_Iir_List;
--       end if;

--       Res := Create_Iir (Iir_Kind_Association_List);
--       Set_Location (Res, Loc);
--       Nbr_Assoc := 0;
--       for I in Natural loop
--          Formal := Get_Nth_Element (Formal_List, I);
--          exit when Formal = Null_Iir;
--          Actual := Find_Name_In_List (Actual_List, Get_Identifier (Formal));
--          if Actual /= Null_Iir then
--            Assoc := Create_Iir (Iir_Kind_Association_Element_By_Expression);
--             Set_Whole_Association_Flag (Assoc, True);
--             Set_Actual (Assoc, Actual);
--             Nbr_Assoc := Nbr_Assoc + 1;
--          else
--             Assoc := Create_Iir (Iir_Kind_Association_Element_Open);
--          end if;
--          Set_Location (Assoc, Loc);
--          Set_Formal (Assoc, Formal);
--          Set_Associated_Formal (Assoc, Formal);
--          Append_Element (Res, Assoc);
--       end loop;
--       if Nbr_Assoc /= Get_Nbr_Elements (Actual_List) then
--          --  There is non-associated actuals.
--          raise Internal_Error;
--       end if;
--       return Res;
--    end Canon_Default_Map_Association_List;

   --  Inner loop if any; used to canonicalize exit/next statement.
   Cur_Loop : Iir;

   procedure Canon_Procedure_Call (Call : Iir_Procedure_Call)
   is
      Assoc_Chain : Iir;
   begin
      Assoc_Chain := Canon_Association_Chain_And_Actuals
        (Get_Interface_Declaration_Chain (Get_Implementation (Call)),
         Get_Parameter_Association_Chain (Call),
         Call);
      Set_Parameter_Association_Chain (Call, Assoc_Chain);
   end Canon_Procedure_Call;

   procedure Canon_Sequential_Stmts (First : Iir)
   is
      Stmt: Iir;
      Expr: Iir;
      Prev_Loop : Iir;
      Label : Iir;
   begin
      Stmt := First;
      while Stmt /= Null_Iir loop
         case Get_Kind (Stmt) is
            when Iir_Kind_If_Statement =>
               declare
                  Cond: Iir;
                  Clause: Iir := Stmt;
               begin
                  while Clause /= Null_Iir loop
                     Cond := Get_Condition (Clause);
                     if Cond /= Null_Iir then
                        Canon_Expression (Cond);
                     end if;
                     Canon_Sequential_Stmts
                       (Get_Sequential_Statement_Chain (Clause));
                     Clause := Get_Else_Clause (Clause);
                  end loop;
               end;

            when Iir_Kind_Signal_Assignment_Statement =>
               Canon_Expression (Get_Target (Stmt));
               Canon_Waveform_Chain (Get_Waveform_Chain (Stmt), Null_Iir_List);

            when Iir_Kind_Variable_Assignment_Statement =>
               Canon_Expression (Get_Target (Stmt));
               Canon_Expression (Get_Expression (Stmt));

            when Iir_Kind_Wait_Statement =>
               declare
                  Expr: Iir;
                  List: Iir_List;
               begin
                  Expr := Get_Timeout_Clause (Stmt);
                  if Expr /= Null_Iir then
                     Canon_Expression (Expr);
                  end if;
                  Expr := Get_Condition_Clause (Stmt);
                  if Expr /= Null_Iir then
                     Canon_Expression (Expr);
                  end if;
                  List := Get_Sensitivity_List (Stmt);
                  if List = Null_Iir_List and then Expr /= Null_Iir then
                     List := Create_Iir_List;
                     Canon_Extract_Sensitivity (Expr, List, False);
                     Set_Sensitivity_List (Stmt, List);
                  end if;
               end;

            when Iir_Kind_Case_Statement =>
               Canon_Expression (Get_Expression (Stmt));
               declare
                  Choice: Iir;
               begin
                  Choice := Get_Case_Statement_Alternative_Chain (Stmt);
                  while Choice /= Null_Iir loop
                     -- FIXME: canon choice expr.
                     Canon_Sequential_Stmts (Get_Associated (Choice));
                     Choice := Get_Chain (Choice);
                  end loop;
               end;

            when Iir_Kind_Assertion_Statement
              | Iir_Kind_Report_Statement =>
               if Get_Kind (Stmt) = Iir_Kind_Assertion_Statement then
                  Canon_Expression (Get_Assertion_Condition (Stmt));
               end if;
               Expr := Get_Report_Expression (Stmt);
               if Expr /= Null_Iir then
                  Canon_Expression (Expr);
               end if;
               Expr := Get_Severity_Expression (Stmt);
               if Expr /= Null_Iir then
                  Canon_Expression (Expr);
               end if;

            when Iir_Kind_For_Loop_Statement =>
               -- FIXME: decl.
               Prev_Loop := Cur_Loop;
               Cur_Loop := Stmt;
               Canon_Sequential_Stmts (Get_Sequential_Statement_Chain (Stmt));
               Cur_Loop := Prev_Loop;

            when Iir_Kind_While_Loop_Statement =>
               Expr := Get_Condition (Stmt);
               if Expr /= Null_Iir then
                  Canon_Expression (Expr);
               end if;
               Prev_Loop := Cur_Loop;
               Cur_Loop := Stmt;
               Canon_Sequential_Stmts (Get_Sequential_Statement_Chain (Stmt));
               Cur_Loop := Prev_Loop;

            when Iir_Kind_Next_Statement
              | Iir_Kind_Exit_Statement =>
               Expr := Get_Condition (Stmt);
               if Expr /= Null_Iir then
                  Canon_Expression (Expr);
               end if;
               Label := Get_Loop (Stmt);
               if Label = Null_Iir then
                  Set_Loop (Stmt, Cur_Loop);
               end if;

            when Iir_Kind_Procedure_Call_Statement =>
               Canon_Procedure_Call (Get_Procedure_Call (Stmt));

            when Iir_Kind_Null_Statement =>
               null;

            when Iir_Kind_Return_Statement =>
               Canon_Expression (Get_Expression (Stmt));

            when others =>
               Error_Kind ("canon_sequential_stmts", Stmt);
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Canon_Sequential_Stmts;

   -- Create a statement transform from concurrent_signal_assignment
   -- statement STMT (either selected or conditional).
   -- waveform transformation is not done.
   -- PROC is the process created.
   -- PARENT is the place where signal assignment must be placed.  This may
   --  be PROC, or an 'if' statement if the assignment is guarded.
   -- See LRM93 9.5
   procedure Canon_Concurrent_Signal_Assignment
     (Stmt: Iir;
      Proc: out Iir_Sensitized_Process_Statement;
      Chain : out Iir)
   is
      If_Stmt: Iir;
      Sensitivity_List : Iir_List;
   begin
      Proc := Create_Iir (Iir_Kind_Sensitized_Process_Statement);
      Location_Copy (Proc, Stmt);
      Set_Parent (Proc, Get_Parent (Stmt));
      Sensitivity_List := Create_Iir_List;
      Set_Sensitivity_List (Proc, Sensitivity_List);

      --  LRM93 9.5
      --  1. If a label appears on the concurrent signal assignment, then the
      --     same label appears on the process statement.
      Set_Label (Proc, Get_Label (Stmt));

      --  LRM93 9.5
      --  2.  The equivalent process statement is a postponed process if and
      --      only if the current signal assignment statement includes the
      --      reserved word POSTPONED.
      Set_Postponed_Flag (Proc, Get_Postponed_Flag (Proc));

      Canon_Extract_Sensitivity (Get_Target (Stmt), Sensitivity_List, True);

      if Canon_Flag_Expressions then
         Canon_Expression (Get_Target (Stmt));
      end if;

      if Get_Guard (Stmt) /= Null_Iir then
         -- LRM93 9.1
         -- If the option guarded appears in the concurrent signal assignment
         -- statement, then the concurrent signal assignment is called a
         -- guarded assignment.
         -- If the concurrent signal assignement statement is a guarded
         -- assignment and the target of the concurrent signal assignment is
         -- a guarded target, then the statement transform is as follow:
         --   if GUARD then signal_transform else disconnect_statements end if;
         -- Otherwise, if the concurrent signal assignment statement is a
         -- guarded assignement, but the target if the concurrent signal
         -- assignment is not a guarded target, the then statement transform
         -- is as follows:
         --  if GUARD then signal_transform end if;
         If_Stmt := Create_Iir (Iir_Kind_If_Statement);
         Set_Sequential_Statement_Chain (Proc, If_Stmt);
         Location_Copy (If_Stmt, Stmt);
         Canon_Extract_Sensitivity (Get_Guard (Stmt), Sensitivity_List, False);
         Set_Condition (If_Stmt, Get_Guard (Stmt));
         Chain := If_Stmt;

         declare
            Target : Iir;
            Else_Clause : Iir_Elsif;
            Dis_Stmt : Iir_Signal_Assignment_Statement;
         begin
            Target := Get_Target (Stmt);
            if Get_Guarded_Target_State (Stmt) = True then
               --  The target is a guarded target.
               --  create the disconnection statement.
               Else_Clause := Create_Iir (Iir_Kind_Elsif);
               Location_Copy (Else_Clause, Stmt);
               Set_Else_Clause (If_Stmt, Else_Clause);
               Dis_Stmt := Create_Iir (Iir_Kind_Signal_Assignment_Statement);
               Location_Copy (Dis_Stmt, Stmt);
               Set_Target (Dis_Stmt, Target);
               Set_Sequential_Statement_Chain (Else_Clause, Dis_Stmt);
               --  XX
               Set_Waveform_Chain (Dis_Stmt, Null_Iir);
            end if;
         end;
      else
         -- LRM93 9.1
         -- Finally, if the concurrent signal assignment statement is not a
         -- guarded assignment, and the traget of the concurrent signal
         -- assignment is not a guarded target, then the statement transform
         -- is as follows:
         --    signal_transform
         Chain := Proc;
      end if;
   end Canon_Concurrent_Signal_Assignment;

   function Canon_Concurrent_Procedure_Call (El : Iir)
     return Iir_Sensitized_Process_Statement
   is
      Proc : Iir_Sensitized_Process_Statement;
      Call_Stmt : Iir_Procedure_Call_Statement;
      Wait_Stmt : Iir_Wait_Statement;
      Call : Iir_Procedure_Call;
      Assoc_Chain : Iir;
      Assoc : Iir;
      Imp : Iir;
      Inter : Iir;
      Sensitivity_List : Iir_List;
      Is_Sensitized : Boolean;
   begin
      Call := Get_Procedure_Call (El);
      Imp := Get_Implementation (Call);

      --  Optimization: the process is a sensitized process only if the
      --  procedure is known not to have wait statement.
      Is_Sensitized := Get_Wait_State (Imp) = False;

      --  LRM93 9.3
      --  The equivalent process statement has also no sensitivity list, an
      --  empty declarative part, and a statement part that consists of a
      --  procedure call statement followed by a wait statement.
      if Is_Sensitized then
         Proc := Create_Iir (Iir_Kind_Sensitized_Process_Statement);
      else
         Proc := Create_Iir (Iir_Kind_Process_Statement);
      end if;
      Location_Copy (Proc, El);
      Set_Parent (Proc, Get_Parent (El));

      --  LRM93 9.3
      --  The equivalent process statement has a label if and only if the
      --  concurrent procedure call statement has a label; if the equivalent
      --  process statement has a label, it is the same as that of the
      --  concurrent procedure call statement.
      Set_Label (Proc, Get_Label (El));

      --  LRM93 9.3
      --  The equivalent process statement is a postponed process if and only
      --  if the concurrent procedure call statement includes the reserved
      --  word POSTPONED.
      Set_Postponed_Flag (Proc, Get_Postponed_Flag (El));

      Set_Attribute_Value_Chain (Proc, Get_Attribute_Value_Chain (El));

      Call_Stmt := Create_Iir (Iir_Kind_Procedure_Call_Statement);
      Set_Sequential_Statement_Chain (Proc, Call_Stmt);
      Location_Copy (Call_Stmt, El);
      Set_Procedure_Call (Call_Stmt, Call);
      Assoc_Chain := Canon_Association_Chain
        (Get_Interface_Declaration_Chain (Imp),
         Get_Parameter_Association_Chain (Call),
         Call);
      Set_Parameter_Association_Chain (Call, Assoc_Chain);
      Assoc := Assoc_Chain;

      --  LRM93 9.3
      --  If there exists a name that denotes a signal in the actual part of
      --  any association element in the concurrent procedure call statement,
      --  and that actual is associated with a formal parameter of mode IN or
      --  INOUT, then the equivalent process statement includes a final wait
      --  statement with a sensitivity clause that is constructed by taking
      --  the union of the sets constructed by applying th rule of Section 8.1
      --  to each actual part associated with a formal parameter.
      Sensitivity_List := Create_Iir_List;
      while Assoc /= Null_Iir loop
         case Get_Kind (Assoc) is
            when Iir_Kind_Association_Element_By_Expression =>
               Inter := Get_Associated_Formal (Assoc);
               if Get_Mode (Inter) in Iir_In_Modes then
                  Canon_Extract_Sensitivity
                    (Get_Actual (Assoc), Sensitivity_List, False);
               end if;
            when Iir_Kind_Association_Element_Open
              | Iir_Kind_Association_Element_By_Individual =>
               null;
            when others =>
               raise Internal_Error;
         end case;
         Assoc := Get_Chain (Assoc);
      end loop;
      if Get_Nbr_Elements (Sensitivity_List) = 0 then
         Destroy_Iir_List (Sensitivity_List);
      end if;
      if Is_Sensitized then
         Set_Sensitivity_List (Proc, Sensitivity_List);
      else
         Wait_Stmt := Create_Iir (Iir_Kind_Wait_Statement);
         Location_Copy (Wait_Stmt, El);
         Set_Parent (Wait_Stmt, Proc);
         Set_Sensitivity_List (Wait_Stmt, Sensitivity_List);
         Set_Chain (Call_Stmt, Wait_Stmt);
      end if;
      return Proc;
   end Canon_Concurrent_Procedure_Call;

   function Canon_Wave_Transform
     (Orig_Stmt : Iir; Waveform_Chain : Iir_Waveform_Element; Proc : Iir)
     return Iir
   is
      Stmt : Iir;
   begin
      if Waveform_Chain = Null_Iir then
         --  LRM 9.5.1 Conditionnal Signal Assignment
         --  If the waveform is of the form:
         --    UNAFFECTED
         --  then the wave transform in the corresponding process statement
         --  is of the form:
         --    NULL;
         --  In this example, the final NULL causes the driver to be unchanged,
         --  rather than disconnected.
         --  (This is the null statement not a null waveform element).
         Stmt := Create_Iir (Iir_Kind_Null_Statement);
      else
         --  LRM 9.5.1 Conditionnal Signal Assignment
         --  If the waveform is of the form:
         --    waveform_element1, waveform_element1, ..., waveform_elementN
         --  then the wave transform in the corresponding process statement is
         --  of the form:
         --    target <= [ delay_mechanism ] waveform_element1,
         --       waveform_element2, ..., waveform_elementN;
         Stmt := Create_Iir (Iir_Kind_Signal_Assignment_Statement);
         Set_Target (Stmt, Get_Target (Orig_Stmt));
         Canon_Waveform_Chain (Waveform_Chain, Get_Sensitivity_List (Proc));
         Set_Waveform_Chain (Stmt, Waveform_Chain);
         Set_Delay_Mechanism (Stmt, Get_Delay_Mechanism (Orig_Stmt));
         Set_Reject_Time_Expression
           (Stmt, Get_Reject_Time_Expression (Orig_Stmt));
      end if;
      Location_Copy (Stmt, Orig_Stmt);
      return Stmt;
   end Canon_Wave_Transform;

   --  Create signal_transform for a conditional concurrent signal assignment.
   procedure Canon_Conditional_Concurrent_Signal_Assigment
     (Conc_Stmt : Iir; Proc : Iir; Parent : Iir)
   is
      Expr : Iir;
      Stmt : Iir;
      Res1 : Iir;
      Last_Res : Iir;
      Wf : Iir;
      Cond_Wf : Iir_Conditional_Waveform;
      Cond_Wf_Chain : Iir_Conditional_Waveform;
   begin
      Cond_Wf_Chain := Get_Conditional_Waveform_Chain (Conc_Stmt);
      Stmt := Null_Iir;
      Cond_Wf := Cond_Wf_Chain;
      Last_Res := Null_Iir;
      while Cond_Wf /= Null_Iir loop
         Expr := Get_Condition (Cond_Wf);
         Wf := Canon_Wave_Transform
           (Conc_Stmt, Get_Waveform_Chain (Cond_Wf), Proc);
         if Expr = Null_Iir and Cond_Wf = Cond_Wf_Chain then
            Res1 := Wf;
         else
            if Expr /= Null_Iir then
               if Canon_Flag_Expressions then
                  Canon_Expression (Expr);
               end if;
               Canon_Extract_Sensitivity
                 (Expr, Get_Sensitivity_List (Proc), False);
            end if;
            if Stmt = Null_Iir then
               Res1 := Create_Iir (Iir_Kind_If_Statement);
            else
               Res1 := Create_Iir (Iir_Kind_Elsif);
            end if;
            Location_Copy (Res1, Cond_Wf);
            Set_Condition (Res1, Expr);
            Set_Sequential_Statement_Chain (Res1, Wf);
         end if;
         if Stmt = Null_Iir then
            Stmt := Res1;
         else
            Set_Else_Clause (Last_Res, Res1);
         end if;
         Last_Res := Res1;
         Cond_Wf := Get_Chain (Cond_Wf);
      end loop;
      Set_Sequential_Statement_Chain (Parent, Stmt);
   end Canon_Conditional_Concurrent_Signal_Assigment;

   procedure Canon_Selected_Concurrent_Signal_Assignment
     (Conc_Stmt : Iir; Proc : Iir; Parent : Iir)
   is
      Selected_Waveform : Iir;
      Case_Stmt: Iir_Case_Statement;
      Expr : Iir;
      Stmt : Iir;
      Assoc : Iir;
   begin
      Case_Stmt := Create_Iir (Iir_Kind_Case_Statement);
      Set_Sequential_Statement_Chain (Parent, Case_Stmt);
      Location_Copy (Case_Stmt, Conc_Stmt);
      Expr := Get_Expression (Conc_Stmt);
      if Canon_Flag_Expressions then
         Canon_Expression (Expr);
      end if;
      Set_Expression (Case_Stmt, Expr);
      Canon_Extract_Sensitivity
        (Expr, Get_Sensitivity_List (Proc), False);

      Selected_Waveform := Get_Selected_Waveform_Chain (Conc_Stmt);
      Set_Case_Statement_Alternative_Chain (Case_Stmt, Selected_Waveform);
      while Selected_Waveform /= Null_Iir loop
         Assoc := Get_Associated (Selected_Waveform);
         if Assoc /= Null_Iir then
            Stmt := Canon_Wave_Transform (Conc_Stmt, Assoc, Proc);
            Set_Associated (Selected_Waveform, Stmt);
         end if;
         Selected_Waveform := Get_Chain (Selected_Waveform);
      end loop;
   end Canon_Selected_Concurrent_Signal_Assignment;

   procedure Canon_Concurrent_Stmts (Top : Iir_Design_Unit; Parent : Iir)
   is
      --  Current element in the chain of concurrent statements.
      El: Iir;
      --  Previous element or NULL_IIR if EL is the first element.
      --  This is used to make Replace_Stmt efficient.
      Prev_El : Iir;

      --  Replace in the chain EL by N_STMT.
      procedure Replace_Stmt (N_Stmt : Iir) is
      begin
         if Prev_El = Null_Iir then
            Set_Concurrent_Statement_Chain (Parent, N_Stmt);
         else
            Set_Chain (Prev_El, N_Stmt);
         end if;
         Set_Chain (N_Stmt, Get_Chain (El));
      end Replace_Stmt;

      Proc: Iir;
      Stmt: Iir;
      Sub_Chain : Iir;
      Expr: Iir;
      Proc_Num : Natural := 0;
      Sensitivity_List : Iir_List;
   begin
      Prev_El := Null_Iir;
      El := Get_Concurrent_Statement_Chain (Parent);
      while El /= Null_Iir loop
         --  Add a label if required.
         if Canon_Flag_Add_Labels then
            case Get_Kind (El) is
               when Iir_Kind_Psl_Declaration =>
                  null;
               when others =>
                  if Get_Label (El) = Null_Identifier then
                     declare
                        Str : String := Natural'Image (Proc_Num);
                     begin
                        --  Note: the label starts with a capitalized letter,
                        --  to avoid any clash with user's identifiers.
                        Str (1) := 'P';
                        Set_Label (El, Name_Table.Get_Identifier (Str));
                     end;
                     Proc_Num := Proc_Num + 1;
                  end if;
            end case;
         end if;

         case Get_Kind (El) is
            when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
               Canon_Concurrent_Signal_Assignment (El, Proc, Sub_Chain);

               Canon_Conditional_Concurrent_Signal_Assigment
                 (El, Proc, Sub_Chain);

               Replace_Stmt (Proc);
               Free_Iir (El);
               El := Proc;

            when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
               Canon_Concurrent_Signal_Assignment (El, Proc, Sub_Chain);

               Canon_Selected_Concurrent_Signal_Assignment
                 (El, Proc, Sub_Chain);

               Replace_Stmt (Proc);
               Free_Iir (El);
               El := Proc;

            when Iir_Kind_Concurrent_Assertion_Statement =>
               -- Create a new entry.
               Proc := Create_Iir (Iir_Kind_Sensitized_Process_Statement);
               Location_Copy (Proc, El);
               Set_Parent (Proc, Get_Parent (El));

               --  LRM93 9.4
               --  The equivalent process statement has a label if and only if
               --  the current assertion statement has a label; if the
               --  equivalent process statement has a label; it is the same
               --  as that of the concurrent assertion statement.
               Set_Label (Proc, Get_Label (El));

               --  LRM93 9.4
               --  The equivalent process statement is a postponed process if
               --  and only if the current assertion statement includes the
               --  reserved word POSTPONED.
               Set_Postponed_Flag (Proc, Get_Postponed_Flag (El));

               Stmt := Create_Iir (Iir_Kind_Assertion_Statement);
               Set_Sequential_Statement_Chain (Proc, Stmt);
               Location_Copy (Stmt, El);
               Sensitivity_List := Create_Iir_List;
               Set_Sensitivity_List (Proc, Sensitivity_List);

               -- Expand the expression, fill the sensitivity list,
               Canon_Extract_Sensitivity
                 (Get_Assertion_Condition (El), Sensitivity_List, False);
               if Canon_Flag_Expressions then
                  Canon_Expression (Get_Assertion_Condition (El));
               end if;
               Set_Assertion_Condition
                 (Stmt, Get_Assertion_Condition (El));

               Expr := Get_Report_Expression (El);
               if Canon_Flag_Expressions and Expr /= Null_Iir then
                  Canon_Expression (Expr);
               end if;
               Set_Report_Expression (Stmt, Expr);

               Expr := Get_Severity_Expression (El);
               if Canon_Flag_Expressions and Expr /= Null_Iir then
                  Canon_Expression (Expr);
               end if;
               Set_Severity_Expression (Stmt, Expr);

               Replace_Stmt (Proc);
               El := Proc;

            when Iir_Kind_Concurrent_Procedure_Call_Statement =>
               Proc := Canon_Concurrent_Procedure_Call (El);
               Replace_Stmt (Proc);
               El := Proc;

            when Iir_Kind_Sensitized_Process_Statement
              | Iir_Kind_Process_Statement =>
               Canon_Declarations (Top, El, Null_Iir);
               if Canon_Flag_Sequentials_Stmts then
                  Canon_Sequential_Stmts (Get_Sequential_Statement_Chain (El));
               end if;

            when Iir_Kind_Component_Instantiation_Statement =>
               declare
                  Inst : Iir;
                  Assoc_Chain : Iir;
               begin
                  Inst := Get_Instantiated_Unit (El);
                  Inst := Get_Entity_From_Entity_Aspect (Inst);
                  Assoc_Chain := Canon_Association_Chain
                    (Get_Generic_Chain (Inst),
                     Get_Generic_Map_Aspect_Chain (El),
                     El);
                  Set_Generic_Map_Aspect_Chain (El, Assoc_Chain);

                  Assoc_Chain := Canon_Association_Chain
                    (Get_Port_Chain (Inst),
                     Get_Port_Map_Aspect_Chain (El),
                     El);
                  Set_Port_Map_Aspect_Chain (El, Assoc_Chain);
               end;

            when Iir_Kind_Block_Statement =>
               declare
                  Header : Iir_Block_Header;
                  Chain : Iir;
                  Guard : Iir_Guard_Signal_Declaration;
               begin
                  Guard := Get_Guard_Decl (El);
                  if Guard /= Null_Iir then
                     Expr := Get_Guard_Expression (Guard);
                     Set_Guard_Sensitivity_List (Guard, Create_Iir_List);
                     Canon_Extract_Sensitivity
                       (Expr, Get_Guard_Sensitivity_List (Guard), False);
                     if Canon_Flag_Expressions then
                        Canon_Expression (Expr);
                     end if;
                  end if;
                  Header := Get_Block_Header (El);
                  if Header /= Null_Iir then
                     --  Generics.
                     Chain := Get_Generic_Map_Aspect_Chain (Header);
                     if Chain /= Null_Iir then
                        Chain := Canon_Association_Chain
                          (Get_Generic_Chain (Header), Chain, Chain);
                     else
                        Chain := Canon_Default_Association_Chain
                          (Get_Generic_Chain (Header));
                     end if;
                     Set_Generic_Map_Aspect_Chain (Header, Chain);

                     --  Ports.
                     Chain := Get_Port_Map_Aspect_Chain (Header);
                     if Chain /= Null_Iir then
                        Chain := Canon_Association_Chain
                          (Get_Port_Chain (Header), Chain, Chain);
                     else
                        Chain := Canon_Default_Association_Chain
                          (Get_Port_Chain (Header));
                     end if;
                     Set_Port_Map_Aspect_Chain (Header, Chain);
                  end if;
                  Canon_Declarations (Top, El, El);
                  Canon_Concurrent_Stmts (Top, El);
               end;

            when Iir_Kind_Generate_Statement =>
               declare
                  Scheme : Iir;
               begin
                  Scheme := Get_Generation_Scheme (El);
                  if Get_Kind (Scheme) = Iir_Kind_Iterator_Declaration then
                     Canon_Declaration (Top, Scheme, Null_Iir, Null_Iir);
                  elsif Canon_Flag_Expressions then
                     Canon_Expression (Scheme);
                  end if;
                  Canon_Declarations (Top, El, El);
                  Canon_Concurrent_Stmts (Top, El);
               end;

            when Iir_Kind_Psl_Assert_Statement =>
               declare
                  use PSL.Nodes;
                  Prop : PSL_Node;
                  Fa : PSL_NFA;
               begin
                  Prop := Get_Psl_Property (El);
                  Prop := PSL.Rewrites.Rewrite_Property (Prop);
                  Set_Psl_Property (El, Prop);
                  --  Generate the NFA.
                  Fa := PSL.Build.Build_FA (Prop);
                  Set_PSL_NFA (El, Fa);
               end;

            when Iir_Kind_Psl_Default_Clock =>
               null;
            when Iir_Kind_Psl_Declaration =>
               declare
                  use PSL.Nodes;
                  Decl : PSL_Node;
                  Prop : PSL_Node;
                  Fa : PSL_NFA;
               begin
                  Decl := Get_Psl_Declaration (El);
                  case Get_Kind (Decl) is
                     when N_Property_Declaration =>
                        Prop := Get_Property (Decl);
                        Prop := PSL.Rewrites.Rewrite_Property (Prop);
                        Set_Property (Decl, Prop);
                        if Get_Parameter_List (Decl) = Null_Node then
                           --  Generate the NFA.
                           Fa := PSL.Build.Build_FA (Prop);
                           Set_PSL_NFA (El, Fa);
                        end if;
                     when N_Sequence_Declaration
                       | N_Endpoint_Declaration =>
                        Prop := Get_Sequence (Decl);
                        Prop := PSL.Rewrites.Rewrite_SERE (Prop);
                        Set_Sequence (Decl, Prop);
                     when others =>
                        Error_Kind ("canon psl_declaration", Decl);
                  end case;
               end;

            when others =>
               Error_Kind ("canon_concurrent_stmts", El);
         end case;
         Prev_El := El;
         El := Get_Chain (El);
      end loop;
   end Canon_Concurrent_Stmts;

--    procedure Canon_Binding_Indication
--      (Component: Iir; Binding : Iir_Binding_Indication)
--    is
--       List : Iir_Association_List;
--    begin
--       if Binding = Null_Iir then
--          return;
--       end if;
--       List := Get_Generic_Map_Aspect_List (Binding);
--       List := Canon_Association_List (Get_Generic_List (Component), List);
--       Set_Generic_Map_Aspect_List (Binding, List);
--       List := Get_Port_Map_Aspect_List (Binding);
--       List := Canon_Association_List (Get_Port_List (Component), List);
--       Set_Port_Map_Aspect_List (Binding, List);
--    end Canon_Binding_Indication;

   procedure Add_Binding_Indication_Dependence (Top : Iir_Design_Unit;
                                                Binding : Iir)
   is
      Aspect : Iir;
      Unit : Iir;
   begin
      if Binding = Null_Iir then
         return;
      end if;
      Aspect := Get_Entity_Aspect (Binding);
      if Aspect = Null_Iir then
         return;
      end if;
      case Get_Kind (Aspect) is
         when Iir_Kind_Entity_Aspect_Entity =>
            if Get_Architecture (Aspect) /= Null_Iir then
               Unit := Aspect;
            else
               Unit := Get_Entity (Aspect);
            end if;
         when Iir_Kind_Entity_Aspect_Configuration =>
            Unit := Get_Configuration (Aspect);
         when Iir_Kind_Entity_Aspect_Open =>
            Unit := Null_Iir;
         when others =>
            Error_Kind ("add_binding_indication_dependence", Aspect);
      end case;
      if Unit /= Null_Iir then
         Add_Dependence (Top, Unit);
      end if;
   end Add_Binding_Indication_Dependence;

   procedure Canon_Component_Configuration (Top : Iir_Design_Unit; Cfg : Iir)
   is
      Bind : Iir;
      Instances : Iir_List;
      Entity_Aspect : Iir;
      Block : Iir_Block_Configuration;
      Map_Chain : Iir;
      Entity : Iir;
   begin
      Bind := Get_Binding_Indication (Cfg);
      if Bind = Null_Iir then
         --  Add a default binding indication
         --  Extract a component instantiation
         Instances := Get_Instantiation_List (Cfg);
         if Instances = Iir_List_All or Instances = Iir_List_Others then
            --  designator_all and designator_others must have been replaced
            --  by a list during canon.
            raise Internal_Error;
         else
            Bind := Get_Default_Binding_Indication
              (Get_First_Element (Instances));
         end if;
         if Bind = Null_Iir then
            --  Component is not bound.
            return;
         end if;
         Set_Binding_Indication (Cfg, Bind);
         Add_Binding_Indication_Dependence (Top, Bind);
         return;
      else
         Entity_Aspect := Get_Entity_Aspect (Bind);
         if Entity_Aspect = Null_Iir then
            Entity_Aspect := Get_Default_Entity_Aspect (Bind);
            Set_Entity_Aspect (Bind, Entity_Aspect);
         end if;
         if Entity_Aspect /= Null_Iir then
            Add_Binding_Indication_Dependence (Top, Bind);
            Entity := Get_Entity_From_Entity_Aspect (Entity_Aspect);
            Map_Chain := Get_Generic_Map_Aspect_Chain (Bind);
            if Map_Chain = Null_Iir then
               Map_Chain := Get_Default_Generic_Map_Aspect_Chain (Bind);
            else
               Map_Chain := Canon_Association_Chain
                 (Get_Generic_Chain (Entity), Map_Chain, Map_Chain);
            end if;
            Set_Generic_Map_Aspect_Chain (Bind, Map_Chain);

            Map_Chain := Get_Port_Map_Aspect_Chain (Bind);
            if Map_Chain = Null_Iir then
               Map_Chain := Get_Default_Port_Map_Aspect_Chain (Bind);
            else
               Map_Chain := Canon_Association_Chain
                 (Get_Port_Chain (Entity), Map_Chain, Map_Chain);
            end if;
            Set_Port_Map_Aspect_Chain (Bind, Map_Chain);

            if Get_Kind (Cfg) = Iir_Kind_Component_Configuration then
               Block := Get_Block_Configuration (Cfg);
               if Block /= Null_Iir then
                  --  If there is no architecture_identifier in the binding,
                  --  set it from the block_configuration.
                  if Get_Kind (Entity_Aspect) = Iir_Kind_Entity_Aspect_Entity
                    and then Get_Architecture (Entity_Aspect) = Null_Iir
                  then
                     Entity := Get_Library_Unit (Get_Entity (Entity_Aspect));
                     if Get_Kind (Entity) /= Iir_Kind_Entity_Declaration then
                        raise Internal_Error;
                     end if;
                     Set_Architecture
                       (Entity_Aspect, Get_Block_Specification (Block));
                  end if;
                  Canon_Block_Configuration (Top, Block);
               end if;
            end if;
         end if;
      end if;
   end Canon_Component_Configuration;

   procedure Canon_Incremental_Binding
     (Conf_Spec : Iir_Configuration_Specification;
      Comp_Conf : Iir_Component_Configuration;
      Parent : Iir)
   is
      function Merge_Association_Chain
        (Inter_Chain : Iir; First_Chain : Iir; Sec_Chain : Iir)
        return Iir
      is
         --  Result (chain).
         First, Last : Iir;

         --  Copy an association and append new elements to FIRST/LAST.
         procedure Copy_Association (Assoc : in out Iir; Inter : Iir)
         is
            El : Iir;
         begin
            loop
               El := Create_Iir (Get_Kind (Assoc));
               Location_Copy (El, Assoc);
               Set_Formal (El, Get_Formal (Assoc));
               Set_Whole_Association_Flag
                 (El, Get_Whole_Association_Flag (Assoc));

               case Get_Kind (Assoc) is
                  when Iir_Kind_Association_Element_Open =>
                     null;
                  when Iir_Kind_Association_Element_By_Expression =>
                     Set_Actual (El, Get_Actual (Assoc));
                     Set_In_Conversion (El, Get_In_Conversion (Assoc));
                     Set_Out_Conversion (El, Get_Out_Conversion (Assoc));
                     Set_Collapse_Signal_Flag
                       (Assoc,
                        Sem.Can_Collapse_Signals (Assoc, Get_Formal (Assoc)));
                  when Iir_Kind_Association_Element_By_Individual =>
                     Set_Actual_Type (El, Get_Actual_Type (Assoc));
                     Set_Individual_Association_Chain
                       (El, Get_Individual_Association_Chain (Assoc));
                  when others =>
                     Error_Kind ("copy_association", Assoc);
               end case;

               Sub_Chain_Append (First, Last, El);
               Assoc := Get_Chain (Assoc);
               exit when Assoc = Null_Iir;
               exit when Get_Associated_Formal (Assoc) /= Inter;
            end loop;
         end Copy_Association;

         procedure Advance (Assoc : in out Iir; Inter : Iir)
         is
         begin
            loop
               Assoc := Get_Chain (Assoc);
               exit when Assoc = Null_Iir;
               exit when Get_Associated_Formal (Assoc) /= Inter;
            end loop;
         end Advance;

         Inter : Iir;
         F_El : Iir;
         S_El : Iir;
      begin
         if Sec_Chain = Null_Iir then
            --  Short-cut.
            return First_Chain;
         end if;
         F_El := First_Chain;
         Sub_Chain_Init (First, Last);
         Inter := Inter_Chain;
         while Inter /= Null_Iir loop
            --  Consistency check.
            if Get_Associated_Formal (F_El) /= Inter then
               raise Internal_Error;
            end if;
            --  Find the associated in the second chain.
            S_El := Sec_Chain;
            while S_El /= Null_Iir loop
               exit when Get_Associated_Formal (S_El) = Inter;
               S_El := Get_Chain (S_El);
            end loop;
            if S_El /= Null_Iir
              and then Get_Kind (S_El) /= Iir_Kind_Association_Element_Open
            then
               Copy_Association (S_El, Inter);
               Advance (F_El, Inter);
            else
               Copy_Association (F_El, Inter);
            end if;
            Inter := Get_Chain (Inter);
         end loop;
         return First;
      end Merge_Association_Chain;

      Res : Iir_Component_Configuration;
      Cs_Binding : Iir_Binding_Indication;
      Cc_Binding : Iir_Binding_Indication;
      Res_Binding : Iir_Binding_Indication;
      Entity : Iir;
      Instance_List : Iir_List;
      Conf_Instance_List : Iir_List;
      Instance : Iir;
      N_Nbr : Natural;
   begin
      --  Create the new component configuration
      Res := Create_Iir (Iir_Kind_Component_Configuration);
      Location_Copy (Res, Comp_Conf);
      Set_Parent (Res, Parent);
      Set_Component_Name (Res, Get_Component_Name (Conf_Spec));

--       --  Keep in the designator list only the non-incrementally
--       --  bound instances.
--       Inst_List := Get_Instantiation_List (Comp_Conf);
--       Designator_List := Create_Iir_List;
--       for I in 0 .. Get_Nbr_Elements (Inst_List) - 1 loop
--          Inst := Get_Nth_Element (Inst_List, I);
--          if Get_Component_Configuration (Inst) = Comp_Conf then
--             Set_Component_Configuration (Inst, Res);
--             Append_Element (Designator_List, Inst);
--          end if;
--       end loop;
--       Set_Instantiation_List (Res, Designator_List);
--       Set_Binding_Indication
--         (Res, Get_Binding_Indication (Comp_Conf));
--       Append (Last_Item, Conf, Comp_Conf);

      Cs_Binding := Get_Binding_Indication (Conf_Spec);
      Cc_Binding := Get_Binding_Indication (Comp_Conf);
      Res_Binding := Create_Iir (Iir_Kind_Binding_Indication);
      Location_Copy (Res_Binding, Res);
      Set_Binding_Indication (Res, Res_Binding);

      Entity := Get_Entity_From_Entity_Aspect (Get_Entity_Aspect (Cs_Binding));

      --  Merge generic map aspect.
      Set_Generic_Map_Aspect_Chain
        (Res_Binding,
         Merge_Association_Chain (Get_Generic_Chain (Entity),
                                  Get_Generic_Map_Aspect_Chain (Cs_Binding),
                                  Get_Generic_Map_Aspect_Chain (Cc_Binding)));

      --  merge port map aspect
      Set_Port_Map_Aspect_Chain
        (Res_Binding,
         Merge_Association_Chain (Get_Port_Chain (Entity),
                                  Get_Port_Map_Aspect_Chain (Cs_Binding),
                                  Get_Port_Map_Aspect_Chain (Cc_Binding)));

      --  set entity aspect
      Set_Entity_Aspect (Res_Binding, Get_Entity_Aspect (Cs_Binding));

      --  create list of instances:
      --   * keep common instances
      --   replace component_configuration of them
      --   remove them in the instance list of COMP_CONF
      Instance_List := Create_Iir_List;
      Set_Instantiation_List (Res, Instance_List);
      Conf_Instance_List := Get_Instantiation_List (Comp_Conf);
      N_Nbr := 0;
      for I in 0 .. Get_Nbr_Elements (Conf_Instance_List) - 1 loop
         Instance := Get_Nth_Element (Conf_Instance_List, I);
         if Get_Component_Configuration (Instance) = Conf_Spec then
            --  The incremental binding applies to this instance.
            Set_Component_Configuration (Instance, Res);
            Append_Element (Instance_List, Instance);
         else
            Replace_Nth_Element (Conf_Instance_List, N_Nbr, Instance);
            N_Nbr := N_Nbr + 1;
         end if;
      end loop;
      Set_Nbr_Elements (Conf_Instance_List, N_Nbr);

      --  Insert RES.
      Set_Chain (Res, Get_Chain (Comp_Conf));
      Set_Chain (Comp_Conf, Res);
   end Canon_Incremental_Binding;

   procedure Canon_Component_Specification_All_Others
     (Conf : Iir; Parent : Iir; Spec : Iir_List; List : Iir_List; Comp : Iir)
   is
      El : Iir;
      Comp_Conf : Iir;
   begin
      El := Get_Concurrent_Statement_Chain (Parent);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Component_Instantiation_Statement =>
               if Get_Instantiated_Unit (El) = Comp then
                  Comp_Conf := Get_Component_Configuration (El);
                  if Comp_Conf = Null_Iir then
                     --  The component is not yet configured.
                     Append_Element (List, El);
                     Set_Component_Configuration (El, Conf);
                  else
                     --  The component is already configured.
                     --  Handle incremental configuration.
                     if (Get_Kind (Comp_Conf)
                         = Iir_Kind_Configuration_Specification)
                       and then Spec = Iir_List_All
                     then
                        --  FIXME: handle incremental configuration.
                        raise Internal_Error;
                     end if;
                     if Spec = Iir_List_All then
                        --  Several component configuration for an instance.
                        --  Must have been caught by sem.
                        raise Internal_Error;
                     elsif Spec = Iir_List_Others then
                        null;
                     else
                        raise Internal_Error;
                     end if;
                  end if;
               end if;
            when Iir_Kind_Generate_Statement =>
               if False
                 and then Vhdl_Std = Vhdl_87
                 and then
                 Get_Kind (Conf) = Iir_Kind_Configuration_Specification
               then
                  Canon_Component_Specification_All_Others
                    (Conf, El, Spec, List, Comp);
               end if;
            when others =>
               null;
         end case;
         El := Get_Chain (El);
      end loop;
   end Canon_Component_Specification_All_Others;

   procedure Canon_Component_Specification_List
     (Conf : Iir; Parent : Iir; Spec : Iir_List)
   is
      El : Iir;
      Comp_Conf : Iir;
   begin
      --  Already has a designator list.
      for I in Natural loop
         El := Get_Nth_Element (Spec, I);
         exit when El = Null_Iir;
         Comp_Conf := Get_Component_Configuration (El);
         if Comp_Conf /= Null_Iir and then Comp_Conf /= Conf then
            if Get_Kind (Comp_Conf) /= Iir_Kind_Configuration_Specification
              or else Get_Kind (Conf) /= Iir_Kind_Component_Configuration
            then
               raise Internal_Error;
            end if;
            Canon_Incremental_Binding (Comp_Conf, Conf, Parent);
         else
            Set_Component_Configuration (El, Conf);
         end if;
      end loop;
   end Canon_Component_Specification_List;

   --  PARENT is the parent for the chain of concurrent statements.
   procedure Canon_Component_Specification (Conf : Iir; Parent : Iir)
   is
      Spec : Iir_List;
      List : Iir_Designator_List;
   begin
      Spec := Get_Instantiation_List (Conf);

      if Spec = Iir_List_All or Spec = Iir_List_Others then
         List := Create_Iir_List;
         Canon_Component_Specification_All_Others
           (Conf, Parent, Spec, List, Get_Component_Name (Conf));
         Set_Instantiation_List (Conf, List);
      else
         --  Has Already a designator list.
         Canon_Component_Specification_List (Conf, Parent, Spec);
      end if;
   end Canon_Component_Specification;

   --  Replace ALL/OTHERS with the explicit list of signals.
   procedure Canon_Disconnection_Specification
     (Dis : Iir_Disconnection_Specification; Decl_Parent : Iir)
   is
      Signal_List : Iir_List;
      Force : Boolean;
      El : Iir;
      N_List : Iir_Designator_List;
   begin
      if Canon_Flag_Expressions then
         Canon_Expression (Get_Expression (Dis));
      end if;
      Signal_List := Get_Signal_List (Dis);
      if Signal_List = Iir_List_All then
         Force := True;
      elsif Signal_List = Iir_List_Others then
         Force := False;
      else
         return;
      end if;
      N_List := Create_Iir_List;
      Set_Signal_List (Dis, N_List);
      El := Get_Declaration_Chain (Decl_Parent);
      while El /= Null_Iir loop
         if Get_Kind (El) = Iir_Kind_Signal_Declaration
           and then Get_Type (El) = Get_Type (Dis)
           and then Get_Signal_Kind (El) /= Iir_No_Signal_Kind
         then
            if not Get_Has_Disconnect_Flag (El) then
               Set_Has_Disconnect_Flag (El, True);
               Append_Element (N_List, El);
            else
               if Force then
                  raise Internal_Error;
               end if;
            end if;
         end if;
         El := Get_Chain (El);
      end loop;
   end Canon_Disconnection_Specification;

   procedure Canon_Declaration (Top : Iir_Design_Unit;
                                Decl : Iir;
                                Parent : Iir;
                                Decl_Parent : Iir)
   is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Procedure_Body
           | Iir_Kind_Function_Body =>
            Canon_Declarations (Top, Decl, Null_Iir);
            if Canon_Flag_Sequentials_Stmts then
               Canon_Sequential_Stmts (Get_Sequential_Statement_Chain (Decl));
            end if;

         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Function_Declaration =>
            null;

         when Iir_Kind_Type_Declaration =>
            declare
               Def : Iir;
            begin
               Def := Get_Type (Decl);
               if Get_Kind (Def) = Iir_Kind_Protected_Type_Declaration then
                  Canon_Declarations (Decl, Def, Null_Iir);
               end if;
            end;

         when Iir_Kind_Anonymous_Type_Declaration
           | Iir_Kind_Subtype_Declaration =>
            null;

         when Iir_Kind_Protected_Type_Body =>
            Canon_Declarations (Top, Decl, Null_Iir);

         when Iir_Kind_Variable_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Constant_Declaration =>
            if Canon_Flag_Expressions then
               Canon_Expression (Get_Default_Value (Decl));
            end if;

         when Iir_Kind_Iterator_Declaration =>
            null;

         when Iir_Kind_Object_Alias_Declaration =>
            null;
         when Iir_Kind_Non_Object_Alias_Declaration =>
            null;

         when Iir_Kind_File_Declaration =>
            -- FIXME
            null;

         when Iir_Kind_Attribute_Declaration =>
            null;
         when Iir_Kind_Attribute_Specification =>
            if Canon_Flag_Expressions then
               Canon_Expression (Get_Expression (Decl));
            end if;
         when Iir_Kind_Disconnection_Specification =>
            Canon_Disconnection_Specification (Decl, Decl_Parent);

         when Iir_Kind_Group_Template_Declaration =>
            null;
         when Iir_Kind_Group_Declaration =>
            null;

         when Iir_Kind_Use_Clause =>
            null;

         when Iir_Kind_Component_Declaration =>
            null;

         when Iir_Kind_Implicit_Procedure_Declaration
           | Iir_Kind_Implicit_Function_Declaration =>
            null;

         when Iir_Kind_Configuration_Specification =>
            Canon_Component_Specification (Decl, Parent);
            Canon_Component_Configuration (Top, Decl);
--             declare
--                List : Iir_List;
--                Binding : Iir_Binding_Indication;
--                Component : Iir_Component_Declaration;
--                Aspect : Iir;
--                Entity : Iir;
--             begin
--                Binding := Get_Binding_Indication (Decl);
--                Component := Get_Component_Name (Decl);
--                Aspect := Get_Entity_Aspect (Binding);
--                case Get_Kind (Aspect) is
--                   when Iir_Kind_Entity_Aspect_Entity =>
--                      Entity := Get_Entity (Aspect);
--                   when others =>
--                      Error_Kind ("configuration_specification", Aspect);
--                end case;
--                Entity := Get_Library_Unit (Entity);
--                List := Get_Generic_Map_Aspect_List (Binding);
--                if List = Null_Iir_List then
--                   Set_Generic_Map_Aspect_List
--                     (Binding,
--                      Canon_Default_Map_Association_List
--                    (Get_Generic_List (Entity), Get_Generic_List (Component),
--                       Get_Location (Decl)));
--                end if;
--                List := Get_Port_Map_Aspect_List (Binding);
--                if List = Null_Iir_List then
--                   Set_Port_Map_Aspect_List
--                     (Binding,
--                      Canon_Default_Map_Association_List
--                      (Get_Port_List (Entity), Get_Port_List (Component),
--                       Get_Location (Decl)));
--                end if;
--             end;

         when Iir_Kinds_Signal_Attribute =>
            null;
         when others =>
            Error_Kind ("canon_declaration", Decl);
      end case;
   end Canon_Declaration;

   procedure Canon_Declarations (Top : Iir_Design_Unit;
                                 Decl_Parent : Iir;
                                 Parent : Iir)
   is
      Decl : Iir;
   begin
      if Parent /= Null_Iir then
         Clear_Instantiation_Configuration (Parent, True);
      end if;
      Decl := Get_Declaration_Chain (Decl_Parent);
      while Decl /= Null_Iir loop
         Canon_Declaration (Top, Decl, Parent, Decl_Parent);
         Decl := Get_Chain (Decl);
      end loop;
   end Canon_Declarations;

   procedure Canon_Block_Configuration (Top : Iir_Design_Unit;
                                        Conf : Iir_Block_Configuration)
   is
      use Iir_Chains.Configuration_Item_Chain_Handling;
      El : Iir;
      Spec : Iir;
      Stmts : Iir;
      Blk : Iir;
      Sub_Blk : Iir;
      Last_Item : Iir;
   begin
      --  Note: the only allowed declarations are use clauses, which are not
      --  canonicalized.

      --  FIXME: handle indexed/sliced name?
      Spec := Get_Block_Specification (Conf);
      Blk := Get_Block_From_Block_Specification (Spec);
      Stmts := Get_Concurrent_Statement_Chain (Blk);

      Clear_Instantiation_Configuration (Blk, False);

      Build_Init (Last_Item, Conf);

      --  1) Configure instantiations with configuration specifications.
      --  TODO: merge.
      El := Get_Declaration_Chain (Blk);
      while El /= Null_Iir loop
         if Get_Kind (El) = Iir_Kind_Configuration_Specification then
            --  Already canoncalized during canon of block declarations.
            --  But need to set configuration on instantiations.
            Canon_Component_Specification (El, Blk);
         end if;
         El := Get_Chain (El);
      end loop;

      --  2) Configure instantations with component configurations,
      --     and map block configurations with block/generate statements.
      El := Get_Configuration_Item_Chain (Conf);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Configuration_Specification =>
               raise Internal_Error;
            when Iir_Kind_Component_Configuration =>
               Canon_Component_Specification (El, Blk);
            when Iir_Kind_Block_Configuration =>
               Sub_Blk := Get_Block_Specification (El);
               case Get_Kind (Sub_Blk) is
                  when Iir_Kind_Block_Statement =>
                     Set_Block_Block_Configuration (Sub_Blk, El);
                  when Iir_Kind_Indexed_Name
                    | Iir_Kind_Slice_Name =>
                     Sub_Blk := Get_Prefix (Sub_Blk);
                     Set_Prev_Block_Configuration
                       (El, Get_Generate_Block_Configuration (Sub_Blk));
                     Set_Generate_Block_Configuration (Sub_Blk, El);
                  when Iir_Kind_Generate_Statement =>
                     Set_Generate_Block_Configuration (Sub_Blk, El);
                  when others =>
                     Error_Kind ("canon_block_configuration(0)", Sub_Blk);
               end case;
            when others =>
               Error_Kind ("canon_block_configuration(1)", El);
         end case;
         El := Get_Chain (El);
      end loop;

      --  3) Add default component configuration for unspecified component
      --     instantiation statements,
      --     Add default block configuration for unconfigured block statements.
      El := Stmts;
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Component_Instantiation_Statement =>
               declare
                  Comp_Conf : Iir;
                  Comp : Iir;
                  Res : Iir_Component_Configuration;
                  Designator_List : Iir_List;
                  Inst_List : Iir_List;
                  Inst : Iir;
               begin
                  Comp_Conf := Get_Component_Configuration (El);
                  if Comp_Conf = Null_Iir then
                     Comp := Get_Instantiated_Unit (El);
                     if Get_Kind (Comp) = Iir_Kind_Component_Declaration then
                        --  Create a component configuration.
                        --  FIXME: should merge all these default configuration
                        --    of the same component.
                        Res := Create_Iir (Iir_Kind_Component_Configuration);
                        Location_Copy (Res, El);
                        Set_Parent (Res, Conf);
                        Set_Component_Name (Res, Comp);
                        Designator_List := Create_Iir_List;
                        Append_Element (Designator_List, El);
                        Set_Instantiation_List (Res, Designator_List);
                        Append (Last_Item, Conf, Res);
                     end if;
                  elsif Get_Kind (Comp_Conf)
                    = Iir_Kind_Configuration_Specification
                  then
                     --  Create component configuration
                     Res := Create_Iir (Iir_Kind_Component_Configuration);
                     Location_Copy (Res, Comp_Conf);
                     Set_Parent (Res, Conf);
                     Set_Component_Name (Res, Get_Component_Name (Comp_Conf));
                     --  Keep in the designator list only the non-incrementally
                     --  bound instances, and only the instances in the current
                     --  statements parts (vhdl-87 generate issue).
                     Inst_List := Get_Instantiation_List (Comp_Conf);
                     Designator_List := Create_Iir_List;
                     for I in 0 .. Get_Nbr_Elements (Inst_List) - 1 loop
                        Inst := Get_Nth_Element (Inst_List, I);
                        if Get_Component_Configuration (Inst) = Comp_Conf
                          and then Get_Parent (Inst) = Blk
                        then
                           Set_Component_Configuration (Inst, Res);
                           Append_Element (Designator_List, Inst);
                        end if;
                     end loop;
                     Set_Instantiation_List (Res, Designator_List);
                     Set_Binding_Indication
                       (Res, Get_Binding_Indication (Comp_Conf));
                     Append (Last_Item, Conf, Res);
                  end if;
               end;
            when Iir_Kind_Block_Statement =>
               declare
                  Res : Iir_Block_Configuration;
               begin
                  if Get_Block_Block_Configuration (El) = Null_Iir then
                     Res := Create_Iir (Iir_Kind_Block_Configuration);
                     Location_Copy (Res, El);
                     Set_Parent (Res, Conf);
                     Set_Block_Specification (Res, El);
                     Append (Last_Item, Conf, Res);
                  end if;
               end;
            when Iir_Kind_Generate_Statement =>
               declare
                  Res : Iir_Block_Configuration;
                  Scheme : Iir;
                  Blk_Config : Iir_Block_Configuration;
                  Blk_Spec : Iir;
               begin
                  Scheme := Get_Generation_Scheme (El);
                  Blk_Config := Get_Generate_Block_Configuration (El);
                  if Blk_Config = Null_Iir then
                     --  No block configuration for the (implicit) internal
                     --  block.  Create one.
                     Res := Create_Iir (Iir_Kind_Block_Configuration);
                     Location_Copy (Res, El);
                     Set_Parent (Res, Conf);
                     Set_Block_Specification (Res, El);
                     Append (Last_Item, Conf, Res);
                  elsif Get_Kind (Scheme) = Iir_Kind_Iterator_Declaration then
                     Blk_Spec := Get_Block_Specification (Blk_Config);
                     if Get_Kind (Blk_Spec) /= Iir_Kind_Generate_Statement then
                        --  There are partial configurations.
                        --  Create a default block configuration.
                        Res := Create_Iir (Iir_Kind_Block_Configuration);
                        Location_Copy (Res, El);
                        Set_Parent (Res, Conf);
                        Blk_Spec := Create_Iir (Iir_Kind_Selected_Name);
                        Location_Copy (Blk_Spec, Res);
                        Set_Suffix_Identifier
                          (Blk_Spec, Std_Names.Name_Others);
                        Set_Prefix (Blk_Spec, El);
                        Set_Block_Specification (Res, Blk_Spec);
                        Append (Last_Item, Conf, Res);
                     end if;
                  end if;
               end;
            when Iir_Kind_Sensitized_Process_Statement
              | Iir_Kind_Process_Statement
              | Iir_Kind_Psl_Assert_Statement
              | Iir_Kind_Psl_Default_Clock
              | Iir_Kind_Psl_Declaration =>
               null;
            when others =>
               Error_Kind ("canon_block_configuration(3)", El);
         end case;
         El := Get_Chain (El);
      end loop;

      --  4) Canon component configuration and block configuration (recursion).
      El := Get_Configuration_Item_Chain (Conf);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Block_Configuration =>
               Canon_Block_Configuration (Top, El);
            when Iir_Kind_Component_Configuration =>
               Canon_Component_Configuration (Top, El);
            when others =>
               Error_Kind ("canon_block_configuration", El);
         end case;
         El := Get_Chain (El);
      end loop;
   end Canon_Block_Configuration;

   procedure Canonicalize (Unit: Iir_Design_Unit)
   is
      El: Iir;
   begin
      if False then
         --  Canon context clauses.
         --  This code is not executed since context clauses are already
         --  canonicalized.
         El := Get_Context_Items (Unit);
         while El /= Null_Iir loop
            case Get_Kind (El) is
               when Iir_Kind_Use_Clause =>
                  null;
               when Iir_Kind_Library_Clause =>
                  null;
               when others =>
                  Error_Kind ("canonicalize1", El);
            end case;
         end loop;
      end if;

      El := Get_Library_Unit (Unit);
      case Get_Kind (El) is
         when Iir_Kind_Entity_Declaration =>
            Canon_Declarations (Unit, El, El);
            Canon_Concurrent_Stmts (Unit, El);
         when Iir_Kind_Architecture_Declaration =>
            Canon_Declarations (Unit, El, El);
            Canon_Concurrent_Stmts (Unit, El);
         when Iir_Kind_Package_Declaration =>
            Canon_Declarations (Unit, El, Null_Iir);
         when Iir_Kind_Package_Body =>
            Canon_Declarations (Unit, El, Null_Iir);
         when Iir_Kind_Configuration_Declaration =>
            Canon_Declarations (Unit, El, Null_Iir);
            Canon_Block_Configuration (Unit, Get_Block_Configuration (El));
         when others =>
            Error_Kind ("canonicalize2", El);
      end case;
   end Canonicalize;

--    --  Create a default component configuration for component instantiation
--    --  statement INST.
--    function Create_Default_Component_Configuration
--      (Inst : Iir_Component_Instantiation_Statement;
--       Parent : Iir;
--       Config_Unit : Iir_Design_Unit)
--      return Iir_Component_Configuration
--    is
--       Res : Iir_Component_Configuration;
--       Designator : Iir;
--       Comp : Iir_Component_Declaration;
--       Bind : Iir;
--       Aspect : Iir;
--    begin
--       Bind := Get_Default_Binding_Indication (Inst);

--       if Bind = Null_Iir then
--          --  Component is not bound.
--          return Null_Iir;
--       end if;

--       Res := Create_Iir (Iir_Kind_Component_Configuration);
--       Location_Copy (Res, Inst);
--       Set_Parent (Res, Parent);
--       Comp := Get_Instantiated_Unit (Inst);

--       Set_Component_Name (Res, Comp);
--       --  Create the instantiation list with only one element: INST.
--       Designator := Create_Iir (Iir_Kind_Designator_List);
--       Append_Element (Designator, Inst);
--       Set_Instantiation_List (Res, Designator);

--       Set_Binding_Indication (Res, Bind);
--       Aspect := Get_Entity_Aspect (Bind);
--       case Get_Kind (Aspect) is
--          when Iir_Kind_Entity_Aspect_Entity =>
--             Add_Dependence (Config_Unit, Get_Entity (Aspect));
--             if Get_Architecture (Aspect) /= Null_Iir then
--                raise Internal_Error;
--             end if;
--          when others =>
--             Error_Kind ("Create_Default_Component_Configuration", Aspect);
--       end case;

--       return Res;
--    end Create_Default_Component_Configuration;

   --  Create a default configuration declaration for architecture ARCH.
   function Create_Default_Configuration_Declaration
     (Arch : Iir_Architecture_Declaration)
     return Iir_Design_Unit
   is
      Loc : Location_Type;
      Config : Iir_Configuration_Declaration;
      Res : Iir_Design_Unit;
      Entity : Iir_Entity_Declaration;
      Blk_Cfg : Iir_Block_Configuration;
   begin
      Loc := Get_Location (Arch);
      Res := Create_Iir (Iir_Kind_Design_Unit);
      Set_Location (Res, Loc);
      Set_Parent (Res, Get_Parent (Get_Design_Unit (Arch)));
      Set_Date_State (Res, Date_Analyze);
      Set_Date (Res, Date_Uptodate);
      Config := Create_Iir (Iir_Kind_Configuration_Declaration);
      Set_Location (Config, Loc);
      Set_Library_Unit (Res, Config);
      Set_Design_Unit (Config, Res);
      Entity := Get_Entity (Arch);
      Set_Entity (Config, Get_Design_Unit (Entity));
      Set_Dependence_List (Res, Create_Iir_List);
      Add_Dependence (Res, Get_Design_Unit (Entity));
      Add_Dependence (Res, Get_Design_Unit (Arch));

      Blk_Cfg := Create_Iir (Iir_Kind_Block_Configuration);
      Set_Location (Blk_Cfg, Loc);
      Set_Parent (Blk_Cfg, Config);
      Set_Block_Specification (Blk_Cfg, Arch);
      Set_Block_Configuration (Config, Blk_Cfg);

      Canon_Block_Configuration (Res, Blk_Cfg);

      return Res;
   end Create_Default_Configuration_Declaration;

end Canon;
