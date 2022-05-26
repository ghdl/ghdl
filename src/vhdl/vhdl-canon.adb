--  Canonicalization pass
--  Copyright (C) 2002, 2003, 2004, 2005, 2008 Tristan Gingold
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
with Flags; use Flags;
with Name_Table;
with Errorout; use Errorout;

with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Sem;
with Vhdl.Sem_Inst;
with Vhdl.Sem_Specs;
with Vhdl.Nodes_Utils; use Vhdl.Nodes_Utils;
with PSL.Types; use PSL.Types;
with PSL.Nodes;
with PSL.Rewrites;
with PSL.Build;
with PSL.NFAs;
with PSL.NFAs.Utils;
with PSL.Errors; use PSL.Errors;
with Vhdl.Canon_PSL;

package body Vhdl.Canon is
   Canon_Flag_Set_Assoc_Formals : constant Boolean := False;

   --  Canonicalize the chain of declarations in Declaration_Chain of
   --  DECL_PARENT. PARENT must be the parent of the current statements chain,
   --  or NULL_IIR if DECL_PARENT has no corresponding current statments.
   --  TOP is used to add dependencies (from binding indications).
   procedure Canon_Declarations (Top : Iir_Design_Unit;
                                 Decl_Parent : Iir;
                                 Parent : Iir);
   function Canon_Declaration (Top : Iir_Design_Unit; Decl : Iir; Parent : Iir)
                              return Iir;

   procedure Canon_Concurrent_Stmts (Top : Iir_Design_Unit; Parent : Iir);
   procedure Canon_Simultaneous_Stmts (Top : Iir_Design_Unit; Chain : Iir);

   --  Canonicalize an association list.
   --  If ASSOCIATION_LIST is not null, then it is re-ordored and returned.
   --  If ASSOCIATION_LIST is null then:
   --    if INTERFACE_LIST is null then returns null.
   --    if INTERFACE_LIST is not null, a default list is created.
   function Canon_Association_Chain
     (Interface_Chain: Iir; Association_Chain: Iir; Loc : Iir)
     return Iir;

   --  Like Canon_Association_Chain but recurse on actuals.
   function Canon_Association_Chain_And_Actuals
     (Interface_Chain: Iir; Association_Chain: Iir; Loc : Iir)
     return Iir;

   --  Like Canon_Subprogram_Call, but recurse on actuals.
   procedure Canon_Subprogram_Call_And_Actuals (Call : Iir);

   --  Canonicalize block configuration CONF.
   --  TOP is used to added dependences to the design unit which CONF
   --  belongs to.
   procedure Canon_Block_Configuration (Top : Iir_Design_Unit;
                                        Conf : Iir_Block_Configuration);

   procedure Canon_Subtype_Indication (Def : Iir);
   procedure Canon_Subtype_Indication_If_Anonymous (Def : Iir);

   function Canon_Conditional_Signal_Assignment
     (Conc_Stmt : Iir; Proc : Iir; Parent : Iir; Clear : Boolean) return Iir;
   procedure Canon_Conditional_Signal_Assignment_Expression (Stmt : Iir);

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
            Canon_Extract_Sensitivity_Expression
              (Get_Associated_Expr (Assoc), Sensitivity_List, Is_Target);
            Assoc := Get_Chain (Assoc);
         end loop;
      else
         while Assoc /= Null_Iir loop
            Canon_Extract_Sensitivity_Aggregate
              (Get_Associated_Expr (Assoc), Sensitivity_List,
               Is_Target, Aggr_Type, Dim + 1);
            Assoc := Get_Chain (Assoc);
         end loop;
      end if;
   end Canon_Extract_Sensitivity_Aggregate;

   procedure Canon_Extract_Sensitivity_Expression
     (Expr: Iir; Sensitivity_List: Iir_List; Is_Target: Boolean := False)
   is
      El : Iir;
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
                  Canon_Extract_Sensitivity_Expression
                    (Get_Prefix (Expr), Sensitivity_List, Is_Target);
                  Suff := Get_Suffix (Expr);
                  if Get_Kind (Suff)
                    not in Iir_Kinds_Scalar_Type_And_Subtype_Definition
                  then
                     Canon_Extract_Sensitivity_Expression
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
               Canon_Extract_Sensitivity_Expression
                 (Get_Prefix (Expr), Sensitivity_List, Is_Target);
            end if;

         when Iir_Kind_Indexed_Name =>
            if not Is_Target
              and then Get_Name_Staticness (Expr) >= Globally
            then
               if Is_Signal_Object (Expr) then
                  Add_Element (Sensitivity_List, Expr);
               end if;
            else
               Canon_Extract_Sensitivity_Expression
                 (Get_Prefix (Expr), Sensitivity_List, Is_Target);
               declare
                  Flist : constant Iir_Flist := Get_Index_List (Expr);
                  El : Iir;
               begin
                  for I in Flist_First .. Flist_Last (Flist) loop
                     El := Get_Nth_Element (Flist, I);
                     Canon_Extract_Sensitivity_Expression
                       (El, Sensitivity_List, False);
                  end loop;
               end;
            end if;

         when Iir_Kind_Function_Call =>
            El := Get_Parameter_Association_Chain (Expr);
            while El /= Null_Iir loop
               case Get_Kind (El) is
                  when Iir_Kind_Association_Element_By_Expression =>
                     Canon_Extract_Sensitivity_Expression
                       (Get_Actual (El), Sensitivity_List, False);
                  when Iir_Kind_Association_Element_Open =>
                     null;
                  when Iir_Kind_Association_Element_By_Individual =>
                     null;
                  when others =>
                     Error_Kind ("canon_extract_sensitivity(call)", El);
               end case;
               El := Get_Chain (El);
            end loop;

         when Iir_Kind_Qualified_Expression
           | Iir_Kind_Type_Conversion
           | Iir_Kind_Allocator_By_Expression
           | Iir_Kind_Parenthesis_Expression =>
            Canon_Extract_Sensitivity_Expression
              (Get_Expression (Expr), Sensitivity_List, False);

         when Iir_Kind_Allocator_By_Subtype =>
            null;

         when Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference =>
            Canon_Extract_Sensitivity_Expression
              (Get_Prefix (Expr), Sensitivity_List, False);

         when Iir_Kind_External_Variable_Name
           | Iir_Kind_External_Constant_Name =>
            null;

         when Iir_Kinds_Monadic_Operator =>
            Canon_Extract_Sensitivity_Expression
              (Get_Operand (Expr), Sensitivity_List, False);
         when Iir_Kinds_Dyadic_Operator =>
            Canon_Extract_Sensitivity_Expression
              (Get_Left (Expr), Sensitivity_List, False);
            Canon_Extract_Sensitivity_Expression
              (Get_Right (Expr), Sensitivity_List, False);

         when Iir_Kind_Range_Expression =>
            Canon_Extract_Sensitivity_Expression
              (Get_Left_Limit (Expr), Sensitivity_List, False);
            Canon_Extract_Sensitivity_Expression
              (Get_Right_Limit (Expr), Sensitivity_List, False);

         when Iir_Kinds_Type_Attribute =>
            null;
         when Iir_Kinds_Signal_Value_Attribute =>
            --  LRM 8.1
            --  An attribute name: [...]; otherwise, apply this rule to the
            --  prefix of the attribute name.
            Canon_Extract_Sensitivity_Expression
              (Get_Prefix (Expr), Sensitivity_List, False);

         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kinds_Signal_Attribute
           | Iir_Kind_Above_Attribute
           | Iir_Kind_External_Signal_Name =>
            --  LRM 8.1
            --  A simple name that denotes a signal, add the longest static
            --  prefix of the name to the sensitivity set;
            --
            --  An attribute name: if the designator denotes a signal
            --  attribute, add the longest static prefix of the name of the
            --  implicit signal denoted by the attribute name to the
            --  sensitivity set; [...]
            if not Is_Target then
               Add_Element (Sensitivity_List, Expr);
            end if;

         when Iir_Kind_Psl_Endpoint_Declaration =>
            declare
               List : constant Iir_List := Get_PSL_Clock_Sensitivity (Expr);
               It : List_Iterator;
            begin
               It := List_Iterate (List);
               while Is_Valid (It) loop
                  Add_Element (Sensitivity_List, Get_Element (It));
                  Next (It);
               end loop;
            end;

         when Iir_Kind_Object_Alias_Declaration =>
            if not Is_Target and then Is_Signal_Object (Expr) then
               Add_Element (Sensitivity_List, Expr);
            end if;

         when Iir_Kind_Constant_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kinds_Quantity_Declaration =>
            null;

         when Iir_Kinds_Array_Attribute =>
            -- was Iir_Kind_Left_Array_Attribute
            -- ditto Right, Low, High, Length
            -- add Ascending, Range and Reverse_Range...
            null;
            --Canon_Extract_Sensitivity
            --  (Get_Prefix (Expr), Sensitivity_List, Is_Target);

         when Iir_Kind_Value_Attribute
           | Iir_Kind_Image_Attribute
           | Iir_Kinds_Scalar_Type_Attribute =>
            Canon_Extract_Sensitivity_Expression
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
                        Canon_Extract_Sensitivity_Expression
                          (Get_Associated_Expr (El), Sensitivity_List,
                           Is_Target);
                        El := Get_Chain (El);
                     end loop;
                  when others =>
                     Error_Kind ("canon_extract_sensitivity(aggr)", Aggr_Type);
               end case;
            end;

         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Reference_Name =>
            Canon_Extract_Sensitivity_Expression
              (Get_Named_Entity (Expr), Sensitivity_List, Is_Target);

         when others =>
            Error_Kind ("canon_extract_sensitivity", Expr);
      end case;
   end Canon_Extract_Sensitivity_Expression;

   procedure Canon_Extract_Sensitivity_If_Not_Null
     (Expr: Iir; Sensitivity_List: Iir_List; Is_Target: Boolean := False) is
   begin
      if Expr /= Null_Iir then
         Canon_Extract_Sensitivity_Expression
           (Expr, Sensitivity_List, Is_Target);
      end if;
   end Canon_Extract_Sensitivity_If_Not_Null;

   procedure Canon_Extract_Sensitivity_Procedure_Call
     (Call : Iir; Sensitivity_List : Iir_List)
   is
      Assoc : Iir;
      Inter : Iir;
   begin
      Assoc := Get_Parameter_Association_Chain (Call);
      Inter := Get_Interface_Declaration_Chain (Get_Implementation (Call));
      while Assoc /= Null_Iir loop
         if (Get_Kind (Assoc) = Iir_Kind_Association_Element_By_Expression)
           and then (Get_Mode (Get_Association_Interface (Assoc, Inter))
                       /= Iir_Out_Mode)
         then
            Canon_Extract_Sensitivity_Expression
              (Get_Actual (Assoc), Sensitivity_List);
         end if;
         Next_Association_Interface (Assoc, Inter);
      end loop;
   end Canon_Extract_Sensitivity_Procedure_Call;

   procedure Canon_Extract_Sensitivity_Waveform (Chain : Iir; List : Iir_List)
   is
      We: Iir_Waveform_Element;
   begin
      We := Chain;
      while We /= Null_Iir loop
         Canon_Extract_Sensitivity_Expression (Get_We_Value (We), List);
         Canon_Extract_Sensitivity_If_Not_Null (Get_Time (We), List);
         We := Get_Chain (We);
      end loop;
   end Canon_Extract_Sensitivity_Waveform;

   procedure Canon_Extract_Sensitivity_Signal_Assignment_Common
     (Stmt : Iir; List : Iir_List) is
   begin
      Canon_Extract_Sensitivity_Expression (Get_Target (Stmt), List, True);
      Canon_Extract_Sensitivity_If_Not_Null
        (Get_Reject_Time_Expression (Stmt), List);
   end Canon_Extract_Sensitivity_Signal_Assignment_Common;

   procedure Canon_Extract_Sensitivity_Conditional_Signal_Assignment
     (Stmt : Iir; List : Iir_List)
   is
      Cwe : Iir;
   begin
      Canon_Extract_Sensitivity_Signal_Assignment_Common (Stmt, List);
      Cwe := Get_Conditional_Waveform_Chain (Stmt);
      while Cwe /= Null_Iir loop
         Canon_Extract_Sensitivity_If_Not_Null (Get_Condition (Cwe), List);
         Canon_Extract_Sensitivity_Waveform (Get_Waveform_Chain (Cwe), List);
         Cwe := Get_Chain (Cwe);
      end loop;
   end Canon_Extract_Sensitivity_Conditional_Signal_Assignment;

   procedure Canon_Extract_Sensitivity_Simple_Signal_Assignment
     (Stmt : Iir; List : Iir_List) is
   begin
      Canon_Extract_Sensitivity_Signal_Assignment_Common (Stmt, List);
      Canon_Extract_Sensitivity_Waveform (Get_Waveform_Chain (Stmt), List);
   end Canon_Extract_Sensitivity_Simple_Signal_Assignment;

   procedure Canon_Extract_Sensitivity_Selected_Signal_Assignment
     (Stmt : Iir; List : Iir_List)
   is
      Swf : Node;
      Wf : Node;
   begin
      Canon_Extract_Sensitivity_Signal_Assignment_Common (Stmt, List);
      Canon_Extract_Sensitivity_Expression (Get_Expression (Stmt), List);

      Swf := Get_Selected_Waveform_Chain (Stmt);
      while Swf /= Null_Node loop
         Wf := Get_Associated_Chain (Swf);
         if Wf /= Null_Iir then
            Canon_Extract_Sensitivity_Waveform (Wf, List);
         end if;
         Swf := Get_Chain (Swf);
      end loop;
   end Canon_Extract_Sensitivity_Selected_Signal_Assignment;

   procedure Canon_Extract_Sensitivity_Assertion_Statement
     (Stmt : Iir; List : Iir_List) is
   begin
      Canon_Extract_Sensitivity_Expression
        (Get_Assertion_Condition (Stmt), List);
      Canon_Extract_Sensitivity_If_Not_Null
        (Get_Severity_Expression (Stmt), List);
      Canon_Extract_Sensitivity_If_Not_Null
        (Get_Report_Expression (Stmt), List);
   end Canon_Extract_Sensitivity_Assertion_Statement;

   procedure Canon_Extract_Sensitivity_Statement
     (Stmt : Iir; List : Iir_List) is
   begin
      case Iir_Kinds_Sequential_Statement_Ext (Get_Kind (Stmt)) is
         when Iir_Kind_Assertion_Statement =>
            --  LRM08 11.3
            --  * For each assertion, report, next, exit or return
            --    statement, apply the rule of 10.2 to each expression
            --    in the statement, and construct the union of the
            --    resulting sets.
            Canon_Extract_Sensitivity_Assertion_Statement (Stmt, List);
         when Iir_Kind_Report_Statement =>
            --  LRM08 11.3
            --  See assertion_statement case.
            Canon_Extract_Sensitivity_If_Not_Null
              (Get_Severity_Expression (Stmt), List);
            Canon_Extract_Sensitivity_Expression
              (Get_Report_Expression (Stmt), List);
         when Iir_Kind_Next_Statement
            | Iir_Kind_Exit_Statement =>
            --  LRM08 11.3
            --  See assertion_statement case.
            Canon_Extract_Sensitivity_If_Not_Null
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
            Canon_Extract_Sensitivity_Expression
              (Get_Target (Stmt), List, True);
            Canon_Extract_Sensitivity_Expression
              (Get_Expression (Stmt), List, False);
         when Iir_Kind_Simple_Signal_Assignment_Statement =>
            --  LRM08 11.3
            --  See variable assignment statement case.
            Canon_Extract_Sensitivity_Simple_Signal_Assignment (Stmt, List);
         when Iir_Kind_Conditional_Signal_Assignment_Statement =>
            Canon_Extract_Sensitivity_Conditional_Signal_Assignment
              (Stmt, List);
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
                     Canon_Extract_Sensitivity_Expression (Cond, List);
                  end if;
                  Canon_Extract_Sensitivity_Sequential_Statement_Chain
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
            Canon_Extract_Sensitivity_Expression (Get_Expression (Stmt), List);
            declare
               Choice : Iir;
            begin
               Choice := Get_Case_Statement_Alternative_Chain (Stmt);
               while Choice /= Null_Iir loop
                  Canon_Extract_Sensitivity_Sequential_Statement_Chain
                    (Get_Associated_Chain (Choice), List);
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
            Canon_Extract_Sensitivity_Sequential_Statement_Chain
              (Get_Sequential_Statement_Chain (Stmt), List);
         when Iir_Kind_For_Loop_Statement =>
            --  LRM08 11.3
            --  See loop statement case.
            declare
               It : constant Iir := Get_Parameter_Specification (Stmt);
               It_Type : constant Iir := Get_Type (It);
               Rng     : constant Iir := Get_Range_Constraint (It_Type);
            begin
               if Get_Kind (Rng) = Iir_Kind_Range_Expression then
                  Canon_Extract_Sensitivity_Expression (Rng, List);
               end if;
            end;
            Canon_Extract_Sensitivity_Sequential_Statement_Chain
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
            Canon_Extract_Sensitivity_Procedure_Call
              (Get_Procedure_Call (Stmt), List);
         when Iir_Kind_Selected_Waveform_Assignment_Statement
           | Iir_Kind_Conditional_Variable_Assignment_Statement
           | Iir_Kind_Signal_Force_Assignment_Statement
           | Iir_Kind_Signal_Release_Assignment_Statement
           | Iir_Kind_Break_Statement
           | Iir_Kind_Wait_Statement
           | Iir_Kind_Suspend_State_Statement =>
            Error_Kind ("canon_extract_sensitivity_statement", Stmt);
      end case;
   end Canon_Extract_Sensitivity_Statement;

   procedure Canon_Extract_Sensitivity_Sequential_Statement_Chain
     (Chain : Iir; List : Iir_List)
   is
      Stmt : Iir;
   begin
      Stmt := Chain;
      while Stmt /= Null_Iir loop
         Canon_Extract_Sensitivity_Statement (Stmt, List);
         Stmt := Get_Chain (Stmt);
      end loop;
   end Canon_Extract_Sensitivity_Sequential_Statement_Chain;

   procedure Canon_Extract_Sensitivity_From_Callees
     (Callees_List : Iir_List; Sensitivity_List : Iir_List)
   is
      Callee : Iir;
      Orig_Callee : Iir;
      It : List_Iterator;
      Bod : Iir;
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
      It := List_Iterate (Callees_List);
      while Is_Valid (It) loop
         Callee := Get_Element (It);

         --  For subprograms of instantiated packages, refer to the
         --  uninstantiated subprogram.
         --  FIXME: not for macro-expanded packages
         Orig_Callee := Sem_Inst.Get_Origin (Callee);
         if Orig_Callee /= Null_Iir then
            Callee := Orig_Callee;
         end if;

         if not Get_Seen_Flag (Callee) then
            Set_Seen_Flag (Callee, True);
            case Get_All_Sensitized_State (Callee) is
               when Read_Signal =>
                  Bod := Get_Subprogram_Body (Callee);

                  --  Extract sensitivity from signals read in the body.
                  --  FIXME: what about signals read during in declarations ?
                  Canon_Extract_Sensitivity_Sequential_Statement_Chain
                    (Get_Sequential_Statement_Chain (Bod), Sensitivity_List);

                  --  Extract sensitivity from subprograms called.
                  Canon_Extract_Sensitivity_From_Callees
                    (Get_Callees_List (Bod), Sensitivity_List);

               when No_Signal =>
                  null;

               when Invalid_Signal =>
                  --  Cannot be here.  The error must have been detected.
                  raise Internal_Error;

               when Unknown =>
                  --  Must be a subprogram declared in a different design unit,
                  --  or a subprogram calling such a subprogram.
                  --  Only a package can apply to this case.
                  --  Will be checked at elaboration.
                  pragma Assert (not Flags.Flag_Elaborate);
                  null;
            end case;
         end if;
         Next (It);
      end loop;
   end Canon_Extract_Sensitivity_From_Callees;

   function Canon_Extract_Sensitivity_Process
     (Proc : Iir_Sensitized_Process_Statement) return Iir_List
   is
      Res : Iir_List;
   begin
      Res := Create_Iir_List;

      --  Signals read by statements.
      --  FIXME: justify why signals read in declarations don't care.
      Canon_Extract_Sensitivity_Sequential_Statement_Chain
        (Get_Sequential_Statement_Chain (Proc), Res);

      --  Signals read indirectly by subprograms called.
      Canon_Extract_Sensitivity_From_Callees (Get_Callees_List (Proc), Res);

      --  Reset Seen_Flag of proc and its callees.
      Set_Seen_Flag (Proc, True);
      Clear_Seen_Flag (Proc);

      return Res;
   end Canon_Extract_Sensitivity_Process;

   procedure Canon_Aggregate_Expression (Expr: Iir)
   is
      Assoc : Iir;
   begin
      Assoc := Get_Association_Choices_Chain (Expr);
      while Assoc /= Null_Iir loop
         case Get_Kind (Assoc) is
            when Iir_Kind_Choice_By_Others
              | Iir_Kind_Choice_By_None
              | Iir_Kind_Choice_By_Name =>
               null;
            when Iir_Kind_Choice_By_Expression =>
               Canon_Expression (Get_Choice_Expression (Assoc));
            when Iir_Kind_Choice_By_Range =>
               declare
                  Choice : constant Iir := Get_Choice_Range (Assoc);
               begin
                  if Get_Kind (Choice) = Iir_Kind_Range_Expression then
                     Canon_Expression (Choice);
                  end if;
               end;
            when others =>
               Error_Kind ("canon_aggregate_expression", Assoc);
         end case;
         Canon_Expression (Get_Associated_Expr (Assoc));
         Assoc := Get_Chain (Assoc);
      end loop;
   end Canon_Aggregate_Expression;

   -- canon on expressions, mainly for function calls.
   procedure Canon_Expression (Expr: Iir) is
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
               Suffix := Strip_Denoting_Name (Get_Suffix (Expr));
               if Get_Kind (Suffix) /= Iir_Kind_Subtype_Declaration then
                  Canon_Expression (Suffix);
               end if;
               Canon_Expression (Get_Prefix (Expr));
            end;

         when Iir_Kind_Indexed_Name =>
            Canon_Expression (Get_Prefix (Expr));
            declare
               Flist : constant Iir_Flist := Get_Index_List (Expr);
               El : Iir;
            begin
               for I in Flist_First .. Flist_Last (Flist) loop
                  El := Get_Nth_Element (Flist, I);
                  Canon_Expression (El);
               end loop;
            end;

         when Iir_Kind_Selected_Element =>
            Canon_Expression (Get_Prefix (Expr));
         when Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference =>
            Canon_Expression (Get_Prefix (Expr));

         when Iir_Kinds_Denoting_Name =>
            Canon_Expression (Get_Named_Entity (Expr));

         when Iir_Kinds_Monadic_Operator =>
            Canon_Expression (Get_Operand (Expr));
         when Iir_Kinds_Dyadic_Operator =>
            Canon_Expression (Get_Left (Expr));
            Canon_Expression (Get_Right (Expr));

         when Iir_Kind_Function_Call =>
            Canon_Subprogram_Call_And_Actuals (Expr);
            -- FIXME:
            -- should canon concatenation.

         when Iir_Kind_Parenthesis_Expression =>
            Canon_Expression (Get_Expression (Expr));
         when Iir_Kind_Type_Conversion
           | Iir_Kind_Qualified_Expression =>
            Canon_Expression (Get_Expression (Expr));
         when Iir_Kind_Aggregate =>
            Canon_Aggregate_Expression (Expr);
         when Iir_Kind_Allocator_By_Expression =>
            Canon_Expression (Get_Expression (Expr));
         when Iir_Kind_Allocator_By_Subtype =>
            declare
               Ind : constant Iir := Get_Subtype_Indication (Expr);
            begin
               if Get_Kind (Ind) = Iir_Kind_Array_Subtype_Definition then
                  Canon_Subtype_Indication (Ind);
               end if;
            end;

         when Iir_Kinds_Literal
           | Iir_Kind_Simple_Aggregate
           | Iir_Kind_Unit_Declaration =>
            null;

         when Iir_Kinds_Array_Attribute =>
            -- No need to canon parameter, since it is a locally static
            -- expression.
            declare
               Prefix : constant Iir := Get_Prefix (Expr);
            begin
               if Get_Kind (Prefix) in Iir_Kinds_Denoting_Name
                 and then (Get_Kind (Get_Named_Entity (Prefix))
                             in Iir_Kinds_Type_Declaration)
               then
                  --  No canon for types.
                  null;
               else
                  Canon_Expression (Prefix);
               end if;
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

         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration =>
            null;

         when Iir_Kind_Enumeration_Literal
           | Iir_Kind_Overflow_Literal =>
            null;

         when Iir_Kind_Element_Declaration =>
            null;

         when Iir_Kind_Attribute_Value
           | Iir_Kind_Attribute_Name =>
            null;

         when others =>
            Error_Kind ("canon_expression", Expr);
            null;
      end case;
   end Canon_Expression;

   procedure Canon_Expression_If_Valid (Expr : Iir) is
   begin
      if Is_Valid (Expr) then
         Canon_Expression (Expr);
      end if;
   end Canon_Expression_If_Valid;

   procedure Canon_PSL_Expression (Expr : PSL_Node)
   is
      use PSL.Nodes;
   begin
      case Get_Kind (Expr) is
         when N_HDL_Expr
           | N_HDL_Bool =>
            Canon_Expression (Get_HDL_Node (Expr));
         when N_True | N_EOS =>
            null;
         when N_Not_Bool =>
            Canon_PSL_Expression (Get_Boolean (Expr));
         when N_And_Bool
           | N_Or_Bool =>
            Canon_PSL_Expression (Get_Left (Expr));
            Canon_PSL_Expression (Get_Right (Expr));
         when others =>
            Error_Kind ("canon_psl_expression", Expr);
      end case;
   end Canon_PSL_Expression;

   procedure Canon_Discrete_Range (Rng : Iir) is
   begin
      case Get_Kind (Rng) is
         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            Canon_Expression (Get_Range_Constraint (Rng));
         when Iir_Kind_Enumeration_Type_Definition =>
            null;
         when others =>
            Error_Kind ("canon_discrete_range", Rng);
      end case;
   end Canon_Discrete_Range;

   --  Extract sensitivity of WAVEFORM.
   procedure Extract_Waveform_Sensitivity
     (Waveform : Iir; Sensitivity_List: Iir_List)
   is
      We : Iir_Waveform_Element;
   begin
      We := Waveform;
      while We /= Null_Iir loop
         Canon_Extract_Sensitivity_Expression
           (Get_We_Value (We), Sensitivity_List, False);
         We := Get_Chain (We);
      end loop;
   end Extract_Waveform_Sensitivity;

   --  Canon expression of WAVEFORM.
   procedure Canon_Waveform_Expression (Waveform : Iir)
   is
      We : Iir_Waveform_Element;
   begin
      if Get_Kind (Waveform) = Iir_Kind_Unaffected_Waveform then
         pragma Assert (Get_Chain (Waveform) = Null_Iir);
         return;
      end if;

      We := Waveform;
      while We /= Null_Iir loop
         Canon_Expression (Get_We_Value (We));
         if Get_Time (We) /= Null_Iir then
            Canon_Expression (Get_Time (We));
         end if;
         We := Get_Chain (We);
      end loop;
   end Canon_Waveform_Expression;

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
      Formal : Iir;
      Assoc_Chain : Iir;

      Found : Boolean;
   begin
      if not Canon_Flag_Associations then
         return Association_Chain;
      end if;

      --  No argument, so return now.
      if Interface_Chain = Null_Iir then
         pragma Assert (Association_Chain = Null_Iir);
         return Null_Iir;
      end if;

      Chain_Init (N_Chain, Last);
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

            Formal := Get_Formal (Assoc_El);
            if Formal  = Null_Iir then
               Formal := Inter;
               if Canon_Flag_Set_Assoc_Formals then
                  Set_Formal (Assoc_El, Inter);
               end if;
            else
               Formal := Get_Interface_Of_Formal (Formal);
            end if;

            if Formal = Inter then

               --  Remove ASSOC_EL from ASSOC_CHAIN
               if Prev_Assoc_El /= Null_Iir then
                  Set_Chain (Prev_Assoc_El, Next_Assoc_El);
               else
                  Assoc_Chain := Next_Assoc_El;
               end if;

               --  Append ASSOC_EL in N_CHAIN.
               Set_Chain (Assoc_El, Null_Iir);
               Chain_Append (N_Chain, Last, Assoc_El);

               case Iir_Kinds_Association_Element (Get_Kind (Assoc_El)) is
                  when Iir_Kind_Association_Element_Open =>
                     goto Done;
                  when Iir_Kind_Association_Element_By_Expression
                     | Iir_Kind_Association_Element_By_Name =>
                     if Get_Whole_Association_Flag (Assoc_El) then
                        goto Done;
                     end if;
                  when Iir_Kind_Association_Element_By_Individual =>
                     Found := True;
                  when Iir_Kind_Association_Element_Package
                    | Iir_Kind_Association_Element_Type
                    | Iir_Kind_Association_Element_Subprogram
                    | Iir_Kind_Association_Element_Terminal =>
                     goto Done;
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
         Set_Whole_Association_Flag (Assoc_El, True);
         Location_Copy (Assoc_El, Loc);

         if Canon_Flag_Set_Assoc_Formals then
            Set_Formal (Assoc_El, Inter);
         end if;

         Chain_Append (N_Chain, Last, Assoc_El);

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
     (Interface_Chain: Iir; Association_Chain: Iir; Loc : Iir)
     return Iir
   is
      Res : Iir;
   begin
      Res := Canon_Association_Chain (Interface_Chain, Association_Chain, Loc);
      if Canon_Flag_Expressions then
         Canon_Association_Chain_Actuals (Res);
      end if;
      return Res;
   end Canon_Association_Chain_And_Actuals;

   procedure Canon_Subprogram_Call (Call : Iir)
   is
      Imp : constant Iir := Get_Implementation (Call);
      Inter_Chain : constant Iir := Get_Interface_Declaration_Chain (Imp);
      Assoc_Chain : Iir;
   begin
      Assoc_Chain := Get_Parameter_Association_Chain (Call);
      Assoc_Chain := Canon_Association_Chain (Inter_Chain, Assoc_Chain, Call);
      Set_Parameter_Association_Chain (Call, Assoc_Chain);
   end Canon_Subprogram_Call;

   procedure Canon_Subprogram_Call_And_Actuals (Call : Iir) is
   begin
      Canon_Subprogram_Call (Call);
      if Canon_Flag_Expressions then
         Canon_Association_Chain_Actuals
           (Get_Parameter_Association_Chain (Call));
      end if;
   end Canon_Subprogram_Call_And_Actuals;

   --  Create a default association list for INTERFACE_LIST.
   --  The default is a list of interfaces associated with open.
   function Canon_Default_Association_Chain (Interface_Chain : Iir)
     return Iir
   is
      Res : Iir;
      Last : Iir;
      Assoc, El : Iir;
   begin
      if not Canon_Flag_Associations then
         return Null_Iir;
      end if;

      El := Interface_Chain;
      Chain_Init (Res, Last);
      while El /= Null_Iir loop
         Assoc := Create_Iir (Iir_Kind_Association_Element_Open);
         Set_Whole_Association_Flag (Assoc, True);
         Set_Artificial_Flag (Assoc, True);
         if Canon_Flag_Set_Assoc_Formals then
            Set_Formal (Assoc, El);
         end if;
         Location_Copy (Assoc, El);
         Chain_Append (Res, Last, Assoc);
         El := Get_Chain (El);
      end loop;
      return Res;
   end Canon_Default_Association_Chain;

   function Canon_Conditional_Variable_Assignment_Statement (Stmt : Iir)
                                                            return Iir
   is
      Target : constant Iir := Get_Target (Stmt);
      Cond_Expr : Iir;
      Expr : Iir;
      Asgn : Iir;
      Res : Iir;
      El, N_El : Iir;
   begin
      Cond_Expr := Get_Conditional_Expression_Chain (Stmt);
      Res := Create_Iir (Iir_Kind_If_Statement);
      Set_Label (Res, Get_Label (Stmt));
      Set_Suspend_Flag (Res, False);
      El := Res;

      loop
         --  Fill if/elsif statement.
         Set_Parent (El, Get_Parent (Stmt));
         Location_Copy (El, Cond_Expr);
         Set_Condition (El, Get_Condition (Cond_Expr));

         --  Create simple variable assignment.
         Asgn := Create_Iir (Iir_Kind_Variable_Assignment_Statement);
         Location_Copy (Asgn, Cond_Expr);
         Set_Parent (Asgn, Res);
         Set_Target (Asgn, Target);
         Expr := Get_Expression (Cond_Expr);
         if Canon_Flag_Expressions then
            Canon_Expression (Expr);
         end if;
         Set_Expression (Asgn, Expr);

         Set_Sequential_Statement_Chain (El, Asgn);

         --  Next condition.
         Cond_Expr := Get_Chain (Cond_Expr);
         exit when Cond_Expr = Null_Iir;

         N_El := Create_Iir (Iir_Kind_Elsif);
         Set_Else_Clause (El, N_El);
         El := N_El;
      end loop;

      return Res;
   end Canon_Conditional_Variable_Assignment_Statement;

   function Canon_Conditional_Signal_Assignment_Statement (Stmt : Iir)
                                                         return Iir is
   begin
      return Canon_Conditional_Signal_Assignment
        (Stmt, Null_Iir, Get_Parent (Stmt), False);
   end Canon_Conditional_Signal_Assignment_Statement;

   --  Inner loop if any; used to canonicalize exit/next statement.
   Cur_Loop : Iir;

   function Canon_Sequential_Stmts (First : Iir) return Iir
   is
      Stmt: Iir;
      N_Stmt : Iir;
      Res, Last : Iir;
   begin
      Chain_Init (Res, Last);

      Stmt := First;
      while Stmt /= Null_Iir loop

         --  Keep the same statement by default.
         N_Stmt := Stmt;

         case Iir_Kinds_Sequential_Statement_Ext (Get_Kind (Stmt)) is
            when Iir_Kind_If_Statement =>
               declare
                  Cond: Iir;
                  Clause: Iir;
                  Stmts : Iir;
               begin
                  Clause := Stmt;
                  while Clause /= Null_Iir loop
                     Cond := Get_Condition (Clause);
                     Canon_Expression_If_Valid (Cond);
                     Stmts := Get_Sequential_Statement_Chain (Clause);
                     Stmts := Canon_Sequential_Stmts (Stmts);
                     Set_Sequential_Statement_Chain (Clause, Stmts);
                     Clause := Get_Else_Clause (Clause);
                  end loop;
               end;

            when Iir_Kind_Simple_Signal_Assignment_Statement =>
               Canon_Expression (Get_Target (Stmt));
               Canon_Waveform_Expression (Get_Waveform_Chain (Stmt));

            when Iir_Kind_Conditional_Signal_Assignment_Statement =>
               Canon_Conditional_Signal_Assignment_Expression (Stmt);
               N_Stmt := Canon_Conditional_Signal_Assignment_Statement (Stmt);

            when Iir_Kind_Variable_Assignment_Statement =>
               Canon_Expression (Get_Target (Stmt));
               Canon_Expression (Get_Expression (Stmt));

            when Iir_Kind_Conditional_Variable_Assignment_Statement =>
               N_Stmt :=
                 Canon_Conditional_Variable_Assignment_Statement (Stmt);

            when Iir_Kind_Wait_Statement =>
               declare
                  List : Iir_List;
                  Expr : Iir;
               begin
                  Canon_Expression_If_Valid (Get_Timeout_Clause (Stmt));
                  Expr := Get_Condition_Clause (Stmt);
                  Canon_Expression_If_Valid (Expr);
                  List := Get_Sensitivity_List (Stmt);
                  if List = Null_Iir_List and then Expr /= Null_Iir then
                     List := Create_Iir_List;
                     Canon_Extract_Sensitivity_Expression (Expr, List, False);
                     Set_Sensitivity_List (Stmt, List);
                  end if;
               end;

            when Iir_Kind_Case_Statement =>
               Canon_Expression (Get_Expression (Stmt));
               declare
                  Choice: Iir;
                  Stmts : Iir;
               begin
                  Choice := Get_Case_Statement_Alternative_Chain (Stmt);
                  while Choice /= Null_Iir loop
                     -- FIXME: canon choice expr.
                     Stmts := Get_Associated_Chain (Choice);
                     Stmts := Canon_Sequential_Stmts (Stmts);
                     Set_Associated_Chain (Choice, Stmts);
                     Choice := Get_Chain (Choice);
                  end loop;
               end;

            when Iir_Kind_Assertion_Statement
              | Iir_Kind_Report_Statement =>
               if Get_Kind (Stmt) = Iir_Kind_Assertion_Statement then
                  Canon_Expression (Get_Assertion_Condition (Stmt));
               end if;
               Canon_Expression_If_Valid (Get_Report_Expression (Stmt));
               Canon_Expression_If_Valid (Get_Severity_Expression (Stmt));

            when Iir_Kind_For_Loop_Statement =>
               declare
                  Prev_Loop : constant Iir := Cur_Loop;
                  Stmts : Iir;
               begin
                  -- FIXME: decl.
                  Cur_Loop := Stmt;
                  if Canon_Flag_Expressions then
                     Canon_Discrete_Range
                       (Get_Type (Get_Parameter_Specification (Stmt)));
                  end if;
                  Stmts := Get_Sequential_Statement_Chain (Stmt);
                  Stmts := Canon_Sequential_Stmts (Stmts);
                  Set_Sequential_Statement_Chain (Stmt, Stmts);
                  Cur_Loop := Prev_Loop;
               end;

            when Iir_Kind_While_Loop_Statement =>
               declare
                  Stmts : Iir;
                  Prev_Loop : Iir;
               begin
                  Canon_Expression_If_Valid (Get_Condition (Stmt));
                  Prev_Loop := Cur_Loop;
                  Cur_Loop := Stmt;
                  Stmts := Get_Sequential_Statement_Chain (Stmt);
                  Stmts := Canon_Sequential_Stmts (Stmts);
                  Set_Sequential_Statement_Chain (Stmt, Stmts);
                  Cur_Loop := Prev_Loop;
               end;

            when Iir_Kind_Next_Statement
              | Iir_Kind_Exit_Statement =>
               declare
                  Loop_Label : Iir;
               begin
                  Canon_Expression_If_Valid (Get_Condition (Stmt));
                  Loop_Label := Get_Loop_Label (Stmt);
                  if Loop_Label = Null_Iir then
                     Set_Loop_Label (Stmt, Build_Simple_Name (Cur_Loop, Stmt));
                  end if;
               end;

            when Iir_Kind_Procedure_Call_Statement =>
               Canon_Subprogram_Call_And_Actuals (Get_Procedure_Call (Stmt));

            when Iir_Kind_Null_Statement =>
               null;

            when Iir_Kind_Return_Statement =>
               Canon_Expression (Get_Expression (Stmt));

            when Iir_Kind_Selected_Waveform_Assignment_Statement
              | Iir_Kind_Signal_Force_Assignment_Statement
              | Iir_Kind_Signal_Release_Assignment_Statement
              | Iir_Kind_Break_Statement
              | Iir_Kind_Suspend_State_Statement =>
               Error_Kind ("canon_sequential_stmts", Stmt);
         end case;

         Chain_Append (Res, Last, N_Stmt);

         Stmt := Get_Chain (Stmt);
      end loop;

      return Res;
   end Canon_Sequential_Stmts;

   function Canon_Insert_Suspend_State_Statement (Stmt : Iir; Var : Iir)
                                                  return Iir
   is
      Last : Iir;
      Num : Int32;
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Suspend_State_Statement);
      Location_Copy (Res, Stmt);
      Set_Parent (Res, Get_Parent (Stmt));
      Set_Chain (Res, Stmt);

      Last := Get_Suspend_State_Chain (Var);
      if Last = Null_Iir then
         Num := 0;
      else
         Num := Get_Suspend_State_Index (Last);
      end if;

      Set_Suspend_State_Index (Res, Num + 1);
      Set_Suspend_State_Chain (Res, Last);
      Set_Suspend_State_Chain (Var, Res);
      return Res;
   end Canon_Insert_Suspend_State_Statement;

   function Canon_Add_Suspend_State_Statement (First : Iir; Var : Iir)
                                              return Iir
   is
      Stmt: Iir;
      S_Stmt : Iir;
      Res, Last : Iir;
   begin
      Chain_Init (Res, Last);

      Stmt := First;
      while Stmt /= Null_Iir loop

         S_Stmt := Null_Iir;

         case Get_Kind (Stmt) is
            when Iir_Kind_Simple_Signal_Assignment_Statement
               | Iir_Kind_Conditional_Signal_Assignment_Statement =>
               null;

            when Iir_Kind_Variable_Assignment_Statement
              | Iir_Kind_Conditional_Variable_Assignment_Statement =>
               null;

            when Iir_Kind_If_Statement =>
               if Get_Suspend_Flag (Stmt) then
                  declare
                     Clause: Iir;
                     Stmts : Iir;
                  begin
                     Clause := Stmt;
                     while Clause /= Null_Iir loop
                        Stmts := Get_Sequential_Statement_Chain (Clause);
                        Stmts := Canon_Add_Suspend_State_Statement
                          (Stmts, Var);
                        Set_Sequential_Statement_Chain (Clause, Stmts);
                        Clause := Get_Else_Clause (Clause);
                     end loop;
                  end;
               end if;

            when Iir_Kind_Wait_Statement =>
               S_Stmt := Canon_Insert_Suspend_State_Statement (Stmt, Var);

            when Iir_Kind_Case_Statement =>
               if Get_Suspend_Flag (Stmt) then
                  declare
                     Choice: Iir;
                     Stmts : Iir;
                  begin
                     Choice := Get_Case_Statement_Alternative_Chain (Stmt);
                     while Choice /= Null_Iir loop
                        -- FIXME: canon choice expr.
                        Stmts := Get_Associated_Chain (Choice);
                        Stmts := Canon_Add_Suspend_State_Statement
                          (Stmts, Var);
                        Set_Associated_Chain (Choice, Stmts);
                        Choice := Get_Chain (Choice);
                     end loop;
                  end;
               end if;

            when Iir_Kind_Assertion_Statement
              | Iir_Kind_Report_Statement =>
               null;

            when Iir_Kind_For_Loop_Statement
              | Iir_Kind_While_Loop_Statement =>
               if Get_Suspend_Flag (Stmt) then
                  declare
                     Stmts : Iir;
                  begin
                     Stmts := Get_Sequential_Statement_Chain (Stmt);
                     Stmts := Canon_Add_Suspend_State_Statement
                       (Stmts, Var);
                     Set_Sequential_Statement_Chain (Stmt, Stmts);
                  end;
               end if;

            when Iir_Kind_Next_Statement
              | Iir_Kind_Exit_Statement =>
               null;

            when Iir_Kind_Procedure_Call_Statement =>
               if Get_Suspend_Flag (Stmt) then
                  S_Stmt := Canon_Insert_Suspend_State_Statement (Stmt, Var);
               end if;

            when Iir_Kind_Null_Statement =>
               null;

            when Iir_Kind_Return_Statement =>
               null;

            when others =>
               Error_Kind ("canon_add_suspend_state_statement", Stmt);
         end case;

         if S_Stmt /= Null_Iir then
            Chain_Append (Res, Last, S_Stmt);
         end if;
         Chain_Append (Res, Last, Stmt);

         Stmt := Get_Chain (Stmt);
      end loop;

      return Res;
   end Canon_Add_Suspend_State_Statement;

   procedure Canon_Add_Suspend_State (Proc : Iir)
   is
      Var : Iir;
      Stmts : Iir;
   begin
      pragma Assert (Kind_In (Proc, Iir_Kind_Process_Statement,
                              Iir_Kind_Procedure_Body));

      --  Create suspend state variable.
      Var := Create_Iir (Iir_Kind_Suspend_State_Declaration);
      Set_Location (Var, Get_Location (Proc));
      Set_Parent (Var, Proc);

      --  Insert it.
      Set_Chain (Var, Get_Declaration_Chain (Proc));
      Set_Declaration_Chain (Proc, Var);

      --  Add suspend state statements.
      Stmts := Get_Sequential_Statement_Chain (Proc);
      Stmts := Canon_Add_Suspend_State_Statement (Stmts, Var);
      Set_Sequential_Statement_Chain (Proc, Stmts);
   end Canon_Add_Suspend_State;

   --  Create a statement transform from concurrent_signal_assignment
   --  statement STMT (either selected or conditional).
   --  waveform transformation is not done.
   --  PROC is the process created.
   --  PARENT is the place where signal assignment must be placed.  This may
   --   be PROC, or an 'if' statement if the assignment is guarded.
   --  See LRM93 9.5
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
      Set_Chain (Proc, Get_Chain (Stmt));
      Sensitivity_List := Create_Iir_List;
      Set_Sensitivity_List (Proc, Sensitivity_List);
      Set_Is_Ref (Proc, True);
      Set_Process_Origin (Proc, Stmt);

      --  LRM93 9.5
      --  1. If a label appears on the concurrent signal assignment, then the
      --     same label appears on the process statement.
      Set_Label (Proc, Get_Label (Stmt));

      --  LRM93 9.5
      --  2.  The equivalent process statement is a postponed process if and
      --      only if the current signal assignment statement includes the
      --      reserved word POSTPONED.
      Set_Postponed_Flag (Proc, Get_Postponed_Flag (Proc));

      Canon_Extract_Sensitivity_Expression
        (Get_Target (Stmt), Sensitivity_List, True);

      if Get_Guard (Stmt) /= Null_Iir then
         --  LRM93 9.1
         --  If the option guarded appears in the concurrent signal assignment
         --  statement, then the concurrent signal assignment is called a
         --  guarded assignment.
         --  If the concurrent signal assignement statement is a guarded
         --  assignment and the target of the concurrent signal assignment is
         --  a guarded target, then the statement transform is as follow:
         --    if GUARD then
         --       signal_transform
         --    else
         --       disconnect_statements
         --    end if;
         --  Otherwise, if the concurrent signal assignment statement is a
         --  guarded assignement, but the target if the concurrent signal
         --  assignment is not a guarded target, the then statement transform
         --  is as follows:
         --   if GUARD then signal_transform end if;
         If_Stmt := Create_Iir (Iir_Kind_If_Statement);
         Set_Parent (If_Stmt, Proc);
         Set_Sequential_Statement_Chain (Proc, If_Stmt);
         Location_Copy (If_Stmt, Stmt);
         Canon_Extract_Sensitivity_Expression
           (Get_Guard (Stmt), Sensitivity_List, False);
         Set_Condition (If_Stmt, Get_Guard (Stmt));
         Set_Is_Ref (If_Stmt, True);
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
               Dis_Stmt :=
                 Create_Iir (Iir_Kind_Simple_Signal_Assignment_Statement);
               Location_Copy (Dis_Stmt, Stmt);
               Set_Parent (Dis_Stmt, If_Stmt);
               Set_Target (Dis_Stmt, Target);
               Set_Is_Ref (Dis_Stmt, True);
               Set_Sequential_Statement_Chain (Else_Clause, Dis_Stmt);
               --  XX
               Set_Waveform_Chain (Dis_Stmt, Null_Iir);
            end if;
         end;
      else
         --  LRM93 9.1
         --  Finally, if the concurrent signal assignment statement is not a
         --  guarded assignment, and the traget of the concurrent signal
         --  assignment is not a guarded target, then the statement transform
         --  is as follows:
         --     signal_transform
         Chain := Proc;
      end if;
   end Canon_Concurrent_Signal_Assignment;

   function Canon_Concurrent_Procedure_Call (Conc_Stmt : Iir)
     return Iir_Sensitized_Process_Statement
   is
      Call : constant Iir_Procedure_Call := Get_Procedure_Call (Conc_Stmt);
      Imp : constant Iir := Get_Implementation (Call);
      Proc : Iir_Sensitized_Process_Statement;
      Call_Stmt : Iir_Procedure_Call_Statement;
      Wait_Stmt : Iir_Wait_Statement;
      Sensitivity_List : Iir_List;
      Is_Sensitized : Boolean;
   begin
      --  Optimization: the process is a sensitized process only if the
      --  procedure is known not to have wait statement.  This is possible only
      --  when generating code at once for the whole design, otherwise this
      --  may create discrepencies in translate structures due to states.
      Is_Sensitized :=
        (Get_Wait_State (Imp) = False) and Flags.Flag_Whole_Analyze;

      --  LRM93 9.3
      --  The equivalent process statement has also no sensitivity list, an
      --  empty declarative part, and a statement part that consists of a
      --  procedure call statement followed by a wait statement.
      if Is_Sensitized then
         Proc := Create_Iir (Iir_Kind_Sensitized_Process_Statement);
      else
         Proc := Create_Iir (Iir_Kind_Process_Statement);
      end if;
      Location_Copy (Proc, Conc_Stmt);
      Set_Parent (Proc, Get_Parent (Conc_Stmt));
      Set_Chain (Proc, Get_Chain (Conc_Stmt));
      Set_Process_Origin (Proc, Conc_Stmt);
      Set_Procedure_Call (Conc_Stmt, Null_Iir);

      --  LRM93 9.3
      --  The equivalent process statement has a label if and only if the
      --  concurrent procedure call statement has a label; if the equivalent
      --  process statement has a label, it is the same as that of the
      --  concurrent procedure call statement.
      Set_Label (Proc, Get_Label (Conc_Stmt));

      --  LRM93 9.3
      --  The equivalent process statement is a postponed process if and only
      --  if the concurrent procedure call statement includes the reserved
      --  word POSTPONED.
      Set_Postponed_Flag (Proc, Get_Postponed_Flag (Conc_Stmt));

      Call_Stmt := Create_Iir (Iir_Kind_Procedure_Call_Statement);
      Set_Sequential_Statement_Chain (Proc, Call_Stmt);
      Location_Copy (Call_Stmt, Conc_Stmt);
      Set_Parent (Call_Stmt, Proc);
      Set_Procedure_Call (Call_Stmt, Call);

      --  LRM93 9.3
      --  If there exists a name that denotes a signal in the actual part of
      --  any association element in the concurrent procedure call statement,
      --  and that actual is associated with a formal parameter of mode IN or
      --  INOUT, then the equivalent process statement includes a final wait
      --  statement with a sensitivity clause that is constructed by taking
      --  the union of the sets constructed by applying th rule of Section 8.1
      --  to each actual part associated with a formal parameter.
      Sensitivity_List := Create_Iir_List;
      Canon_Extract_Sensitivity_Procedure_Call (Call, Sensitivity_List);
      if Is_Sensitized then
         Set_Sensitivity_List (Proc, Sensitivity_List);
         Set_Is_Ref (Proc, True);
      else
         Wait_Stmt := Create_Iir (Iir_Kind_Wait_Statement);
         Location_Copy (Wait_Stmt, Conc_Stmt);
         Set_Parent (Wait_Stmt, Proc);
         Set_Sensitivity_List (Wait_Stmt, Sensitivity_List);
         Set_Is_Ref (Wait_Stmt, True);
         Set_Chain (Call_Stmt, Wait_Stmt);
      end if;
      return Proc;
   end Canon_Concurrent_Procedure_Call;

   --  Return a statement from a waveform.
   function Canon_Wave_Transform (Orig_Stmt : Iir;
                                  Waveform_Chain : Iir_Waveform_Element;
                                  Proc : Iir;
                                  Is_First : Boolean)
                                 return Iir
   is
      Stmt : Iir;
      Sensitivity_List : Iir_List;
   begin
      if Get_Kind (Waveform_Chain) = Iir_Kind_Unaffected_Waveform then
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
         Stmt := Create_Iir (Iir_Kind_Simple_Signal_Assignment_Statement);
         Set_Target (Stmt, Get_Target (Orig_Stmt));
         if not Is_First then
            Set_Is_Ref (Stmt, True);
         end if;
         if Proc /= Null_Iir then
            Sensitivity_List := Get_Sensitivity_List (Proc);
            Extract_Waveform_Sensitivity (Waveform_Chain, Sensitivity_List);
         end if;
         Set_Waveform_Chain (Stmt, Waveform_Chain);
         Set_Delay_Mechanism (Stmt, Get_Delay_Mechanism (Orig_Stmt));
         Set_Reject_Time_Expression
           (Stmt, Get_Reject_Time_Expression (Orig_Stmt));
         Set_Reject_Time_Expression (Orig_Stmt, Null_Iir);
      end if;
      Location_Copy (Stmt, Orig_Stmt);
      return Stmt;
   end Canon_Wave_Transform;

   --  Create signal_transform for a concurrent simple signal assignment.
   procedure Canon_Concurrent_Simple_Signal_Assignment
     (Conc_Stmt : Iir; Proc : Iir; Parent : Iir)
   is
      Stmt : Iir;
   begin
      Stmt := Canon_Wave_Transform
        (Conc_Stmt, Get_Waveform_Chain (Conc_Stmt), Proc, True);
      Set_Waveform_Chain (Conc_Stmt, Null_Iir);
      Set_Target (Conc_Stmt, Null_Iir);
      Set_Parent (Stmt, Parent);
      Set_Sequential_Statement_Chain (Parent, Stmt);
   end Canon_Concurrent_Simple_Signal_Assignment;

   procedure Canon_Conditional_Signal_Assignment_Expression (Stmt : Iir)
   is
      Cond_Wf : Iir_Conditional_Waveform;
   begin
      Cond_Wf := Get_Conditional_Waveform_Chain (Stmt);
      while Cond_Wf /= Null_Iir loop
         Canon_Expression_If_Valid (Get_Condition (Cond_Wf));
         Canon_Waveform_Expression (Get_Waveform_Chain (Cond_Wf));

         Cond_Wf := Get_Chain (Cond_Wf);
      end loop;
   end Canon_Conditional_Signal_Assignment_Expression;

   --  Create signal_transform for a concurrent conditional signal assignment.
   function Canon_Conditional_Signal_Assignment
     (Conc_Stmt : Iir; Proc : Iir; Parent : Iir; Clear : Boolean) return Iir
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

         --  Canon waveform.
         Wf := Get_Waveform_Chain (Cond_Wf);
         Wf := Canon_Wave_Transform
           (Conc_Stmt, Wf, Proc, False); -- Cond_Wf = Cond_Wf_Chain);

         if Expr = Null_Iir and Cond_Wf = Cond_Wf_Chain then
            --  A conditional assignment that is in fact a simple one.  Usual
            --  case for concurrent signal assignment in vhdl 93.
            pragma Assert (Get_Chain (Cond_Wf) = Null_Iir);

            Set_Parent (Wf, Parent);
            Res1 := Wf;
            Stmt := Res1;
         else
            --  A real conditional signal assignment.

            --  Canon condition (if any).
            if Expr /= Null_Iir then
               if Proc /= Null_Iir then
                  Canon_Extract_Sensitivity_Expression
                    (Expr, Get_Sensitivity_List (Proc), False);
               end if;
            end if;
            if Stmt = Null_Iir then
               Res1 := Create_Iir (Iir_Kind_If_Statement);
               Set_Parent (Res1, Parent);
               Stmt := Res1;
            else
               Res1 := Create_Iir (Iir_Kind_Elsif);
               Set_Else_Clause (Last_Res, Res1);
            end if;
            Location_Copy (Res1, Cond_Wf);
            Set_Condition (Res1, Expr);
            Set_Sequential_Statement_Chain (Res1, Wf);
            Set_Parent (Wf, Stmt);
            Last_Res := Res1;
         end if;

         if Clear then
            Set_Condition (Cond_Wf, Null_Iir);
            Set_Waveform_Chain (Cond_Wf, Null_Iir);
         end if;

         Cond_Wf := Get_Chain (Cond_Wf);
      end loop;

      return Stmt;
   end Canon_Conditional_Signal_Assignment;

   --  Create signal_transform for a concurrent conditional signal assignment.
   procedure Canon_Concurrent_Conditional_Signal_Assignment
     (Conc_Stmt : Iir; Proc : Iir; Parent : Iir)
   is
      Stmt : Iir;
   begin
      Stmt := Canon_Conditional_Signal_Assignment
        (Conc_Stmt, Proc, Parent, True);
      Set_Sequential_Statement_Chain (Parent, Stmt);
   end Canon_Concurrent_Conditional_Signal_Assignment;

   procedure Canon_Selected_Signal_Assignment_Expression (Stmt : Iir)
   is
      Selected_Waveform : Iir;
      Waveform : Iir;
   begin
      Canon_Expression (Get_Expression (Stmt));

      Selected_Waveform := Get_Selected_Waveform_Chain (Stmt);
      while Selected_Waveform /= Null_Iir loop
         Waveform := Get_Associated_Chain (Selected_Waveform);
         if Waveform /= Null_Iir then
            Canon_Waveform_Expression (Waveform);
         end if;
         Selected_Waveform := Get_Chain (Selected_Waveform);
      end loop;
   end Canon_Selected_Signal_Assignment_Expression;

   procedure Canon_Concurrent_Selected_Signal_Assignment
     (Conc_Stmt : Iir; Proc : Iir; Parent : Iir)
   is
      Sensitivity_List : constant Iir_List := Get_Sensitivity_List (Proc);
      Expr : constant Iir := Get_Expression (Conc_Stmt);
      Selected_Waveform_Chain : constant Iir :=
        Get_Selected_Waveform_Chain (Conc_Stmt);
      Target : constant Iir := Get_Target (Conc_Stmt);
      Reject_Time : constant Iir := Get_Reject_Time_Expression (Conc_Stmt);
      Selected_Waveform : Iir;
      Case_Stmt: Iir_Case_Statement;
      Stmt : Iir;
      Waveform : Iir;
   begin
      Canon_Extract_Sensitivity_Expression (Expr, Sensitivity_List, False);

      if Vhdl_Std < Vhdl_08 then
         Case_Stmt := Create_Iir (Iir_Kind_Case_Statement);
         Set_Parent (Case_Stmt, Parent);
         Set_Sequential_Statement_Chain (Parent, Case_Stmt);
         Location_Copy (Case_Stmt, Conc_Stmt);

         Set_Expression (Case_Stmt, Expr);

         Set_Case_Statement_Alternative_Chain
           (Case_Stmt, Selected_Waveform_Chain);

         Selected_Waveform := Selected_Waveform_Chain;
         while Selected_Waveform /= Null_Iir loop
            Set_Parent (Selected_Waveform, Case_Stmt);
            Waveform := Get_Associated_Chain (Selected_Waveform);
            if Waveform /= Null_Iir then
               Stmt := Canon_Wave_Transform
                 (Conc_Stmt, Waveform, Proc,
                  Selected_Waveform = Selected_Waveform_Chain);
               Set_Parent (Stmt, Case_Stmt);
               Set_Associated_Chain (Selected_Waveform, Stmt);
            end if;
            Selected_Waveform := Get_Chain (Selected_Waveform);
         end loop;
      else
         Stmt := Create_Iir (Iir_Kind_Selected_Waveform_Assignment_Statement);
         Set_Parent (Stmt, Parent);
         Set_Sequential_Statement_Chain (Parent, Stmt);
         Location_Copy (Stmt, Conc_Stmt);

         Set_Expression (Stmt, Expr);

         Set_Target (Stmt, Target);
         Set_Delay_Mechanism (Stmt, Get_Delay_Mechanism (Conc_Stmt));
         Set_Reject_Time_Expression (Stmt, Reject_Time);

         Set_Selected_Waveform_Chain (Stmt, Selected_Waveform_Chain);
         Set_Selected_Waveform_Chain (Conc_Stmt, Null_Iir);
         Selected_Waveform := Selected_Waveform_Chain;
         while Selected_Waveform /= Null_Iir loop
            Waveform := Get_Associated_Chain (Selected_Waveform);
            Set_Parent (Selected_Waveform, Stmt);
            if Waveform /= Null_Iir then
               Extract_Waveform_Sensitivity (Waveform, Sensitivity_List);
            end if;
            Selected_Waveform := Get_Chain (Selected_Waveform);
         end loop;
      end if;

      --  Transfer ownership.
      Set_Expression (Conc_Stmt, Null_Iir);
      Set_Target (Conc_Stmt, Null_Iir);
      Set_Selected_Waveform_Chain (Conc_Stmt, Null_Iir);
      Set_Reject_Time_Expression (Conc_Stmt, Null_Iir);
   end Canon_Concurrent_Selected_Signal_Assignment;

   procedure Canon_Generate_Statement_Body
     (Top : Iir_Design_Unit; Bod : Iir) is
   begin
      Canon_Declarations (Top, Bod, Bod);
      Canon_Concurrent_Stmts (Top, Bod);
   end Canon_Generate_Statement_Body;

   --  Return TRUE iff NFA has an edge with an EOS.
   --  If so, we need to create a finalizer.
   function Psl_Need_Finalizer (Nfa : PSL_NFA) return Boolean
   is
      use PSL.NFAs;
      S : NFA_State;
      E : NFA_Edge;
   begin
      S := Get_Final_State (Nfa);
      E := Get_First_Dest_Edge (S);
      while E /= No_Edge loop
         if PSL.NFAs.Utils.Has_EOS (Get_Edge_Expr (E)) then
            return True;
         end if;
         E := Get_Next_Dest_Edge (E);
      end loop;
      return False;
   end Psl_Need_Finalizer;

   --  Size the NFA and extract clock sensitivity.
   procedure Canon_Psl_Clocked_NFA (Stmt : Iir)
   is
      Fa : constant PSL_NFA := Get_PSL_NFA (Stmt);
      Num : Natural;
      List : Iir_List;
   begin
      PSL.NFAs.Labelize_States (Fa, Num);
      Set_PSL_Nbr_States (Stmt, Int32 (Num));

      Set_PSL_EOS_Flag (Stmt, Psl_Need_Finalizer (Fa));

      List := Create_Iir_List;
      Canon_PSL.Canon_Extract_Sensitivity (Get_PSL_Clock (Stmt), List);
      Set_PSL_Clock_Sensitivity (Stmt, List);
   end Canon_Psl_Clocked_NFA;

   procedure Canon_Psl_Property_Directive (Stmt : Iir)
   is
      use PSL.Nodes;
      use PSL.NFAs;
      Prop : PSL_Node;
      Fa : PSL_NFA;
      Final : NFA_State;
   begin
      --  Rewrite (simplify) the property.
      Prop := Get_Psl_Property (Stmt);
      Prop := PSL.Rewrites.Rewrite_Property (Prop);
      Set_Psl_Property (Stmt, Prop);

      --  Generate the NFA.
      case Get_Kind (Prop) is
         when N_Async_Abort
            | N_Sync_Abort
            | N_Abort =>
            Prop := Get_Property (Prop);
            Set_PSL_Abort_Flag (Stmt, True);
         when others =>
            null;
      end case;
      Fa := PSL.Build.Build_FA (Prop);
      Set_PSL_NFA (Stmt, Fa);

      Final := Get_Final_State (Fa);
      if Get_First_Dest_Edge (Final) = No_Edge then
         Warning_Msg_Sem (Warnid_Useless, +Stmt, "property cannot fail");
      end if;

      Canon_Psl_Clocked_NFA (Stmt);
      if Canon_Flag_Expressions then
         Canon_PSL_Expression (Get_PSL_Clock (Stmt));
      end if;
   end Canon_Psl_Property_Directive;

   procedure Canon_Psl_Sequence_Directive (Stmt : Iir)
   is
      Seq : PSL_Node;
      Fa : PSL_NFA;
   begin
      Seq := Get_Psl_Sequence (Stmt);
      Seq := PSL.Rewrites.Rewrite_SERE (Seq);
      Set_Psl_Sequence (Stmt, Seq);

      --  Generate the NFA.
      Fa := PSL.Build.Build_SERE_FA (Seq);

      --  IEEE1850-2005 PSL 7.1.6
      --  cover {r} is semantically equivalent to cover {[*]; r}.  That is,
      --  there is an implicit [*] starting the sequence.
      if Get_Kind (Stmt) = Iir_Kind_Psl_Cover_Directive then
         PSL.NFAs.Utils.Set_Init_Loop (Fa);
      end if;
      Set_PSL_NFA (Stmt, Fa);

      Canon_Psl_Clocked_NFA (Stmt);
      if Canon_Flag_Expressions then
         Canon_PSL_Expression (Get_PSL_Clock (Stmt));
      end if;
   end Canon_Psl_Sequence_Directive;

   procedure Canon_Psl_Assert_Directive (Stmt : Iir) is
   begin
      Canon_Psl_Property_Directive (Stmt);
      if Canon_Flag_Expressions then
         Canon_Expression (Get_Report_Expression (Stmt));
      end if;
   end Canon_Psl_Assert_Directive;

   procedure Canon_Psl_Cover_Directive (Stmt : Iir) is
   begin
      Canon_Psl_Sequence_Directive (Stmt);
      if Canon_Flag_Expressions then
         Canon_Expression (Get_Report_Expression (Stmt));
      end if;
   end Canon_Psl_Cover_Directive;

   procedure Canon_If_Case_Generate_Statement_Body
     (Bod : Iir; Alt_Num : in out Natural; Top : Iir_Design_Unit) is
   begin
      if Canon_Flag_Add_Labels
        and then Get_Alternative_Label (Bod) = Null_Identifier
      then
         declare
            Str : String := Natural'Image (Alt_Num);
         begin
            --  Note: the label starts with a capitalized
            --  letter, to avoid any clash with user's
            --  identifiers.
            Str (1) := 'B';
            Set_Alternative_Label (Bod, Name_Table.Get_Identifier (Str));
         end;
      end if;

      Canon_Generate_Statement_Body (Top, Bod);
      Alt_Num := Alt_Num + 1;
   end Canon_If_Case_Generate_Statement_Body;

   function Canon_Concurrent_Assertion_Statement (Stmt : Iir) return Iir
   is
      Proc : Iir;
      Asrt : Iir;
      Expr : Iir;
      Sensitivity_List : Iir_List;
   begin
      -- Create a new entry.
      Proc := Create_Iir (Iir_Kind_Sensitized_Process_Statement);
      Location_Copy (Proc, Stmt);
      Set_Parent (Proc, Get_Parent (Stmt));
      Set_Chain (Proc, Get_Chain (Stmt));
      Set_Process_Origin (Proc, Stmt);

      --  LRM93 9.4
      --  The equivalent process statement has a label if and only if the
      --  current assertion statement has a label; if the equivalent process
      --  statement has a label; it is the same as that of the concurrent
      --  assertion statement.
      Set_Label (Proc, Get_Label (Stmt));

      --  LRM93 9.4
      --  The equivalent process statement is a postponed process if and only
      --  if the current assertion statement includes the reserved word
      --  POSTPONED.
      Set_Postponed_Flag (Proc, Get_Postponed_Flag (Stmt));

      Asrt := Create_Iir (Iir_Kind_Assertion_Statement);
      Set_Sequential_Statement_Chain (Proc, Asrt);
      Set_Parent (Asrt, Proc);
      Location_Copy (Asrt, Stmt);
      Sensitivity_List := Create_Iir_List;
      Set_Sensitivity_List (Proc, Sensitivity_List);
      Set_Is_Ref (Proc, True);

      -- Expand the expression, fill the sensitivity list,
      Expr := Get_Assertion_Condition (Stmt);
      Canon_Extract_Sensitivity_Expression (Expr, Sensitivity_List, False);
      Set_Assertion_Condition (Asrt, Expr);
      Set_Assertion_Condition (Stmt, Null_Iir);

      Expr := Get_Report_Expression (Stmt);
      Set_Report_Expression (Asrt, Expr);
      Set_Report_Expression (Stmt, Null_Iir);

      Expr := Get_Severity_Expression (Stmt);
      Set_Severity_Expression (Asrt, Expr);
      Set_Severity_Expression (Stmt, Null_Iir);

      return Proc;
   end Canon_Concurrent_Assertion_Statement;

   function Canon_Concurrent_Break_Statement (Stmt : Iir) return Iir
   is
      Proc : Iir;
      Brk : Iir;
      Sensitivity_List : Iir_List;
      Cond : Iir;
   begin
      -- Create a new entry.
      Proc := Create_Iir (Iir_Kind_Sensitized_Process_Statement);
      Location_Copy (Proc, Stmt);
      Set_Parent (Proc, Get_Parent (Stmt));
      Set_Chain (Proc, Get_Chain (Stmt));
      Set_Process_Origin (Proc, Stmt);

      --  AMS-LRM17 11.9 Concurrent break statement
      --  The equivalent process statement has a label if and only if the
      --  concurrent break statement has a label; if the equivalent process
      --  statement has a label, it is the same as that of the concurrent
      --  break statement.
      Set_Label (Proc, Get_Label (Stmt));

      --  AMS-LRM17 11.9 Concurrent break statement
      --  The equivalent process statement does not include the reserved word
      --  postponed, [...]
      Set_Postponed_Flag (Proc, False);

      Brk := Create_Iir (Iir_Kind_Break_Statement);
      Set_Sequential_Statement_Chain (Proc, Brk);
      Set_Parent (Brk, Proc);
      Location_Copy (Brk, Stmt);

      Cond := Get_Condition (Stmt);
      Set_Break_Element (Brk, Get_Break_Element (Stmt));
      Set_Break_Element (Stmt, Null_Iir);
      Set_Condition (Brk, Cond);
      Set_Condition (Stmt, Null_Iir);

      --  AMS-LRM17 11.9 Concurrent break statement
      --  If the concurrent break statement has a sensitivity clause, then
      --  the wait statement of the equivalent process statement contains the
      --  same sensitivity clause; otherwise, if a name that denotes a signal
      --  appears in the Boolean expression that defines the condition of the
      --  break, then the wait statement includes a sensitivity clause that is
      --  constructed by applying the rule of 10.2 to that expression;
      --  otherwise the wait statement contains no sensitivity clause.  The
      --  wait statement does not contain a condition clause of a timeout
      --  clause.
      Sensitivity_List := Get_Sensitivity_List (Stmt);
      if Sensitivity_List = Null_Iir_List and then Cond /= Null_Iir then
         Sensitivity_List := Create_Iir_List;
         Canon_Extract_Sensitivity_Expression (Cond, Sensitivity_List, False);
      end if;
      Set_Sensitivity_List (Proc, Sensitivity_List);
      Set_Is_Ref (Proc, True);

      return Proc;
   end Canon_Concurrent_Break_Statement;

   procedure Canon_Concurrent_Label (Stmt : Iir; Proc_Num : in out Natural) is
   begin
      --  Add a label if required.
      if Canon_Flag_Add_Labels then
         case Get_Kind (Stmt) is
            when Iir_Kind_Psl_Declaration
              | Iir_Kind_Psl_Default_Clock
              | Iir_Kind_Psl_Endpoint_Declaration =>
               null;
            when others =>
               if Get_Label (Stmt) = Null_Identifier then
                  declare
                     Str : String := Natural'Image (Proc_Num);
                  begin
                     --  Note: the label starts with a capitalized letter,
                     --  to avoid any clash with user's identifiers.
                     Str (1) := 'P';
                     Set_Label (Stmt, Name_Table.Get_Identifier (Str));
                  end;
                  Proc_Num := Proc_Num + 1;
               end if;
         end case;
      end if;
   end Canon_Concurrent_Label;

   procedure Canon_Concurrent_Statement
     (Stmt : in out Iir; Top : Iir_Design_Unit)
   is
      Sub_Chain : Iir;
      Proc : Iir;
   begin
      case Get_Kind (Stmt) is
         when Iir_Kind_Concurrent_Simple_Signal_Assignment =>
            if Canon_Flag_Expressions then
               Canon_Expression (Get_Target (Stmt));
               Canon_Waveform_Expression (Get_Waveform_Chain (Stmt));
            end if;

            if Canon_Flag_Concurrent_Stmts then
               Canon_Concurrent_Signal_Assignment (Stmt, Proc, Sub_Chain);
               Canon_Concurrent_Simple_Signal_Assignment
                 (Stmt, Proc, Sub_Chain);
               Stmt := Proc;
            end if;

         when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
            if Canon_Flag_Expressions then
               Canon_Expression (Get_Target (Stmt));
               Canon_Conditional_Signal_Assignment_Expression (Stmt);
            end if;

            if Canon_Flag_Concurrent_Stmts then
               Canon_Concurrent_Signal_Assignment (Stmt, Proc, Sub_Chain);
               Canon_Concurrent_Conditional_Signal_Assignment
                 (Stmt, Proc, Sub_Chain);
               Stmt := Proc;
            end if;

         when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
            if Canon_Flag_Expressions then
               Canon_Expression (Get_Target (Stmt));
               Canon_Selected_Signal_Assignment_Expression (Stmt);
            end if;

            if Canon_Flag_Concurrent_Stmts then
               Canon_Concurrent_Signal_Assignment (Stmt, Proc, Sub_Chain);
               Canon_Concurrent_Selected_Signal_Assignment
                 (Stmt, Proc, Sub_Chain);
               Stmt := Proc;
            end if;

         when Iir_Kind_Concurrent_Assertion_Statement =>
            if Canon_Flag_Expressions then
               Canon_Expression (Get_Assertion_Condition (Stmt));
               Canon_Expression_If_Valid (Get_Report_Expression (Stmt));
               Canon_Expression_If_Valid (Get_Severity_Expression (Stmt));
            end if;

            if Canon_Flag_Concurrent_Stmts then
               Stmt := Canon_Concurrent_Assertion_Statement (Stmt);
            end if;

         when Iir_Kind_Concurrent_Break_Statement =>
            if Canon_Flag_Expressions then
               Canon_Expression_If_Valid (Get_Condition (Stmt));
            end if;
            if Canon_Flag_Concurrent_Stmts then
               Stmt := Canon_Concurrent_Break_Statement (Stmt);
            end if;

         when Iir_Kind_Concurrent_Procedure_Call_Statement =>
            declare
               Call : constant Iir_Procedure_Call :=
                 Get_Procedure_Call (Stmt);
               Imp : constant Iir := Get_Implementation (Call);
               Assoc_Chain : Iir;
            begin
               Assoc_Chain := Canon_Association_Chain_And_Actuals
                 (Get_Interface_Declaration_Chain (Imp),
                  Get_Parameter_Association_Chain (Call),
                  Call);
               Set_Parameter_Association_Chain (Call, Assoc_Chain);
            end;

            if Canon_Flag_Concurrent_Stmts then
               Stmt := Canon_Concurrent_Procedure_Call (Stmt);
            end if;

         when Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement =>
            if Canon_Flag_Add_Suspend_State
              and then Get_Kind (Stmt) = Iir_Kind_Process_Statement
            then
               Canon_Add_Suspend_State (Stmt);
            end if;
            Canon_Declarations (Top, Stmt, Null_Iir);
            if Canon_Flag_Sequentials_Stmts then
               declare
                  Stmts : Iir;
               begin
                  Stmts := Get_Sequential_Statement_Chain (Stmt);
                  Stmts := Canon_Sequential_Stmts (Stmts);
                  Set_Sequential_Statement_Chain (Stmt, Stmts);
               end;
            end if;
            if Canon_Flag_All_Sensitivity
              and then Canon_Flag_Sequentials_Stmts
              and then Get_Kind (Stmt) = Iir_Kind_Sensitized_Process_Statement
              and then Get_Sensitivity_List (Stmt) = Iir_List_All
            then
               Set_Sensitivity_List
                 (Stmt, Canon_Extract_Sensitivity_Process (Stmt));
            end if;

         when Iir_Kind_Component_Instantiation_Statement =>
            declare
               Inst : Iir;
               Assoc_Chain : Iir;
            begin
               Inst := Get_Instantiated_Unit (Stmt);
               Inst := Get_Entity_From_Entity_Aspect (Inst);
               Assoc_Chain := Canon_Association_Chain_And_Actuals
                 (Get_Generic_Chain (Inst),
                  Get_Generic_Map_Aspect_Chain (Stmt),
                  Stmt);
               Set_Generic_Map_Aspect_Chain (Stmt, Assoc_Chain);

               Assoc_Chain := Canon_Association_Chain_And_Actuals
                 (Get_Port_Chain (Inst),
                  Get_Port_Map_Aspect_Chain (Stmt),
                  Stmt);
               Set_Port_Map_Aspect_Chain (Stmt, Assoc_Chain);
            end;

         when Iir_Kind_Block_Statement =>
            declare
               Header : constant Iir_Block_Header := Get_Block_Header (Stmt);
               Guard : constant Iir_Guard_Signal_Declaration :=
                 Get_Guard_Decl (Stmt);
               Chain : Iir;
               Expr : Iir;
            begin
               if Guard /= Null_Iir then
                  Expr := Get_Guard_Expression (Guard);
                  Set_Guard_Sensitivity_List (Guard, Create_Iir_List);
                  Canon_Extract_Sensitivity_Expression
                    (Expr, Get_Guard_Sensitivity_List (Guard), False);
                  if Canon_Flag_Expressions then
                     Canon_Expression (Stmt);
                  end if;
               end if;
               if Header /= Null_Iir then
                  --  Generics.
                  Chain := Get_Generic_Map_Aspect_Chain (Header);
                  if Chain /= Null_Iir then
                     Chain := Canon_Association_Chain_And_Actuals
                       (Get_Generic_Chain (Header), Chain, Chain);
                  else
                     Chain := Canon_Default_Association_Chain
                       (Get_Generic_Chain (Header));
                  end if;
                  Set_Generic_Map_Aspect_Chain (Header, Chain);

                  --  Ports.
                  Chain := Get_Port_Map_Aspect_Chain (Header);
                  if Chain /= Null_Iir then
                     Chain := Canon_Association_Chain_And_Actuals
                       (Get_Port_Chain (Header), Chain, Chain);
                  else
                     Chain := Canon_Default_Association_Chain
                       (Get_Port_Chain (Header));
                  end if;
                  Set_Port_Map_Aspect_Chain (Header, Chain);
               end if;
               Canon_Declarations (Top, Stmt, Stmt);
               Canon_Concurrent_Stmts (Top, Stmt);
            end;

         when Iir_Kind_If_Generate_Statement =>
            declare
               Clause : Iir;
               Alt_Num : Natural;
            begin
               Clause := Stmt;
               Alt_Num := 1;
               while Clause /= Null_Iir loop
                  if Canon_Flag_Expressions then
                     Canon_Expression_If_Valid (Get_Condition (Stmt));
                  end if;

                  Canon_If_Case_Generate_Statement_Body
                    (Get_Generate_Statement_Body (Clause), Alt_Num, Top);

                  Clause := Get_Generate_Else_Clause (Clause);
               end loop;
            end;

         when Iir_Kind_Case_Generate_Statement =>
            declare
               Alt : Iir;
               Alt_Num : Natural;
            begin
               Alt_Num := 1;
               if Canon_Flag_Expressions then
                  Canon_Expression (Get_Expression (Stmt));
               end if;
               Alt := Get_Case_Statement_Alternative_Chain (Stmt);
               while Alt /= Null_Iir loop
                  if not Get_Same_Alternative_Flag (Alt) then
                     Canon_If_Case_Generate_Statement_Body
                       (Get_Associated_Block (Alt), Alt_Num, Top);
                  end if;

                  Alt := Get_Chain (Alt);
               end loop;
            end;

         when Iir_Kind_For_Generate_Statement =>
            declare
               Decl : constant Iir := Get_Parameter_Specification (Stmt);
               New_Decl : Iir;
            begin
               New_Decl := Canon_Declaration (Top, Decl, Null_Iir);
               pragma Assert (New_Decl = Decl);

               Canon_Generate_Statement_Body
                 (Top, Get_Generate_Statement_Body (Stmt));
            end;

         when Iir_Kind_Psl_Assert_Directive =>
            Canon_Psl_Assert_Directive (Stmt);
         when Iir_Kind_Psl_Assume_Directive =>
            Canon_Psl_Property_Directive (Stmt);
         when Iir_Kind_Psl_Cover_Directive =>
            Canon_Psl_Cover_Directive (Stmt);
         when Iir_Kind_Psl_Restrict_Directive =>
            Canon_Psl_Sequence_Directive (Stmt);

         when Iir_Kind_Psl_Default_Clock =>
            null;
         when Iir_Kind_Psl_Declaration =>
            declare
               use PSL.Nodes;
               Decl : constant PSL_Node := Get_Psl_Declaration (Stmt);
               Prop : PSL_Node;
               Fa : PSL_NFA;
            begin
               case Get_Kind (Decl) is
                  when N_Property_Declaration =>
                     Prop := Get_Property (Decl);
                     Prop := PSL.Rewrites.Rewrite_Property (Prop);
                     Set_Property (Decl, Prop);
                     if Get_Parameter_List (Decl) = Null_PSL_Node then
                        --  Generate the NFA.
                        Fa := PSL.Build.Build_FA (Prop);
                        Set_PSL_NFA (Stmt, Fa);
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
         when Iir_Kind_Psl_Endpoint_Declaration =>
            declare
               use PSL.Nodes;
               Decl : constant PSL_Node := Get_Psl_Declaration (Stmt);
               Seq : PSL_Node;
               Fa : PSL_NFA;
            begin
               pragma Assert (Get_Parameter_List (Decl) = Null_PSL_Node);
               Seq := Get_Sequence (Decl);
               Seq := PSL.Rewrites.Rewrite_SERE (Seq);
               Set_Sequence (Decl, Seq);
               --  Generate the NFA.
               Fa := PSL.Build.Build_SERE_FA (Seq);
               Set_PSL_NFA (Stmt, Fa);
               Canon_Psl_Clocked_NFA (Stmt);
            end;

         when Iir_Kind_Simple_Simultaneous_Statement =>
            if Canon_Flag_Expressions then
               Canon_Expression (Get_Simultaneous_Left (Stmt));
               Canon_Expression (Get_Simultaneous_Right (Stmt));
            end if;
         when Iir_Kind_Simultaneous_If_Statement =>
            declare
               Clause : Iir;
            begin
               Clause := Stmt;
               while Clause /= Null_Iir loop
                  if Canon_Flag_Expressions then
                     Canon_Expression_If_Valid (Get_Condition (Clause));
                  end if;
                  Canon_Simultaneous_Stmts
                    (Top, Get_Simultaneous_Statement_Chain (Clause));
                  Clause := Get_Else_Clause (Clause);
               end loop;
            end;
         when Iir_Kind_Simultaneous_Case_Statement =>
            declare
               Alt : Iir;
            begin
               if Canon_Flag_Expressions then
                  Canon_Expression (Get_Expression (Stmt));
               end if;
               Alt := Get_Case_Statement_Alternative_Chain (Stmt);
               while Alt /= Null_Iir loop
                  if not Get_Same_Alternative_Flag (Alt) then
                     Canon_Simultaneous_Stmts
                       (Top, Get_Associated_Block (Alt));
                  end if;
                  Alt := Get_Chain (Alt);
               end loop;
            end;
         when Iir_Kind_Simultaneous_Procedural_Statement =>
            Canon_Declarations (Top, Stmt, Null_Iir);
            if Canon_Flag_Sequentials_Stmts then
               declare
                  Stmts : Iir;
               begin
                  Stmts := Get_Sequential_Statement_Chain (Stmt);
                  Stmts := Canon_Sequential_Stmts (Stmts);
                  Set_Sequential_Statement_Chain (Stmt, Stmts);
               end;
            end if;
         when Iir_Kind_Simultaneous_Null_Statement =>
            null;

         when others =>
            Error_Kind ("canon_concurrent_statement", Stmt);
      end case;
   end Canon_Concurrent_Statement;

   procedure Canon_Concurrent_Stmts (Top : Iir_Design_Unit; Parent : Iir)
   is
      --  Current element in the chain of concurrent statements.
      Stmt : Iir;
      Prev_Stmt : Iir;

      Proc_Num : Natural := 0;
   begin
      Prev_Stmt := Null_Iir;
      Stmt := Get_Concurrent_Statement_Chain (Parent);
      while Stmt /= Null_Iir loop
         Canon_Concurrent_Label (Stmt, Proc_Num);

         Canon_Concurrent_Statement (Stmt, Top);

         --  STMT may have been changed.
         if Prev_Stmt = Null_Iir then
            Set_Concurrent_Statement_Chain (Parent, Stmt);
         else
            Set_Chain (Prev_Stmt, Stmt);
         end if;
         Prev_Stmt := Stmt;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Canon_Concurrent_Stmts;

   procedure Canon_Simultaneous_Stmts (Top : Iir_Design_Unit; Chain : Iir)
   is
      Stmt : Iir;
      Prev_Stmt : Iir;
      Proc_Num : Natural := 0;
   begin
      Stmt := Chain;
      while Stmt /= Null_Iir loop
         Canon_Concurrent_Label (Stmt, Proc_Num);

         Prev_Stmt := Stmt;
         Canon_Concurrent_Statement (Stmt, Top);
         pragma Assert (Stmt = Prev_Stmt);

         Stmt := Get_Chain (Stmt);
      end loop;
   end Canon_Simultaneous_Stmts;

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
      Ent : Iir;
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
               Add_Dependence (Top, Aspect);
            else
               Ent := Get_Entity (Aspect);
               pragma Assert (Ent /= Null_Iir);
               if Get_Kind (Ent) = Iir_Kind_Entity_Declaration then
                  Ent := Get_Design_Unit (Ent);
               end if;
               Add_Dependence (Top, Ent);
            end if;
         when Iir_Kind_Entity_Aspect_Configuration =>
            Add_Dependence (Top, Get_Design_Unit (Get_Configuration (Aspect)));
         when Iir_Kind_Entity_Aspect_Open =>
            null;
         when others =>
            Error_Kind ("add_binding_indication_dependence", Aspect);
      end case;
   end Add_Binding_Indication_Dependence;

   --  Canon the component_configuration or configuration_specification CFG.
   --  TOP is used to add dependences.
   procedure Canon_Component_Configuration (Top : Iir_Design_Unit; Cfg : Iir)
   is
      --  True iff CFG is a component_configuration.
      --  False iff CFG is a configuration_specification.
      Is_Config : constant Boolean :=
        Get_Kind (Cfg) = Iir_Kind_Component_Configuration;

      Bind : Iir;
      Comp : Iir;
      Instances : Iir_Flist;
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
         --  Designator_all and designator_others must have been replaced
         --  by a list during canon.
         pragma Assert (Instances not in Iir_Flists_All_Others);
         Bind := Get_Default_Binding_Indication
           (Get_Named_Entity (Get_Nth_Element (Instances, 0)));
         if Bind = Null_Iir then
            --  Component is not bound.
            return;
         end if;
         Set_Binding_Indication (Cfg, Bind);
         Set_Is_Ref (Cfg, True);
         Add_Binding_Indication_Dependence (Top, Bind);
         if Is_Config then
            Entity_Aspect := Get_Entity_Aspect (Bind);
            Entity := Get_Entity_From_Entity_Aspect (Entity_Aspect);
            case Get_Kind (Entity) is
               when Iir_Kind_Entity_Declaration =>
                  Sem_Specs.Sem_Check_Missing_Generic_Association
                    (Get_Generic_Chain (Entity),
                     Get_Generic_Map_Aspect_Chain (Bind),
                     Null_Iir,
                     Cfg);
               when Iir_Kind_Foreign_Module =>
                  null;
               when others =>
                  raise Internal_Error;
            end case;
         end if;
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
            Comp := Get_Named_Entity (Get_Component_Name (Cfg));

            --  Canon generic map
            Map_Chain := Get_Generic_Map_Aspect_Chain (Bind);
            if Map_Chain = Null_Iir then
               if Is_Config and then Is_Valid (Entity) then
                  Map_Chain := Sem_Specs.Create_Default_Map_Aspect
                    (Comp, Entity, Sem_Specs.Map_Generic, Bind);
                  --  Check all non-associated generics have a default value.
                  Sem_Specs.Sem_Check_Missing_Generic_Association
                    (Get_Generic_Chain (Entity), Map_Chain, Null_Iir, Bind);
               end if;
            else
               Map_Chain := Canon_Association_Chain
                 (Get_Generic_Chain (Entity), Map_Chain, Map_Chain);
            end if;
            Set_Generic_Map_Aspect_Chain (Bind, Map_Chain);

            --  Canon port map
            Map_Chain := Get_Port_Map_Aspect_Chain (Bind);
            if Map_Chain = Null_Iir then
               if Is_Config and then Is_Valid (Entity) then
                  Map_Chain := Sem_Specs.Create_Default_Map_Aspect
                    (Comp, Entity, Sem_Specs.Map_Port, Bind);
               end if;
            else
               Map_Chain := Canon_Association_Chain
                 (Get_Port_Chain (Entity), Map_Chain, Map_Chain);
            end if;
            Set_Port_Map_Aspect_Chain (Bind, Map_Chain);

            if Is_Config then
               Block := Get_Block_Configuration (Cfg);
               if Block /= Null_Iir then
                  --  If there is no architecture_identifier in the binding,
                  --  set it from the block_configuration.
                  if Get_Kind (Entity_Aspect) = Iir_Kind_Entity_Aspect_Entity
                    and then Get_Architecture (Entity_Aspect) = Null_Iir
                  then
                     Entity := Get_Entity (Entity_Aspect);
                     pragma Assert
                       (Get_Kind (Entity) = Iir_Kind_Entity_Declaration);
                     Set_Architecture
                       (Entity_Aspect,
                        Build_Reference_Name
                          (Get_Block_Specification (Block)));
                  end if;
                  Canon_Block_Configuration (Top, Block);
               end if;
            end if;
         end if;
      end if;
   end Canon_Component_Configuration;

   --  Create the 'final' binding indication in case of incremental binding.
   procedure Canon_Incremental_Binding
     (Conf_Spec : Iir_Configuration_Specification;
      Comp_Conf : Iir_Component_Configuration;
      Parent : Iir)
   is
      --  Merge associations from FIRST_CHAIN and SEC_CHAIN.
      function Merge_Association_Chain
        (Inter_Chain : Iir; First_Chain : Iir; Sec_Chain : Iir) return Iir
      is
         --  Result (chain).
         First, Last : Iir;

         --  Copy an association and append new elements to FIRST/LAST.  In
         --  case of individual associations, all associations for the
         --  interface are copied.
         procedure Copy_Association
           (Assoc : in out Iir; Inter : in out Iir; Copy_Inter : Iir)
         is
            El : Iir;
            Formal : Iir;
         begin
            loop
               El := Create_Iir (Get_Kind (Assoc));
               Location_Copy (El, Assoc);

               --  Copy formal.
               --  Special case: formal comes from a default binding
               --  indication.  In that case Is_Forward_Ref is set, which makes
               --  it non-copiable by Sem_Inst.
               Formal := Get_Formal (Assoc);
               if Is_Valid (Formal) then
                  if Get_Kind (Formal) = Iir_Kind_Simple_Name
                    and then Get_Is_Forward_Ref (Formal)
                  then
                     Formal := Build_Simple_Name
                       (Get_Named_Entity (Formal), Formal);
                  else
                     Formal := Sem_Inst.Copy_Tree (Formal);
                  end if;
                  Set_Formal (El, Formal);
               else
                  Formal := Inter;
               end if;
               Set_Whole_Association_Flag
                 (El, Get_Whole_Association_Flag (Assoc));

               case Get_Kind (Assoc) is
                  when Iir_Kind_Association_Element_Open =>
                     null;
                  when Iir_Kind_Association_Element_By_Expression
                    | Iir_Kind_Association_Element_By_Name =>
                     Set_Actual (El, Sem_Inst.Copy_Tree (Get_Actual (Assoc)));
                     Set_Actual_Conversion
                       (El,
                        Sem_Inst.Copy_Tree (Get_Actual_Conversion (Assoc)));
                     Set_Formal_Conversion
                       (El,
                        Sem_Inst.Copy_Tree (Get_Formal_Conversion (Assoc)));
                     Set_Collapse_Signal_Flag
                       (Assoc,
                        Sem.Can_Collapse_Signals (Assoc, Formal));
                  when Iir_Kind_Association_Element_By_Individual =>
                     Set_Actual_Type (El, Get_Actual_Type (Assoc));
                  when others =>
                     Error_Kind ("copy_association", Assoc);
               end case;

               Chain_Append (First, Last, El);
               Next_Association_Interface (Assoc, Inter);
               exit when Assoc = Null_Iir;
               exit when
                 Get_Association_Interface (Assoc, Inter) /= Copy_Inter;
            end loop;
         end Copy_Association;

         procedure Advance
           (Assoc : in out Iir; Inter : in out Iir; Skip_Inter : Iir) is
         begin
            loop
               Next_Association_Interface (Assoc, Inter);
               exit when Assoc = Null_Iir;
               exit when
                 Get_Association_Interface (Assoc, Inter) /= Skip_Inter;
            end loop;
         end Advance;

         Inter : Iir;
         F_El : Iir;
         F_Inter : Iir;
         S_El : Iir;
         S_Inter : Iir;
      begin
         F_El := First_Chain;
         F_Inter := Inter_Chain;
         Chain_Init (First, Last);
         Inter := Inter_Chain;
         while Inter /= Null_Iir loop
            --  Consistency check.
            pragma Assert (Get_Association_Interface (F_El, F_Inter) = Inter);

            --  Find the association in the second chain.
            S_El := Find_First_Association_For_Interface
              (Sec_Chain, Inter_Chain, Inter);

            if S_El /= Null_Iir
              and then Get_Kind (S_El) /= Iir_Kind_Association_Element_Open
            then
               --  Exists and not open: use it.
               S_Inter := Inter;
               Copy_Association (S_El, S_Inter, Inter);
               Advance (F_El, F_Inter, Inter);
            else
               --  Does not exist: use the one from first chain.
               Copy_Association (F_El, F_Inter, Inter);
            end if;
            Inter := Get_Chain (Inter);
         end loop;
         return First;
      end Merge_Association_Chain;

      Comp_Name : constant Iir := Get_Component_Name (Conf_Spec);
      Comp : constant Iir := Get_Named_Entity (Comp_Name);
      Cs_Binding : constant Iir := Get_Binding_Indication (Conf_Spec);
      Cc_Binding : constant Iir := Get_Binding_Indication (Comp_Conf);
      Res : Iir_Component_Configuration;
      Cs_Chain : Iir;
      Res_Binding : Iir_Binding_Indication;
      Entity : Iir;
      Instance_List : Iir_List;
      Conf_Instance_List : Iir_Flist;
      Instance : Iir;
      Instance_Name : Iir;
      N_Nbr : Natural;
   begin
      --  Create the new component configuration
      Res := Create_Iir (Iir_Kind_Component_Configuration);
      Location_Copy (Res, Comp_Conf);
      Set_Parent (Res, Parent);
      Set_Component_Name (Res, Build_Reference_Name (Comp_Name));

      Res_Binding := Create_Iir (Iir_Kind_Binding_Indication);
      Location_Copy (Res_Binding, Res);
      Set_Binding_Indication (Res, Res_Binding);

      Entity := Get_Entity_From_Entity_Aspect (Get_Entity_Aspect (Cs_Binding));

      --  Merge generic map aspect.
      Cs_Chain := Get_Generic_Map_Aspect_Chain (Cs_Binding);
      if Cs_Chain = Null_Iir then
         Cs_Chain := Sem_Specs.Create_Default_Map_Aspect
           (Comp, Entity, Sem_Specs.Map_Generic, Cs_Binding);
      end if;
      Set_Generic_Map_Aspect_Chain
        (Res_Binding,
         Merge_Association_Chain (Get_Generic_Chain (Entity),
                                  Cs_Chain,
                                  Get_Generic_Map_Aspect_Chain (Cc_Binding)));

      --  Merge port map aspect.
      Cs_Chain := Get_Port_Map_Aspect_Chain (Cs_Binding);
      if Cs_Chain = Null_Iir then
         Cs_Chain := Sem_Specs.Create_Default_Map_Aspect
           (Comp, Entity, Sem_Specs.Map_Port, Cs_Binding);
      end if;
      Set_Port_Map_Aspect_Chain
        (Res_Binding,
         Merge_Association_Chain (Get_Port_Chain (Entity),
                                  Cs_Chain,
                                  Get_Port_Map_Aspect_Chain (Cc_Binding)));

      --  Set entity aspect.
      Set_Entity_Aspect
        (Res_Binding, Sem_Inst.Copy_Tree (Get_Entity_Aspect (Cs_Binding)));

      --  Create list of instances:
      --   * keep common instances
      --   replace component_configuration of them
      --   remove them in the instance list of COMP_CONF
      Instance_List := Create_Iir_List;
      Conf_Instance_List := Get_Instantiation_List (Comp_Conf);
      N_Nbr := 0;
      for I in Flist_First .. Flist_Last (Conf_Instance_List) loop
         Instance_Name := Get_Nth_Element (Conf_Instance_List, I);
         Instance := Get_Named_Entity (Instance_Name);
         if Get_Component_Configuration (Instance) = Conf_Spec then
            --  The incremental binding applies to this instance.
            Set_Component_Configuration (Instance, Res);
            Append_Element (Instance_List, Instance_Name);
         else
            Set_Nth_Element (Conf_Instance_List, N_Nbr, Instance_Name);
            N_Nbr := N_Nbr + 1;
         end if;
      end loop;
      Set_Instantiation_List (Comp_Conf,
                              Truncate_Flist (Conf_Instance_List, N_Nbr));
      Set_Instantiation_List (Res, List_To_Flist (Instance_List));

      --  Insert RES.
      Set_Chain (Res, Get_Chain (Comp_Conf));
      Set_Chain (Comp_Conf, Res);
   end Canon_Incremental_Binding;

   procedure Canon_Component_Specification_All_Others
     (Conf : Iir; Parent : Iir; Spec : Iir_Flist; List : Iir_List; Comp : Iir)
   is
      El : Iir;
      Comp_Conf : Iir;
      Inst : Iir;
   begin
      El := Get_Concurrent_Statement_Chain (Parent);
      while El /= Null_Iir loop
         --  Handle only component instantiation of COMP.
         if Get_Kind (El) = Iir_Kind_Component_Instantiation_Statement
           and then Is_Component_Instantiation (El)
           and then Get_Named_Entity (Get_Instantiated_Unit (El)) = Comp
         then
            Comp_Conf := Get_Component_Configuration (El);
            if Comp_Conf = Null_Iir then
               --  The component is not yet configured.
               Inst := Build_Simple_Name (El, El);
               Set_Is_Forward_Ref (Inst, True);
               Append_Element (List, Inst);
               Set_Component_Configuration (El, Conf);
            else
               --  The component is already configured.
               --  Handle incremental configuration.
               if Get_Kind (Comp_Conf) = Iir_Kind_Configuration_Specification
                 and then Spec = Iir_Flist_All
               then
                  --  FIXME: handle incremental configuration.
                  raise Internal_Error;
               end if;
               --  Several component configuration for an instance.
               --  Must have been caught by sem.
               pragma Assert (Spec = Iir_Flist_Others);
            end if;
         end if;
         El := Get_Chain (El);
      end loop;
   end Canon_Component_Specification_All_Others;

   procedure Canon_Component_Specification_List
     (Conf : Iir; Parent : Iir; Spec : Iir_Flist)
   is
      El : Iir;
      Comp_Conf : Iir;
   begin
      --  Already has a designator list.
      for I in Flist_First .. Flist_Last (Spec) loop
         El := Get_Nth_Element (Spec, I);
         El := Get_Named_Entity (El);
         Comp_Conf := Get_Component_Configuration (El);
         if Comp_Conf /= Null_Iir and then Comp_Conf /= Conf then
            pragma Assert
              (Get_Kind (Comp_Conf) = Iir_Kind_Configuration_Specification);
            pragma Assert
              (Get_Kind (Conf) = Iir_Kind_Component_Configuration);
            Canon_Incremental_Binding (Comp_Conf, Conf, Parent);
         else
            Set_Component_Configuration (El, Conf);
         end if;
      end loop;
   end Canon_Component_Specification_List;

   --  PARENT is the parent for the chain of concurrent statements.
   procedure Canon_Component_Specification (Conf : Iir; Parent : Iir)
   is
      Spec : constant Iir_Flist := Get_Instantiation_List (Conf);
      List : Iir_List;
   begin
      if Spec in Iir_Flists_All_Others then
         List := Create_Iir_List;
         Canon_Component_Specification_All_Others
           (Conf, Parent, Spec, List,
            Get_Named_Entity (Get_Component_Name (Conf)));
         Set_Instantiation_List (Conf, List_To_Flist (List));
      else
         --  Has Already a designator list.
         Canon_Component_Specification_List (Conf, Parent, Spec);
      end if;
   end Canon_Component_Specification;

   --  Replace ALL/OTHERS with the explicit list of signals.
   procedure Canon_Disconnection_Specification
     (Dis : Iir_Disconnection_Specification)
   is
      Signal_List : Iir_Flist;
      Force : Boolean;
      El : Iir;
      N_List : Iir_List;
      Dis_Type : Iir;
   begin
      if Canon_Flag_Expressions then
         Canon_Expression (Get_Expression (Dis));
      end if;

      if Canon_Flag_Specification_Lists then
         Signal_List := Get_Signal_List (Dis);
         if Signal_List = Iir_Flist_All then
            Force := True;
         elsif Signal_List = Iir_Flist_Others then
            Force := False;
         else
            --  User list: nothing to do.
            return;
         end if;

         Dis_Type := Get_Type (Get_Type_Mark (Dis));
         N_List := Create_Iir_List;
         Set_Is_Ref (Dis, True);
         El := Get_Declaration_Chain (Get_Parent (Dis));
         while El /= Null_Iir loop
            if Get_Kind (El) = Iir_Kind_Signal_Declaration
              and then Get_Type (El) = Dis_Type
              and then Get_Guarded_Signal_Flag (El)
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
         Set_Signal_List (Dis, List_To_Flist (N_List));
      end if;
   end Canon_Disconnection_Specification;

   --  Replace ALL/OTHERS with the explicit list of signals.
   procedure Canon_Step_Limit_Specification (Limit : Iir)
   is
      Quantity_List : Iir_Flist;
      Force : Boolean;
      El : Iir;
      N_List : Iir_List;
      Quan_Type : Iir;
   begin
      if Canon_Flag_Expressions then
         Canon_Expression (Get_Expression (Limit));
      end if;

      if Canon_Flag_Specification_Lists then
         Quantity_List := Get_Quantity_List (Limit);
         if Quantity_List = Iir_Flist_All then
            Force := True;
         elsif Quantity_List = Iir_Flist_Others then
            Force := False;
         else
            --  User list: nothing to do.
            return;
         end if;

         pragma Unreferenced (Force);

         Quan_Type := Get_Type (Get_Type_Mark (Limit));
         N_List := Create_Iir_List;
         Set_Is_Ref (Limit, True);
         El := Get_Declaration_Chain (Get_Parent (Limit));
         while El /= Null_Iir loop
            if Get_Kind (El) in Iir_Kinds_Quantity_Declaration
              and then Get_Type (El) = Quan_Type
            then
               raise Internal_Error;
            end if;
            El := Get_Chain (El);
         end loop;
         Set_Quantity_List (Limit, List_To_Flist (N_List));
      end if;
   end Canon_Step_Limit_Specification;

   procedure Canon_Subtype_Indication (Def : Iir) is
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Array_Subtype_Definition =>
            declare
               Indexes : constant Iir_Flist := Get_Index_Subtype_List (Def);
               Index : Iir;
            begin
               for I in Flist_First .. Flist_Last (Indexes) loop
                  Index := Get_Index_Type (Indexes, I);
                  Canon_Subtype_Indication_If_Anonymous (Index);
               end loop;
            end;
         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition =>
            declare
               Rng : constant Iir := Get_Range_Constraint (Def);
            begin
               if Get_Kind (Rng) = Iir_Kind_Range_Expression then
                  Canon_Expression (Rng);
               end if;
            end;
         when Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Record_Type_Definition =>
            null;
         when Iir_Kind_Access_Subtype_Definition =>
            null;
         when others =>
            Error_Kind ("canon_subtype_indication", Def);
      end case;
   end Canon_Subtype_Indication;

   procedure Canon_Subtype_Indication_If_Anonymous (Def : Iir) is
   begin
      if Is_Anonymous_Type_Definition (Def) then
         Canon_Subtype_Indication (Def);
      end if;
   end Canon_Subtype_Indication_If_Anonymous;

   --  Return the new package declaration (if any).
   function Canon_Package_Instantiation_Declaration (Decl : Iir) return Iir
   is
      Pkg : constant Iir := Get_Uninstantiated_Package_Decl (Decl);
      Bod : Iir;
   begin
      --  Canon map aspect.
      Set_Generic_Map_Aspect_Chain
        (Decl,
         Canon_Association_Chain_And_Actuals
           (Get_Generic_Chain (Decl),
            Get_Generic_Map_Aspect_Chain (Decl), Decl));

      --  Generate the body now.
      --  Note: according to the LRM, if the instantiation occurs within a
      --  package, the body of the instance should be appended to the package
      --  body.
      --  FIXME: generate only if generating code for this unit.
      if Get_Macro_Expanded_Flag (Pkg)
        and then Get_Need_Body (Pkg)
      then
         Bod := Sem_Inst.Instantiate_Package_Body (Decl);
         Set_Parent (Bod, Get_Parent (Decl));
         Set_Instance_Package_Body (Decl, Bod);
      end if;

      return Decl;
   end Canon_Package_Instantiation_Declaration;

   function Canon_Declaration (Top : Iir_Design_Unit; Decl : Iir; Parent : Iir)
                              return Iir
   is
      Stmts : Iir;
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Procedure_Body
            | Iir_Kind_Function_Body =>
            Canon_Declarations (Top, Decl, Null_Iir);
            if Canon_Flag_Add_Suspend_State
              and then Get_Kind (Decl) = Iir_Kind_Procedure_Body
              and then Get_Suspend_Flag (Decl)
            then
               Canon_Add_Suspend_State (Decl);
            end if;
            if Canon_Flag_Sequentials_Stmts then
               Stmts := Get_Sequential_Statement_Chain (Decl);
               Stmts := Canon_Sequential_Stmts (Stmts);
               Set_Sequential_Statement_Chain (Decl, Stmts);
            end if;

         when Iir_Kind_Procedure_Declaration
            | Iir_Kind_Function_Declaration =>
            null;
         when Iir_Kind_Function_Instantiation_Declaration
            | Iir_Kind_Procedure_Instantiation_Declaration =>
            --  Canon map aspect.
            Set_Generic_Map_Aspect_Chain
              (Decl,
               Canon_Association_Chain_And_Actuals
                 (Get_Generic_Chain (Decl),
                  Get_Generic_Map_Aspect_Chain (Decl), Decl));

         when Iir_Kind_Type_Declaration =>
            declare
               Def : Iir;
            begin
               Def := Get_Type_Definition (Decl);
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
               Canon_Subtype_Indication_If_Anonymous (Get_Type (Decl));
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
            Canon_Disconnection_Specification (Decl);
         when Iir_Kind_Step_Limit_Specification =>
            Canon_Step_Limit_Specification (Decl);

         when Iir_Kind_Group_Template_Declaration =>
            null;
         when Iir_Kind_Group_Declaration =>
            null;

         when Iir_Kind_Use_Clause =>
            null;

         when Iir_Kind_Component_Declaration =>
            null;

         when Iir_Kind_Configuration_Specification =>
            if Canon_Flag_Configurations then
               Canon_Component_Specification (Decl, Parent);
               Canon_Component_Configuration (Top, Decl);
            end if;

         when Iir_Kind_Package_Declaration =>
            Canon_Declarations (Top, Decl, Parent);
         when Iir_Kind_Package_Body =>
            Canon_Declarations (Top, Decl, Parent);

         when Iir_Kind_Package_Instantiation_Declaration =>
            return Canon_Package_Instantiation_Declaration (Decl);

         when Iir_Kind_Signal_Attribute_Declaration =>
            null;

         when Iir_Kind_Nature_Declaration
            | Iir_Kind_Subnature_Declaration =>
            null;
         when Iir_Kind_Terminal_Declaration =>
            null;
         when Iir_Kinds_Quantity_Declaration =>
            null;

         when Iir_Kind_Psl_Default_Clock =>
            null;

         when Iir_Kind_Suspend_State_Declaration =>
            null;

         when others =>
            Error_Kind ("canon_declaration", Decl);
      end case;
      return Decl;
   end Canon_Declaration;

   procedure Canon_Declarations (Top : Iir_Design_Unit;
                                 Decl_Parent : Iir;
                                 Parent : Iir)
   is
      Decl : Iir;
      Prev_Decl : Iir;
      New_Decl : Iir;
   begin
      if Parent /= Null_Iir then
         Clear_Instantiation_Configuration (Parent);
      end if;

      Decl := Get_Declaration_Chain (Decl_Parent);
      Prev_Decl := Null_Iir;
      while Decl /= Null_Iir loop
         New_Decl := Canon_Declaration (Top, Decl, Parent);

         if New_Decl /= Decl then
            --  Replace declaration
            if Prev_Decl = Null_Iir then
               Set_Declaration_Chain (Decl_Parent, New_Decl);
            else
               Set_Chain (Prev_Decl, New_Decl);
            end if;
         end if;

         Prev_Decl := New_Decl;
         Decl := Get_Chain (New_Decl);
      end loop;
   end Canon_Declarations;

   --  Append for FIRST_ITEM/LAST_ITEM the default block or component
   --  configuration for statement EL (unless there is already a configuration
   --  for it).
   --  Always clear the association to the configuration for the statement.
   procedure Canon_Block_Configuration_Statement
     (El : Iir; Blk : Iir; Parent : Iir; First_Item, Last_Item : in out Iir)
   is
      procedure Create_Default_Block_Configuration (Targ : Iir)
      is
         Res : Iir;
         Spec : Iir;
      begin
         Res := Create_Iir (Iir_Kind_Block_Configuration);
         Location_Copy (Res, Targ);
         Set_Parent (Res, Parent);
         if True then
            --  For debugging.  Display as user block configuration.
            Spec := Build_Simple_Name (Targ, Targ);
         else
            --  To reduce size, it is possible to refer directly to the block
            --  itself, without using a name.
            Spec := El;
         end if;
         Set_Block_Specification (Res, Spec);
         Chain_Append (First_Item, Last_Item, Res);
      end Create_Default_Block_Configuration;
   begin
      case Get_Kind (El) is
         when Iir_Kind_Component_Instantiation_Statement =>
            declare
               Comp_Conf       : Iir;
               Res             : Iir_Component_Configuration;
               Designator_List : Iir_List;
               Inst_List       : Iir_Flist;
               Inst            : Iir;
               Inst_Name       : Iir;
            begin
               Comp_Conf := Get_Component_Configuration (El);
               if Comp_Conf = Null_Iir then
                  if Is_Component_Instantiation (El) then
                     --  Create a component configuration.
                     --  FIXME: should merge all these default configuration
                     --    of the same component.
                     Res := Create_Iir (Iir_Kind_Component_Configuration);
                     Location_Copy (Res, El);
                     Set_Parent (Res, Parent);
                     Set_Component_Name
                       (Res,
                        Build_Reference_Name (Get_Instantiated_Unit (El)));
                     Designator_List := Create_Iir_List;
                     Append_Element
                       (Designator_List, Build_Simple_Name (El, El));
                     Set_Instantiation_List
                       (Res, List_To_Flist (Designator_List));
                     Chain_Append (First_Item, Last_Item, Res);
                  end if;
               elsif Get_Kind (Comp_Conf)
                 = Iir_Kind_Configuration_Specification
               then
                  --  Create component configuration
                  Res := Create_Iir (Iir_Kind_Component_Configuration);
                  Location_Copy (Res, Comp_Conf);
                  Set_Parent (Res, Parent);
                  Set_Component_Name
                    (Res,
                     Build_Reference_Name (Get_Component_Name (Comp_Conf)));
                  --  Keep in the designator list only the non-incrementally
                  --  bound instances, and only the instances in the current
                  --  statements parts (vhdl-87 generate issue).
                  Inst_List := Get_Instantiation_List (Comp_Conf);
                  Designator_List := Create_Iir_List;
                  for I in Flist_First .. Flist_Last (Inst_List) loop
                     Inst_Name := Get_Nth_Element (Inst_List, I);
                     Inst := Get_Named_Entity (Inst_Name);
                     if Get_Component_Configuration (Inst) = Comp_Conf
                       and then Get_Parent (Inst) = Blk
                     then
                        Set_Component_Configuration (Inst, Res);
                        Append_Element (Designator_List,
                                        Build_Reference_Name (Inst_Name));
                     end if;
                  end loop;
                  Set_Instantiation_List
                    (Res, List_To_Flist (Designator_List));
                  Set_Binding_Indication
                    (Res, Get_Binding_Indication (Comp_Conf));
                  Set_Is_Ref (Res, True);
                  Chain_Append (First_Item, Last_Item, Res);
               end if;
               Set_Component_Configuration (El, Null_Iir);
            end;
         when Iir_Kind_Block_Statement =>
            if Get_Block_Block_Configuration (El) = Null_Iir then
               Create_Default_Block_Configuration (El);
            end if;
         when Iir_Kind_If_Generate_Statement =>
            declare
               Clause     : Iir;
               Bod        : Iir;
               Blk_Config : Iir_Block_Configuration;
            begin
               Clause := El;
               while Clause /= Null_Iir loop
                  Bod := Get_Generate_Statement_Body (Clause);
                  Blk_Config := Get_Generate_Block_Configuration (Bod);
                  if Blk_Config = Null_Iir then
                     Create_Default_Block_Configuration (Bod);
                  end if;
                  Set_Generate_Block_Configuration (Bod, Null_Iir);
                  Clause := Get_Generate_Else_Clause (Clause);
               end loop;
            end;
         when Iir_Kind_Case_Generate_Statement =>
            declare
               Alt        : Iir;
               Bod        : Iir;
               Blk_Config : Iir_Block_Configuration;
            begin
               Alt := Get_Case_Statement_Alternative_Chain (El);
               while Alt /= Null_Iir loop
                  if not Get_Same_Alternative_Flag (Alt) then
                     Bod := Get_Associated_Block (Alt);
                     Blk_Config := Get_Generate_Block_Configuration (Bod);
                     if Blk_Config = Null_Iir then
                        Create_Default_Block_Configuration (Bod);
                     end if;
                     Set_Generate_Block_Configuration (Bod, Null_Iir);
                  end if;
                  Alt := Get_Chain (Alt);
               end loop;
            end;
         when Iir_Kind_For_Generate_Statement =>
            declare
               Bod        : constant Iir := Get_Generate_Statement_Body (El);
               Blk_Config : constant Iir_Block_Configuration :=
                 Get_Generate_Block_Configuration (Bod);
               Res        : Iir_Block_Configuration;
               Blk_Spec   : Iir;
            begin
               if Blk_Config = Null_Iir then
                  Create_Default_Block_Configuration (Bod);
               else
                  Blk_Spec := Strip_Denoting_Name
                    (Get_Block_Specification (Blk_Config));
                  if Get_Kind (Blk_Spec) /= Iir_Kind_Generate_Statement_Body
                  then
                     --  There are generate specification with range or
                     --  expression.  Create a default block configuration
                     --  for the (possible) non-covered values.
                     Res := Create_Iir (Iir_Kind_Block_Configuration);
                     Location_Copy (Res, El);
                     Set_Parent (Res, Parent);
                     Blk_Spec := Create_Iir (Iir_Kind_Indexed_Name);
                     Location_Copy (Blk_Spec, Res);
                     Set_Index_List (Blk_Spec, Iir_Flist_Others);
                     Set_Base_Name (Blk_Spec, El);
                     Set_Prefix (Blk_Spec, Build_Simple_Name (Bod, Res));
                     Set_Block_Specification (Res, Blk_Spec);
                     Chain_Append (First_Item, Last_Item, Res);
                  end if;
               end if;
               Set_Generate_Block_Configuration (Bod, Null_Iir);
            end;

         when Iir_Kinds_Simple_Concurrent_Statement
            | Iir_Kind_Psl_Default_Clock
            | Iir_Kind_Psl_Declaration
            | Iir_Kind_Psl_Endpoint_Declaration
            | Iir_Kind_Simple_Simultaneous_Statement =>
            null;

         when others =>
            Error_Kind ("canon_block_configuration(3)", El);
      end case;
   end Canon_Block_Configuration_Statement;

   --  Recursion for Canon_Block_Configuration: canonicalize each item of a
   --  block configuration (starting with FIRST_ITEM).
   procedure Canon_Block_Configuration_Recurse (Top : Iir_Design_Unit;
                                                First_Item : Iir)
   is
      El : Iir;
   begin
      El := First_Item;
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Block_Configuration =>
               Canon_Block_Configuration (Top, El);
            when Iir_Kind_Component_Configuration =>
               Canon_Component_Configuration (Top, El);
            when others =>
               Error_Kind ("canon_block_configuration_recurse", El);
         end case;
         El := Get_Chain (El);
      end loop;
   end Canon_Block_Configuration_Recurse;

   procedure Canon_Block_Configuration (Top : Iir_Design_Unit;
                                        Conf : Iir_Block_Configuration)
   is
      --  use Iir_Chains.Configuration_Item_Chain_Handling;
      Spec : constant Iir := Get_Block_Specification (Conf);
      Blk : constant Iir := Get_Block_From_Block_Specification (Spec);
      Stmts : constant Iir := Get_Concurrent_Statement_Chain (Blk);
      El : Iir;
      Sub_Blk : Iir;
      First_Item, Last_Item : Iir;

   begin
      --  Note: the only allowed declarations are use clauses, which are not
      --  canonicalized.

      --  FIXME: handle indexed/sliced name?

      Clear_Instantiation_Configuration (Blk);

      --  1) Configure instantiations with configuration specifications.
      --  TODO: merge.
      El := Get_Declaration_Chain (Blk);
      while El /= Null_Iir loop
         if Get_Kind (El) = Iir_Kind_Configuration_Specification then
            --  Already canonicalized during canon of block declarations.
            --  But need to set configuration on instantiations.
            Canon_Component_Specification (El, Blk);
         end if;
         El := Get_Chain (El);
      end loop;

      --  2) Configure instantations with component configurations,
      --     and map block configurations with block/generate statements.
      First_Item := Get_Configuration_Item_Chain (Conf);
      El := First_Item;
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Configuration_Specification =>
               raise Internal_Error;
            when Iir_Kind_Component_Configuration =>
               Canon_Component_Specification (El, Blk);
            when Iir_Kind_Block_Configuration =>
               Sub_Blk := Get_Block_From_Block_Specification
                 (Get_Block_Specification (El));
               case Get_Kind (Sub_Blk) is
                  when Iir_Kind_Block_Statement =>
                     Set_Block_Block_Configuration (Sub_Blk, El);
                  when Iir_Kind_Indexed_Name
                    | Iir_Kind_Slice_Name =>
                     Sub_Blk := Strip_Denoting_Name (Get_Prefix (Sub_Blk));
                     Set_Prev_Block_Configuration
                       (El, Get_Generate_Block_Configuration (Sub_Blk));
                     Set_Generate_Block_Configuration (Sub_Blk, El);
                  when Iir_Kind_Parenthesis_Name =>
                     Sub_Blk := Get_Named_Entity (Sub_Blk);
                     Set_Prev_Block_Configuration
                       (El, Get_Generate_Block_Configuration (Sub_Blk));
                     Set_Generate_Block_Configuration (Sub_Blk, El);
                  when Iir_Kind_Generate_Statement_Body =>
                     Set_Generate_Block_Configuration (Sub_Blk, El);
                  when others =>
                     Error_Kind ("canon_block_configuration(0)", Sub_Blk);
               end case;
            when others =>
               Error_Kind ("canon_block_configuration(1)", El);
         end case;
         Last_Item := El;
         El := Get_Chain (El);
      end loop;

      --  3) Add default component configuration for unspecified component
      --     instantiation statements,
      --     Add default block configuration for unconfigured block statements.
      El := Stmts;
      while El /= Null_Iir loop
         Canon_Block_Configuration_Statement
           (El, Blk, Conf, First_Item, Last_Item);
         El := Get_Chain (El);
      end loop;
      Set_Configuration_Item_Chain (Conf, First_Item);

      --  4) Canon component configuration and block configuration (recursion).
      Canon_Block_Configuration_Recurse (Top, First_Item);
   end Canon_Block_Configuration;

   procedure Canon_Interface_List (Chain : Iir)
   is
      Inter : Iir;
   begin
      if Canon_Flag_Expressions then
         Inter := Chain;
         while Inter /= Null_Iir loop
            Canon_Subtype_Indication_If_Anonymous (Get_Type (Inter));
            Canon_Expression (Get_Default_Value (Inter));
            Inter := Get_Chain (Inter);
         end loop;
      end if;
   end Canon_Interface_List;

   procedure Canon_Psl_Verification_Unit (Unit : Iir_Design_Unit)
   is
      Decl       : constant Iir := Get_Library_Unit (Unit);
      Item       : Iir;
      Prev_Item  : Iir;
      Blk_Cfg    : Iir;
      First_Conf : Iir;
      Last_Conf  : Iir;
      Proc_Num   : Natural := 0;
   begin
      Blk_Cfg := Create_Iir (Iir_Kind_Block_Configuration);
      Set_Location (Blk_Cfg, Get_Location (Unit));
      Set_Parent (Blk_Cfg, Unit);
      Set_Block_Specification (Blk_Cfg, Build_Simple_Name (Decl, Blk_Cfg));
      Set_Verification_Block_Configuration (Decl, Blk_Cfg);

      First_Conf := Null_Iir;
      Last_Conf := Null_Iir;

      Prev_Item := Null_Iir;
      Item := Get_Vunit_Item_Chain (Decl);
      while Item /= Null_Iir loop
         case Get_Kind (Item) is
            when Iir_Kind_Psl_Default_Clock
               | Iir_Kind_PSL_Inherit_Spec =>
               null;
            when Iir_Kind_Psl_Assert_Directive =>
               Canon_Psl_Assert_Directive (Item);
            when Iir_Kind_Psl_Assume_Directive =>
               Canon_Psl_Property_Directive (Item);
            when Iir_Kind_Psl_Restrict_Directive =>
               Canon_Psl_Sequence_Directive (Item);
            when Iir_Kind_Psl_Cover_Directive =>
               Canon_Psl_Cover_Directive (Item);
            when Iir_Kind_Psl_Declaration =>
               Canon_Concurrent_Statement (Item, Unit);
            when Iir_Kind_Signal_Declaration
               | Iir_Kind_Constant_Declaration
               | Iir_Kind_Type_Declaration
               | Iir_Kind_Subtype_Declaration
               | Iir_Kind_Anonymous_Type_Declaration
               | Iir_Kind_Function_Declaration
               | Iir_Kind_Procedure_Declaration
               | Iir_Kind_Function_Body
               | Iir_Kind_Procedure_Body
               | Iir_Kind_Attribute_Declaration
               | Iir_Kind_Attribute_Specification
               | Iir_Kind_Object_Alias_Declaration
               | Iir_Kind_Non_Object_Alias_Declaration =>
               Item := Canon_Declaration (Unit, Item, Null_Iir);
            when Iir_Kinds_Concurrent_Signal_Assignment
               | Iir_Kinds_Process_Statement
               | Iir_Kinds_Generate_Statement
               | Iir_Kind_Block_Statement
               | Iir_Kind_Concurrent_Procedure_Call_Statement
               | Iir_Kind_Component_Instantiation_Statement =>
               Canon_Concurrent_Label (Item, Proc_Num);
               Canon_Concurrent_Statement (Item, Unit);
               Canon_Block_Configuration_Statement
                 (Item, Unit, Unit, First_Conf, Last_Conf);
            when others =>
               Error_Kind ("canon_psl_verification_unit", Item);
         end case;

         if Prev_Item = Null_Iir then
            Set_Vunit_Item_Chain (Decl, Item);
         else
            Set_Chain (Prev_Item, Item);
         end if;
         Prev_Item := Item;
         Item := Get_Chain (Item);
      end loop;

      Set_Configuration_Item_Chain (Blk_Cfg, First_Conf);
      Canon_Block_Configuration_Recurse (Unit, First_Conf);
   end Canon_Psl_Verification_Unit;

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
               when Iir_Kind_Use_Clause
                 | Iir_Kind_Library_Clause
                 | Iir_Kind_Context_Reference =>
                  null;
               when others =>
                  Error_Kind ("canonicalize1", El);
            end case;
            El := Get_Chain (El);
         end loop;
      end if;

      El := Get_Library_Unit (Unit);
      case Iir_Kinds_Library_Unit (Get_Kind (El)) is
         when Iir_Kind_Entity_Declaration =>
            Canon_Interface_List (Get_Generic_Chain (El));
            Canon_Interface_List (Get_Port_Chain (El));
            Canon_Declarations (Unit, El, El);
            Canon_Concurrent_Stmts (Unit, El);
         when Iir_Kind_Architecture_Body =>
            Canon_Declarations (Unit, El, El);
            Canon_Concurrent_Stmts (Unit, El);
         when Iir_Kind_Package_Declaration =>
            Canon_Declarations (Unit, El, Null_Iir);
         when Iir_Kind_Package_Body =>
            Canon_Declarations (Unit, El, Null_Iir);
         when Iir_Kind_Configuration_Declaration =>
            Canon_Declarations (Unit, El, Null_Iir);
            if Canon_Flag_Configurations then
               Canon_Block_Configuration (Unit, Get_Block_Configuration (El));
            end if;
         when Iir_Kind_Package_Instantiation_Declaration =>
            El := Canon_Package_Instantiation_Declaration (El);
            Set_Library_Unit (Unit, El);
         when Iir_Kind_Context_Declaration =>
            null;
         when Iir_Kind_Vunit_Declaration =>
            Canon_Psl_Verification_Unit (Unit);
         when Iir_Kind_Vmode_Declaration
           | Iir_Kind_Vprop_Declaration =>
            null;
         when Iir_Kind_Foreign_Module =>
            raise Internal_Error;
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
     (Arch : Iir_Architecture_Body) return Iir_Design_Unit
   is
      Loc : constant Location_Type := Get_Location (Arch);
      Config : Iir_Configuration_Declaration;
      Res : Iir_Design_Unit;
      Blk_Cfg : Iir_Block_Configuration;
   begin
      Res := Create_Iir (Iir_Kind_Design_Unit);
      Set_Location (Res, Loc);
      Set_Parent (Res, Get_Parent (Get_Design_Unit (Arch)));
      Set_Date_State (Res, Date_Analyze);
      Set_Date (Res, Date_Uptodate);

      Config := Create_Iir (Iir_Kind_Configuration_Declaration);
      Set_Location (Config, Loc);
      Set_Library_Unit (Res, Config);
      Set_Design_Unit (Config, Res);
      Set_Entity_Name (Config, Get_Entity_Name (Arch));
      Set_Dependence_List (Res, Create_Iir_List);
      Add_Dependence (Res, Get_Design_Unit (Get_Entity (Config)));
      Add_Dependence (Res, Get_Design_Unit (Arch));

      Blk_Cfg := Create_Iir (Iir_Kind_Block_Configuration);
      Set_Location (Blk_Cfg, Loc);
      Set_Parent (Blk_Cfg, Config);
      Set_Block_Specification (Blk_Cfg, Build_Simple_Name (Arch, Blk_Cfg));
      Set_Block_Configuration (Config, Blk_Cfg);

      Canon_Block_Configuration (Res, Blk_Cfg);

      return Res;
   end Create_Default_Configuration_Declaration;

end Vhdl.Canon;
