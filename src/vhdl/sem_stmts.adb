--  Semantic analysis.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with Types; use Types;
with Flags; use Flags;
with Sem_Specs; use Sem_Specs;
with Std_Package; use Std_Package;
with Sem; use Sem;
with Sem_Decls; use Sem_Decls;
with Sem_Expr; use Sem_Expr;
with Sem_Names; use Sem_Names;
with Sem_Scopes; use Sem_Scopes;
with Sem_Types;
with Sem_Psl;
with Std_Names;
with Evaluation; use Evaluation;
with Iirs_Utils; use Iirs_Utils;
with Xrefs; use Xrefs;

package body Sem_Stmts is
   -- Process is the scope, this is also the process for which drivers can
   -- be created.
   -- Note: FIRST_STMT is the first statement, which can be get by:
   --  get_sequential_statement_chain (usual)
   --  get_associated_chain (for case statement).
   procedure Sem_Sequential_Statements_Internal (First_Stmt : Iir);

   -- Access to the current subprogram or process.
   Current_Subprogram: Iir := Null_Iir;

   function Get_Current_Subprogram return Iir is
   begin
      return Current_Subprogram;
   end Get_Current_Subprogram;

   -- Access to the current concurrent statement.
   -- Null_iir if no one.
   Current_Concurrent_Statement : Iir := Null_Iir;

   function Get_Current_Concurrent_Statement return Iir is
   begin
      return Current_Concurrent_Statement;
   end Get_Current_Concurrent_Statement;

   --  LRM 8 Sequential statements.
   --  All statements may be labeled.
   --  Such labels are implicitly declared at the beginning of the declarative
   --  part of the innermost enclosing process statement of subprogram body.
   procedure Sem_Sequential_Labels (First_Stmt : Iir)
   is
      Stmt: Iir;
      Label: Name_Id;
   begin
      Stmt := First_Stmt;
      while Stmt /= Null_Iir loop
         Label := Get_Label (Stmt);
         if Label /= Null_Identifier then
            Sem_Scopes.Add_Name (Stmt);
            Name_Visible (Stmt);
            Xref_Decl (Stmt);
         end if;

         --  Some statements have sub-lists of statements.
         case Get_Kind (Stmt) is
            when Iir_Kind_For_Loop_Statement
              | Iir_Kind_While_Loop_Statement =>
               Sem_Sequential_Labels (Get_Sequential_Statement_Chain (Stmt));
            when Iir_Kind_If_Statement =>
               declare
                  Clause : Iir;
               begin
                  Clause := Stmt;
                  while Clause /= Null_Iir loop
                     Sem_Sequential_Labels
                       (Get_Sequential_Statement_Chain (Clause));
                     Clause := Get_Else_Clause (Clause);
                  end loop;
               end;
            when Iir_Kind_Case_Statement =>
               declare
                  El : Iir;
               begin
                  El := Get_Case_Statement_Alternative_Chain (Stmt);
                  while El /= Null_Iir loop
                     Sem_Sequential_Labels (Get_Associated_Chain (El));
                     El := Get_Chain (El);
                  end loop;
               end;
            when others =>
               null;
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Sem_Sequential_Labels;

   procedure Fill_Array_From_Aggregate_Associated
     (Chain : Iir; Nbr : in out Natural; Arr : in out Iir_Array)
   is
      El : Iir;
      Ass : Iir;
   begin
      El := Chain;
      while El /= Null_Iir loop
         Ass := Get_Associated_Expr (El);
         if Get_Kind (Ass) = Iir_Kind_Aggregate then
            Fill_Array_From_Aggregate_Associated
              (Get_Association_Choices_Chain (Ass), Nbr, Arr);
         else
            Arr (Nbr) := Ass;
            Nbr := Nbr + 1;
         end if;
         El := Get_Chain (El);
      end loop;
   end Fill_Array_From_Aggregate_Associated;

   --  Return TRUE iff there is no common elements designed by N1 and N2.
   --  N1 and N2 are static names.
   --  FIXME:  The current implementation is completly wrong; should check from
   --   prefix to suffix.
   function Is_Disjoint (N1, N2: Iir) return Boolean
   is
      List1, List2 : Iir_List;
      El1, El2 : Iir;
   begin
      if N1 = N2 then
         return False;
      end if;
      if Get_Kind (N1) = Iir_Kind_Indexed_Name
        and then Get_Kind (N2) = Iir_Kind_Indexed_Name
      then
         if Is_Disjoint (Get_Prefix (N1), Get_Prefix (N2)) then
            return True;
         end if;
         --  Check indexes.
         List1 := Get_Index_List (N1);
         List2 := Get_Index_List (N2);
         for I in Natural loop
            El1 := Get_Nth_Element (List1, I);
            El2 := Get_Nth_Element (List2, I);
            exit when El1 = Null_Iir;
            El1 := Eval_Expr (El1);
            Replace_Nth_Element (List1, I, El1);
            El2 := Eval_Expr (El2);
            Replace_Nth_Element (List2, I, El2);
            --  EL are of discrete type.
            if Get_Value (El1) /= Get_Value (El2) then
               return True;
            end if;
         end loop;
         return False;
      elsif Get_Kind (N1) in Iir_Kinds_Denoting_Name
        and then Get_Kind (N2) in Iir_Kinds_Denoting_Name
      then
         return Get_Named_Entity (N1) /= Get_Named_Entity (N2);
      else
         return True;
      end if;
   end Is_Disjoint;

   procedure Check_Uniq_Aggregate_Associated
     (Aggr : Iir_Aggregate; Nbr : Natural)
   is
      Chain : constant Iir := Get_Association_Choices_Chain (Aggr);
      subtype El_Array_Type is Iir_Array (0 .. Nbr - 1);
      Name_Arr, Obj_Arr : El_Array_Type;
      Index : Natural;
      El : Iir;
   begin
      --  Fill the array.
      Index := 0;
      Fill_Array_From_Aggregate_Associated (Chain, Index, Name_Arr);
      --  Should be the same.
      pragma Assert (Index = Nbr);

      --  Replace name with object.  Return now in case of error (not an
      --  object or not a static name).
      for I in Name_Arr'Range loop
         El := Name_To_Object (Name_Arr (I));
         if El = Null_Iir
           or else Get_Name_Staticness (El) /= Locally
         then
            --  Error...
            return;
         end if;
         Obj_Arr (I) := El;
      end loop;

      --  Check each element is uniq.
      for I in Name_Arr'Range loop
         for J in 0 .. I - 1 loop
            if not Is_Disjoint (Obj_Arr (I), Obj_Arr (J)) then
               Error_Msg_Sem
                 (+Name_Arr (I), "target is assigned more than once",
                  Cont => True);
               Error_Msg_Sem
                 (+Name_Arr (J), " (previous assignment is here)");
               return;
            end if;
         end loop;
      end loop;
   end Check_Uniq_Aggregate_Associated;

   --  Do checks for the target of an assignment.
   procedure Check_Simple_Signal_Target
     (Stmt : Iir; Target : Iir; Staticness : Iir_Staticness);
   --  STMT is used to localize the error (if any).
   procedure Check_Simple_Variable_Target
     (Stmt : Iir; Target : Iir; Staticness : Iir_Staticness);

   -- Semantic associed with signal mode.
   -- See �4.3.3
   type Boolean_Array_Of_Iir_Mode is array (Iir_Mode) of Boolean;
   Iir_Mode_Readable : constant Boolean_Array_Of_Iir_Mode :=
     (Iir_Unknown_Mode => False,
      Iir_In_Mode => True,
      Iir_Out_Mode => False,
      Iir_Inout_Mode => True,
      Iir_Buffer_Mode => True,
      Iir_Linkage_Mode => False);
   Iir_Mode_Writable : constant Boolean_Array_Of_Iir_Mode :=
     (Iir_Unknown_Mode => False,
      Iir_In_Mode => False,
      Iir_Out_Mode => True,
      Iir_Inout_Mode => True,
      Iir_Buffer_Mode => True,
      Iir_Linkage_Mode => False);

   procedure Check_Aggregate_Target
     (Stmt : Iir; Target : Iir; Nbr : in out Natural)
   is
      Choice : Iir;
      Ass : Iir;
   begin
      Choice := Get_Association_Choices_Chain (Target);
      while Choice /= Null_Iir loop
         case Get_Kind (Choice) is
            when Iir_Kind_Choice_By_Range =>
               --  LRM93 8.4
               --  It is an error if an element association in such an
               --  aggregate contains an OTHERS choice or a choice that is
               --  a discrete range.
               Error_Msg_Sem
                 (+Choice, "discrete range choice not allowed for target");
            when Iir_Kind_Choice_By_Others =>
               --  LRM93 8.4
               --  It is an error if an element association in such an
               --  aggregate contains an OTHERS choice or a choice that is
               --  a discrete range.
               Error_Msg_Sem
                 (+Choice, "others choice not allowed for target");
            when Iir_Kind_Choice_By_Expression
              | Iir_Kind_Choice_By_Name
              | Iir_Kind_Choice_By_None =>
               --  LRM93 9.4
               --  Such a target may not only contain locally static signal
               --  names [...]
               Ass := Get_Associated_Expr (Choice);
               if Get_Kind (Ass) = Iir_Kind_Aggregate then
                  Check_Aggregate_Target (Stmt, Ass, Nbr);
               else
                  if Get_Kind (Stmt) in
                    Iir_Kinds_Variable_Assignment_Statement
                  then
                     Check_Simple_Variable_Target (Stmt, Ass, Locally);
                  else
                     Check_Simple_Signal_Target (Stmt, Ass, Locally);
                  end if;
                  Nbr := Nbr + 1;
               end if;
            when others =>
               Error_Kind ("check_aggregate_target", Choice);
         end case;
         Choice := Get_Chain (Choice);
      end loop;
   end Check_Aggregate_Target;

   procedure Check_Simple_Signal_Target
     (Stmt : Iir; Target : Iir; Staticness : Iir_Staticness)
   is
      Target_Object : Iir;
      Target_Prefix : Iir;
      Guarded_Target : Tri_State_Type;
      Targ_Obj_Kind : Iir_Kind;
   begin
      Target_Object := Name_To_Object (Target);
      if Target_Object = Null_Iir then
         Error_Msg_Sem (+Target, "target is not a signal name");
         return;
      end if;

      Target_Prefix := Get_Object_Prefix (Target_Object);
      Targ_Obj_Kind := Get_Kind (Target_Prefix);
      case Targ_Obj_Kind is
         when Iir_Kind_Interface_Signal_Declaration =>
            if not Iir_Mode_Writable (Get_Mode (Target_Prefix)) then
               Error_Msg_Sem
                 (+Target, "%n can't be assigned", +Target_Prefix);
            else
               Sem_Add_Driver (Target_Object, Stmt);
            end if;
         when Iir_Kind_Signal_Declaration =>
            Sem_Add_Driver (Target_Object, Stmt);
         when Iir_Kind_Guard_Signal_Declaration =>
            Error_Msg_Sem (+Stmt, "implicit GUARD signal cannot be assigned");
            return;
         when others =>
            Error_Msg_Sem
              (+Stmt, "target (%n) is not a signal", +Get_Base_Name (Target));
            return;
      end case;
      if Get_Name_Staticness (Target_Object) < Staticness then
         Error_Msg_Sem (+Stmt, "signal name must be static");
      end if;

      --  LRM93 2.1.1.2
      --  A formal signal parameter is a guarded signal if and only if
      --  it is associated with an actual signal that is a guarded
      --  signal.
      --  GHDL: a formal signal interface of a subprogram has no static
      --   kind.  This is determined at run-time, according to the actual
      --   associated with the formal.
      --  GHDL: parent of target cannot be a function.
      if Targ_Obj_Kind = Iir_Kind_Interface_Signal_Declaration
        and then
        Get_Kind (Get_Parent (Target_Prefix)) = Iir_Kind_Procedure_Declaration
      then
         Guarded_Target := Unknown;
      else
         if Get_Guarded_Signal_Flag (Target_Prefix) then
            Guarded_Target := True;
         else
            Guarded_Target := False;
         end if;
      end if;

      case Get_Guarded_Target_State (Stmt) is
         when Unknown =>
            Set_Guarded_Target_State (Stmt, Guarded_Target);
         when True
           | False =>
            if Get_Guarded_Target_State (Stmt) /= Guarded_Target then
               --  LRM93 9.5
               --  It is an error if the target of a concurrent signal
               --  assignment is neither a guarded target nor an
               --  unguarded target.
               Error_Msg_Sem (+Target, "guarded and unguarded target");
            end if;
      end case;
   end Check_Simple_Signal_Target;

   procedure Check_Simple_Variable_Target
     (Stmt : Iir; Target : Iir; Staticness : Iir_Staticness)
   is
      Target_Object : Iir;
      Target_Prefix : Iir;
   begin
      Target_Object := Name_To_Object (Target);
      if Target_Object = Null_Iir then
         Error_Msg_Sem (+Stmt, "target is not a variable name");
         return;
      end if;
      Target_Prefix := Get_Object_Prefix (Target_Object);
      case Get_Kind (Target_Prefix) is
         when Iir_Kind_Interface_Variable_Declaration =>
            if not Iir_Mode_Writable (Get_Mode (Target_Prefix)) then
               Error_Msg_Sem (+Target, "%n cannot be written (bad mode)",
                              +Target_Prefix);
               return;
            end if;
         when Iir_Kind_Variable_Declaration =>
            null;
         when Iir_Kind_Implicit_Dereference
           | Iir_Kind_Dereference  =>
            --  LRM 3.3
            --  An object designated by an access type is always an object of
            --  class variable.
            null;
         when others =>
            Error_Msg_Sem (+Stmt, "%n is not a variable to be assigned",
                           +Target_Prefix);
            return;
      end case;
      if Get_Name_Staticness (Target_Object) < Staticness then
         Error_Msg_Sem
           (+Target, "element of a target aggregate must be a static name");
      end if;
   end Check_Simple_Variable_Target;

   procedure Check_Target (Stmt : Iir; Target : Iir)
   is
      Nbr : Natural;
   begin
      if Get_Kind (Target) = Iir_Kind_Aggregate then
         Nbr := 0;
         Check_Aggregate_Target (Stmt, Target, Nbr);
         Check_Uniq_Aggregate_Associated (Target, Nbr);
      else
         case Get_Kind (Stmt) is
            when Iir_Kind_Variable_Assignment_Statement
              | Iir_Kind_Conditional_Variable_Assignment_Statement =>
               Check_Simple_Variable_Target (Stmt, Target, None);
            when others =>
               Check_Simple_Signal_Target (Stmt, Target, None);
         end case;
      end if;
   end Check_Target;

   type Resolve_Stages is (Resolve_Stage_1, Resolve_Stage_2);
   pragma Unreferenced (Resolve_Stage_2);

   procedure Sem_Signal_Assignment_Target_And_Option
     (Stmt: Iir; Sig_Type : in out Iir)
   is
      --  The target of the assignment.
      Target: Iir;
      --  The value that will be assigned.
      Expr: Iir;
   begin
      Target := Get_Target (Stmt);
      Target := Sem_Expression_Wildcard (Target, Sig_Type);

      if Target /= Null_Iir then
         Set_Target (Stmt, Target);
         if Is_Expr_Fully_Analyzed (Target) then
            Check_Target (Stmt, Target);
            Sig_Type := Get_Type (Target);
            Sem_Types.Set_Type_Has_Signal (Sig_Type);
         end if;
      end if;

      Expr := Get_Reject_Time_Expression (Stmt);
      if Expr /= Null_Iir
        and then Is_Expr_Not_Analyzed (Expr)
      then
         Expr := Sem_Expression (Expr, Time_Type_Definition);
         if Expr /= Null_Iir then
            Check_Read (Expr);
            Set_Reject_Time_Expression (Stmt, Expr);
         end if;
      end if;
   end Sem_Signal_Assignment_Target_And_Option;

   --  Analyze a waveform_list WAVEFORM_LIST that is assigned via statement
   --  ASSIGN_STMT to a subelement or a slice of a signal SIGNAL_DECL.
   procedure Sem_Waveform_Chain (Waveform_Chain : Iir_Waveform_Element;
                                 Waveform_Type : in out Iir)
   is
      Expr: Iir;
      We: Iir_Waveform_Element;
      Time, Last_Time : Iir_Int64;
   begin
      if Waveform_Chain = Null_Iir then
         --  Unaffected.
         return;
      end if;

      --  Start with -1 to allow after 0 ns.
      Last_Time := -1;
      We := Waveform_Chain;
      while We /= Null_Iir loop
         Expr := Get_We_Value (We);
         if Get_Kind (Expr) = Iir_Kind_Null_Literal then
            --  GHDL: allowed only if target is guarded; this is checked by
            --  sem_check_waveform_list.
            null;
         else
            Expr := Sem_Expression_Wildcard (Expr, Waveform_Type);

            if Expr /= Null_Iir then
               if Is_Expr_Fully_Analyzed (Expr) then
                  Check_Read (Expr);
                  Expr := Eval_Expr_If_Static (Expr);
               end if;
               Set_We_Value (We, Expr);

               Merge_Wildcard_Type (Expr, Waveform_Type);
            end if;
         end if;

         --  Analyze time expression.
         if Get_Time (We) /= Null_Iir then
            Expr := Get_Time (We);
            if Is_Expr_Not_Analyzed (Expr) then
               Expr := Sem_Expression (Expr, Time_Type_Definition);
               if Expr /= Null_Iir then
                  Set_Time (We, Expr);
                  Check_Read (Expr);

                  if Get_Expr_Staticness (Expr) = Locally
                    or else (Get_Kind (Expr) = Iir_Kind_Physical_Int_Literal
                               and then Flags.Flag_Time_64)
                  then
                     --  LRM 8.4
                     --  It is an error if the time expression in a waveform
                     --  element evaluates to a negative value.
                     --
                     --  LRM 8.4.1
                     --  It is an error if the sequence of new transactions is
                     --  not in ascending order with repect to time.
                     -- GHDL: this must be checked at run-time, but this is
                     --  also checked now for static expressions.
                     if Get_Expr_Staticness (Expr) = Locally then
                        --  The expression is static, and therefore may be
                        --  evaluated.
                        Expr := Eval_Expr (Expr);
                        Set_Time (We, Expr);
                        Time := Get_Value (Expr);
                     else
                        --  The expression is a physical literal (common case).
                        --  Extract its value.
                        Time := Get_Physical_Value (Expr);
                     end if;
                     if Time < 0 then
                        Error_Msg_Sem
                          (+Expr, "waveform time expression must be >= 0");
                     elsif Time <= Last_Time then
                        Error_Msg_Sem
                          (+Expr,
                           "time must be greather than previous transaction");
                     else
                        Last_Time := Time;
                     end if;
                  end if;
               end if;
            end if;
         else
            if We /= Waveform_Chain then
               --  Time expression must be in ascending order.
               Error_Msg_Sem (+We, "time expression required here");
            end if;

            --  LRM93 12.6.4
            --  It is an error if the execution of any postponed process
            --  causes a delta cycle to occur immediatly after the current
            --  simulation cycle.
            --  GHDL: try to warn for such an error; note the context may be
            --   a procedure body.
            if Current_Concurrent_Statement /= Null_Iir then
               case Get_Kind (Current_Concurrent_Statement) is
                  when Iir_Kind_Sensitized_Process_Statement
                    | Iir_Kind_Process_Statement
                    | Iir_Kind_Concurrent_Conditional_Signal_Assignment
                    | Iir_Kind_Concurrent_Selected_Signal_Assignment =>
                     if Get_Postponed_Flag (Current_Concurrent_Statement) then
                        Warning_Msg_Sem
                          (Warnid_Delta_Cycle, +We,
                           "waveform may cause a delta cycle in a " &
                             "postponed process");
                     end if;
                  when others =>
                     --  Context is a subprogram.
                     null;
               end case;
            end if;

            Last_Time := 0;
         end if;

         We := Get_Chain (We);
      end loop;
   end Sem_Waveform_Chain;

   --  Analyze a waveform chain WAVEFORM_CHAIN that is assigned via statement
   --  ASSIGN_STMT to a subelement or a slice of a signal SIGNAL_DECL.
   procedure Sem_Check_Waveform_Chain
     (Assign_Stmt: Iir; Waveform_Chain: Iir_Waveform_Element)
   is
      We: Iir_Waveform_Element;
      Expr : Iir;
      Targ_Type : Iir;
   begin
      if Waveform_Chain = Null_Iir then
         return;
      end if;

      Targ_Type := Get_Type (Get_Target (Assign_Stmt));

      We := Waveform_Chain;
      while We /= Null_Iir loop
         Expr := Get_We_Value (We);
         if Get_Kind (Expr) = Iir_Kind_Null_Literal then
            --  This is a null waveform element.
            --  LRM93 8.4.1
            --  It is an error if the target of a signal assignment statement
            --  containing a null waveform is not a guarded signal or an
            --  aggregate of guarded signals.
            if Get_Guarded_Target_State (Assign_Stmt) = False then
               Error_Msg_Sem
                 (+Assign_Stmt,
                  "null transactions can be assigned only to guarded signals");
            end if;
         else
            if not Check_Implicit_Conversion (Targ_Type, Expr) then
               Error_Msg_Sem
                 (+We, "length of value does not match length of target");
            end if;
         end if;
         We := Get_Chain (We);
      end loop;
   end Sem_Check_Waveform_Chain;

   procedure Sem_Guard (Stmt: Iir)
   is
      Guard: Iir;
      Guard_Interpretation : Name_Interpretation_Type;
   begin
      Guard := Get_Guard (Stmt);
      if Guard = Null_Iir then
         --  This assignment is not guarded.

         --  LRM93 9.5
         --  It is an error if a concurrent signal assignment is not a guarded
         --  assignment, and the target of the concurrent signal assignment
         --  is a guarded target.
         if Get_Guarded_Target_State (Stmt) = True then
            Error_Msg_Sem
              (+Stmt, "not a guarded assignment for a guarded target");
         end if;
         return;
      end if;
      if Guard /= Stmt then
         -- if set, guard must be equal to stmt here.
         raise Internal_Error;
      end if;
      Guard_Interpretation := Get_Interpretation (Std_Names.Name_Guard);
      if not Valid_Interpretation (Guard_Interpretation) then
         Error_Msg_Sem (+Stmt, "no guard signals for this guarded assignment");
         return;
      end if;

      Guard := Get_Declaration (Guard_Interpretation);
      -- LRM93 9.5:
      -- The signal GUARD [...] an explicitly declared signal of type
      -- BOOLEAN that is visible at the point of the concurrent signal
      -- assignment statement
      -- FIXME.
      case Get_Kind (Guard) is
         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Guard_Signal_Declaration =>
            null;
         when others =>
            Error_Msg_Sem (+Stmt, "visible GUARD object is not a signal",
                          Cont => True);
            Error_Msg_Sem (+Stmt, "GUARD object is %n", +Guard);
            return;
      end case;

      if Get_Type (Guard) /= Boolean_Type_Definition then
         Error_Msg_Sem (+Guard, "GUARD is not of boolean type");
      end if;
      Set_Guard (Stmt, Guard);
   end Sem_Guard;

   procedure Sem_Signal_Assignment (Stmt: Iir)
   is
      Cond_Wf : Iir_Conditional_Waveform;
      Expr : Iir;
      Wf_Chain : Iir_Waveform_Element;
      Target_Type : Iir;
      Done : Boolean;
   begin
      Target_Type := Wildcard_Any_Type;

      Done := False;
      for S in Resolve_Stages loop
         Sem_Signal_Assignment_Target_And_Option (Stmt, Target_Type);
         if Is_Defined_Type (Target_Type) then
            Done := True;
         end if;

         case Get_Kind (Stmt) is
            when Iir_Kind_Concurrent_Simple_Signal_Assignment
              | Iir_Kind_Simple_Signal_Assignment_Statement =>
               Wf_Chain := Get_Waveform_Chain (Stmt);
               Sem_Waveform_Chain (Wf_Chain, Target_Type);
               if Done then
                  Sem_Check_Waveform_Chain (Stmt, Wf_Chain);
               end if;

            when Iir_Kind_Concurrent_Conditional_Signal_Assignment
              | Iir_Kind_Conditional_Signal_Assignment_Statement =>
               Cond_Wf := Get_Conditional_Waveform_Chain (Stmt);
               while Cond_Wf /= Null_Iir loop
                  Wf_Chain := Get_Waveform_Chain (Cond_Wf);
                  Sem_Waveform_Chain (Wf_Chain, Target_Type);
                  if Done then
                     Sem_Check_Waveform_Chain (Stmt, Wf_Chain);
                  end if;
                  if S = Resolve_Stage_1 then
                     --  Must be analyzed only once.
                     Expr := Get_Condition (Cond_Wf);
                     if Expr /= Null_Iir then
                        Expr := Sem_Condition (Expr);
                        if Expr /= Null_Iir then
                           Set_Condition (Cond_Wf, Expr);
                        end if;
                     end if;
                  end if;
                  Cond_Wf := Get_Chain (Cond_Wf);
               end loop;

            when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
               declare
                  El : Iir;
               begin
                  El := Get_Selected_Waveform_Chain (Stmt);
                  while El /= Null_Iir loop
                     Wf_Chain := Get_Associated_Chain (El);
                     Sem_Waveform_Chain (Wf_Chain, Target_Type);
                     if Done then
                        Sem_Check_Waveform_Chain (Stmt, Wf_Chain);
                     end if;
                     El := Get_Chain (El);
                  end loop;
               end;

            when others =>
               raise Internal_Error;
         end case;

         exit when Done;
         if not Is_Defined_Type (Target_Type) then
            Error_Msg_Sem (+Stmt, "cannot resolve type of waveform");
            exit;
         end if;
      end loop;

      case Get_Kind (Stmt) is
         when Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
            Sem_Guard (Stmt);
         when others =>
            null;
      end case;
   end Sem_Signal_Assignment;

   procedure Sem_Conditional_Expression (Cond_Expr : Iir; Atype : in out Iir)
   is
      El : Iir;
      Expr : Iir;
      Cond : Iir;
   begin
      El := Cond_Expr;
      while El /= Null_Iir loop
         Expr := Get_Expression (El);
         Expr := Sem_Expression_Wildcard (Expr, Atype);

         if Expr /= Null_Iir then
            Set_Expression (El, Expr);

            if Is_Expr_Fully_Analyzed (Expr) then
               Check_Read (Expr);
               Expr := Eval_Expr_If_Static (Expr);
            end if;

            Merge_Wildcard_Type (Expr, Atype);
         end if;

         Cond := Get_Condition (El);
         exit when Cond = Null_Iir;

         if Is_Expr_Not_Analyzed (Cond) then
            Cond := Sem_Condition (Cond);
            Set_Condition (El, Cond);
         end if;

         El := Get_Chain (El);
      end loop;
   end Sem_Conditional_Expression;

   procedure Sem_Variable_Assignment (Stmt: Iir)
   is
      Target : Iir;
      Expr : Iir;
      Target_Type : Iir;
      Stmt_Type : Iir;
      Done : Boolean;
   begin
      --  LRM93 8.5 Variable assignment statement
      --  If the target of the variable assignment statement is in the form of
      --  an aggregate, then the type of the aggregate must be determinable
      --  from the context, excluding the aggregate itself but including the
      --  fact that the type of the aggregate must be a composite type.  The
      --  base type of the expression on the right-hand side must be the
      --  same as the base type of the aggregate.
      --
      --  GHDL: this means that the type can only be deduced from the
      --  expression (and not from the target).

      Target := Get_Target (Stmt);
      Stmt_Type := Wildcard_Any_Type;
      for S in Resolve_Stages loop
         Done := False;

         Target := Sem_Expression_Wildcard (Target, Stmt_Type);
         if Target = Null_Iir then
            Target_Type := Stmt_Type;
         else
            Set_Target (Stmt, Target);
            if Is_Expr_Fully_Analyzed (Target) then
               Check_Target (Stmt, Target);
               Done := True;
            end if;
            Target_Type := Get_Type (Target);
            Stmt_Type := Target_Type;
         end if;

         case Iir_Kinds_Variable_Assignment_Statement (Get_Kind (Stmt)) is
            when Iir_Kind_Variable_Assignment_Statement =>
               Expr := Get_Expression (Stmt);
               Expr := Sem_Expression_Wildcard (Expr, Stmt_Type);
               if Expr /= Null_Iir then
                  if Is_Expr_Fully_Analyzed (Expr) then
                     Check_Read (Expr);
                     Expr := Eval_Expr_If_Static (Expr);
                  end if;
                  Set_Expression (Stmt, Expr);
                  Merge_Wildcard_Type (Expr, Stmt_Type);
                  if Done
                    and then not Check_Implicit_Conversion (Target_Type, Expr)
                  then
                     Warning_Msg_Sem
                       (Warnid_Runtime_Error, +Stmt,
                        "expression length does not match target length");
                     Set_Expression (Stmt, Build_Overflow (Expr, Target_Type));
                  end if;
               end if;

            when Iir_Kind_Conditional_Variable_Assignment_Statement =>
               Expr := Get_Conditional_Expression (Stmt);
               Sem_Conditional_Expression (Expr, Stmt_Type);
         end case;

         exit when Done;
         if not Is_Defined_Type (Stmt_Type) then
            Error_Msg_Sem (+Stmt, "cannot resolve type");
            if Get_Kind (Target) = Iir_Kind_Aggregate then
               --  Try to give an advice.
               Error_Msg_Sem (+Stmt, "use a qualified expression for the RHS");
            end if;
            exit;
         end if;
      end loop;
   end Sem_Variable_Assignment;

   procedure Sem_Return_Statement (Stmt: Iir_Return_Statement) is
      Expr: Iir;
   begin
      if Current_Subprogram = Null_Iir then
         Error_Msg_Sem (+Stmt, "return statement not in a subprogram body");
         return;
      end if;
      Expr := Get_Expression (Stmt);
      case Get_Kind (Current_Subprogram) is
         when Iir_Kind_Procedure_Declaration =>
            if Expr /= Null_Iir then
               Error_Msg_Sem
                 (+Stmt, "return in a procedure can't have an expression");
            end if;
            return;
         when Iir_Kind_Function_Declaration =>
            if Expr = Null_Iir then
               Error_Msg_Sem
                 (+Stmt, "return in a function must have an expression");
               return;
            end if;
         when Iir_Kinds_Process_Statement =>
            Error_Msg_Sem (+Stmt, "return statement not allowed in a process");
            return;
         when others =>
            Error_Kind ("sem_return_statement", Stmt);
      end case;
      Set_Type (Stmt, Get_Return_Type (Current_Subprogram));
      Expr := Sem_Expression (Expr, Get_Return_Type (Current_Subprogram));
      if Expr /= Null_Iir then
         Check_Read (Expr);
         Set_Expression (Stmt, Eval_Expr_If_Static (Expr));
      end if;
   end Sem_Return_Statement;

   -- Sem for concurrent and sequential assertion statements.
   procedure Sem_Report_Statement (Stmt : Iir)
   is
      Expr : Iir;
   begin
      Expr := Get_Report_Expression (Stmt);
      if Expr /= Null_Iir then
         Expr := Sem_Expression (Expr, String_Type_Definition);
         Check_Read (Expr);
         Expr := Eval_Expr_If_Static (Expr);
         Set_Report_Expression (Stmt, Expr);
      end if;

      Expr := Get_Severity_Expression (Stmt);
      if Expr /= Null_Iir then
         Expr := Sem_Expression (Expr, Severity_Level_Type_Definition);
         Check_Read (Expr);
         Set_Severity_Expression (Stmt, Expr);
      end if;
   end Sem_Report_Statement;

   procedure Sem_Assertion_Statement (Stmt: Iir)
   is
      Expr : Iir;
   begin
      Expr := Get_Assertion_Condition (Stmt);
      Expr := Sem_Condition (Expr);
      Expr := Eval_Expr_If_Static (Expr);
      Set_Assertion_Condition (Stmt, Expr);

      Sem_Report_Statement (Stmt);
   end Sem_Assertion_Statement;

   --  Analyze a list of case choice LIST, and check for correct CHOICE type.
   procedure Sem_Case_Choices
     (Choice : Iir; Chain : in out Iir; Loc : Location_Type)
   is
      --  Check restrictions on the expression of a One-Dimensional Character
      --  Array Type (ODCAT) given by LRM 8.8
      --  Return FALSE in case of violation.
      function Check_Odcat_Expression (Expr : Iir) return Boolean
      is
         Expr_Type : constant Iir := Get_Type (Expr);
      begin
         --  LRM 8.8 Case Statement
         --  If the expression is of a one-dimensional character array type,
         --  then the expression must be one of the following:
         case Get_Kind (Expr) is
            when Iir_Kinds_Object_Declaration
              | Iir_Kind_Selected_Element =>
               --  FIXME: complete the list.
               --  * the name of an object whose subtype is locally static.
               if Get_Type_Staticness (Expr_Type) /= Locally then
                  Error_Msg_Sem
                    (+Choice, "object subtype is not locally static");
                  return False;
               end if;
            when Iir_Kind_Indexed_Name =>
               --  LRM93
               --  * an indexed name whose prefix is one of the members of
               --    this list and whose indexing expressions are locally
               --    static expression.
               if Flags.Vhdl_Std = Vhdl_87 then
                  Error_Msg_Sem
                    (+Expr, "indexed name not allowed here in vhdl87");
                  return False;
               end if;
               if not Check_Odcat_Expression (Get_Prefix (Expr)) then
                  return False;
               end if;
               --  GHDL: I don't understand why the indexing expressions
               --  must be locally static.  So I don't check this in 93c.
               if Flags.Vhdl_Std /= Vhdl_93c
                 and then
                 Get_Expr_Staticness (Get_First_Element
                                      (Get_Index_List (Expr))) /= Locally
               then
                  Error_Msg_Sem
                    (+Expr, "indexing expression must be locally static");
                  return False;
               end if;
            when Iir_Kind_Slice_Name =>
               --  LRM93
               --  * a slice name whose prefix is one of the members of this
               --    list and whose discrete range is a locally static
               --    discrete range.

               --  LRM87/INT1991 IR96
               --  then the expression must be either a slice name whose
               --  discrete range is locally static, or ..
               if False and Flags.Vhdl_Std = Vhdl_87 then
                  Error_Msg_Sem
                    (+Expr, "slice not allowed as case expression in vhdl87");
                  return False;
               end if;
               if not Check_Odcat_Expression (Get_Prefix (Expr)) then
                  return False;
               end if;
               if Get_Type_Staticness (Expr_Type) /= Locally then
                  Error_Msg_Sem
                    (+Expr, "slice discrete range must be locally static");
                  return False;
               end if;
            when Iir_Kind_Function_Call =>
               --  LRM93
               --  * a function call whose return type mark denotes a
               --    locally static subtype.
               if Flags.Vhdl_Std = Vhdl_87 then
                  Error_Msg_Sem
                    (+Expr, "function call not allowed here in vhdl87");
                  return False;
               end if;
               if Get_Type_Staticness (Expr_Type) /= Locally then
                  Error_Msg_Sem
                    (+Expr, "function call type is not locally static");
               end if;
            when Iir_Kind_Qualified_Expression
              | Iir_Kind_Type_Conversion =>
               --  * a qualified expression or type conversion whose type mark
               --    denotes a locally static subtype.
               if Get_Type_Staticness (Expr_Type) /= Locally then
                  Error_Msg_Sem
                    (+Expr, "type mark is not a locally static subtype");
                  return False;
               end if;
            when Iir_Kind_Simple_Name
              | Iir_Kind_Selected_Name =>
               return Check_Odcat_Expression (Get_Named_Entity (Expr));
            when others =>
               Error_Msg_Sem
                 (+Choice, "bad form of case expression (refer to LRM 8.8)");
               return False;
         end case;
         return True;
      end Check_Odcat_Expression;

      Choice_Type : Iir;
      Low, High : Iir;
      El_Type : Iir;
   begin
      --  LRM 8.8  Case Statement
      --  The expression must be of a discrete type, or of a one-dimensional
      --  array type whose element base type is a character type.
      Choice_Type := Get_Type (Choice);
      case Get_Kind (Choice_Type) is
         when Iir_Kinds_Discrete_Type_Definition =>
            Sem_Choices_Range
              (Chain, Choice_Type, False, True, Loc, Low, High);
         when Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Array_Type_Definition =>
            if not Is_One_Dimensional_Array_Type (Choice_Type) then
               Error_Msg_Sem
                 (+Choice,
                  "expression must be of a one-dimensional array type");
               return;
            end if;
            El_Type := Get_Base_Type (Get_Element_Subtype (Choice_Type));
            if Get_Kind (El_Type) /= Iir_Kind_Enumeration_Type_Definition then
               --  FIXME: check character.
               Error_Msg_Sem
                 (+Choice,
                  "element type of the expression must be a character type");
               return;
            end if;
            if not Check_Odcat_Expression (Choice) then
               return;
            end if;
            Sem_String_Choices_Range (Chain, Choice);
         when others =>
            Error_Msg_Sem (+Choice, "type of expression must be discrete");
      end case;
   end Sem_Case_Choices;

   procedure Sem_Case_Statement (Stmt: Iir_Case_Statement)
   is
      Expr: Iir;
      Chain : Iir;
      El: Iir;
   begin
      Expr := Get_Expression (Stmt);
      Chain := Get_Case_Statement_Alternative_Chain (Stmt);
      -- FIXME: overload.
      Expr := Sem_Case_Expression (Expr);
      if Expr /= Null_Iir then
         Check_Read (Expr);
         Set_Expression (Stmt, Expr);

         Sem_Case_Choices (Expr, Chain, Get_Location (Stmt));
         Set_Case_Statement_Alternative_Chain (Stmt, Chain);
      end if;

      El := Chain;
      while El /= Null_Iir loop
         Sem_Sequential_Statements_Internal (Get_Associated_Chain (El));
         El := Get_Chain (El);
      end loop;
   end Sem_Case_Statement;

   --  Sem the sensitivity list LIST.
   procedure Sem_Sensitivity_List (List: Iir_Designator_List)
   is
      El: Iir;
      Res: Iir;
      Prefix : Iir;
   begin
      if List = Iir_List_All then
         return;
      end if;

      for I in Natural loop
         -- El is an iir_identifier.
         El := Get_Nth_Element (List, I);
         exit when El = Null_Iir;

         Sem_Name (El);

         Res := Get_Named_Entity (El);
         if Res = Error_Mark then
            null;
         elsif Is_Overload_List (Res) or else not Is_Object_Name (Res) then
            Error_Msg_Sem (+El, "a sensitivity element must be a signal name");
         else
            Res := Finish_Sem_Name (El);
            Prefix := Get_Object_Prefix (Res);
            case Get_Kind (Prefix) is
               when Iir_Kind_Signal_Declaration
                 | Iir_Kind_Guard_Signal_Declaration
                 | Iir_Kinds_Signal_Attribute =>
                  null;
               when Iir_Kind_Interface_Signal_Declaration =>
                  if not Iir_Mode_Readable (Get_Mode (Prefix)) then
                     Error_Msg_Sem
                       (+El,
                        "%n of mode out can't be in a sensivity list", +Res);
                  end if;
               when others =>
                  Error_Msg_Sem (+El,
                                 "%n is neither a signal nor a port", +Res);
            end case;
            --  LRM 9.2
            --  Only static signal names (see section 6.1) for which reading
            --  is permitted may appear in the sensitivity list of a process
            --  statement.

            --  LRM 8.1  Wait statement
            --  Each signal name in the sensitivity list must be a static
            --  signal name, and each name must denote a signal for which
            --  reading is permitted.
            if Get_Name_Staticness (Res) < Globally then
               Error_Msg_Sem
                 (+El, "sensitivity element %n must be a static name", +Res);
            end if;

            Replace_Nth_Element (List, I, Res);
         end if;
      end loop;
   end Sem_Sensitivity_List;

   --  Mark STMT and its parents as suspendable.
   procedure Mark_Suspendable (Stmt : Iir)
   is
      Parent : Iir;
   begin
      Parent := Get_Parent (Stmt);
      loop
         case Get_Kind (Parent) is
            when Iir_Kind_Function_Body
              | Iir_Kind_Sensitized_Process_Statement =>
               exit;
            when Iir_Kind_Process_Statement
              | Iir_Kind_Procedure_Body =>
               Set_Suspend_Flag (Parent, True);
               exit;
            when Iir_Kind_If_Statement
              | Iir_Kind_While_Loop_Statement
              | Iir_Kind_For_Loop_Statement
              | Iir_Kind_Case_Statement =>
               Set_Suspend_Flag (Parent, True);
               Parent := Get_Parent (Parent);
            when others =>
               Error_Kind ("mark_suspendable", Parent);
         end case;
      end loop;
   end Mark_Suspendable;

   procedure Sem_Wait_Statement (Stmt: Iir_Wait_Statement)
   is
      Expr: Iir;
      Sensitivity_List : Iir_List;
   begin
      --  Check validity.
      case Get_Kind (Current_Subprogram) is
         when Iir_Kind_Process_Statement =>
            null;
         when Iir_Kind_Function_Declaration =>
            --  LRM93 8.2
            --  It is an error if a wait statement appears in a function
            --  subprogram [...]
            Error_Msg_Sem
              (+Stmt, "wait statement not allowed in a function subprogram");
            return;
         when Iir_Kind_Procedure_Declaration =>
            --  LRM93 8.2
            --  [It is an error ...] or in a procedure that has a parent that
            --  is a function subprogram.
            --  LRM93 8.2
            --  [...] or in a procedure that has a parent that is such a
            --  process statement.
            -- GHDL: this is checked at the end of analysis or during
            --  elaboration.
            Set_Wait_State (Current_Subprogram, True);
         when Iir_Kind_Sensitized_Process_Statement =>
            --  LRM93 8.2
            --  Furthermore, it is an error if a wait statement appears in an
            --  explicit process statement that includes a sensitivity list,
            --  [...]
            Error_Msg_Sem
              (+Stmt, "wait statement not allowed in a sensitized process");
            return;
         when others =>
            raise Internal_Error;
      end case;

      Sensitivity_List := Get_Sensitivity_List (Stmt);
      if Sensitivity_List /= Null_Iir_List then
         Sem_Sensitivity_List (Sensitivity_List);
      end if;

      Expr := Get_Condition_Clause (Stmt);
      if Expr /= Null_Iir then
         Expr := Sem_Condition (Expr);
         Set_Condition_Clause (Stmt, Expr);
      end if;

      Expr := Get_Timeout_Clause (Stmt);
      if Expr /= Null_Iir then
         Expr := Sem_Expression (Expr, Time_Type_Definition);
         if Expr /= Null_Iir then
            Check_Read (Expr);
            Expr := Eval_Expr_If_Static (Expr);
            Set_Timeout_Clause (Stmt, Expr);
            if Get_Expr_Staticness (Expr) = Locally
              and then Get_Value (Expr) < 0
            then
               Error_Msg_Sem (+Stmt, "timeout value must be positive");
            end if;
         end if;
      end if;

      Mark_Suspendable (Stmt);
   end Sem_Wait_Statement;

   procedure Sem_Exit_Next_Statement (Stmt : Iir)
   is
      Cond: Iir;
      Loop_Label : Iir;
      Loop_Stmt: Iir;
      P : Iir;
   begin
      Cond := Get_Condition (Stmt);
      if Cond /= Null_Iir then
         Cond := Sem_Condition (Cond);
         Set_Condition (Stmt, Cond);
      end if;

      Loop_Label := Get_Loop_Label (Stmt);
      if Loop_Label /= Null_Iir then
         Loop_Label := Sem_Denoting_Name (Loop_Label);
         Set_Loop_Label (Stmt, Loop_Label);
         Loop_Stmt := Get_Named_Entity (Loop_Label);
         case Get_Kind (Loop_Stmt) is
            when Iir_Kind_For_Loop_Statement
              | Iir_Kind_While_Loop_Statement =>
               null;
            when others =>
               Error_Class_Match (Loop_Label, "loop statement");
               Loop_Stmt := Null_Iir;
         end case;
      else
         Loop_Stmt := Null_Iir;
      end if;

      --  Check the current statement is inside the labeled loop.
      P := Stmt;
      loop
         P := Get_Parent (P);
         case Get_Kind (P) is
            when Iir_Kind_While_Loop_Statement
              | Iir_Kind_For_Loop_Statement =>
               if Loop_Stmt = Null_Iir or else P = Loop_Stmt then
                  exit;
               end if;
            when Iir_Kind_If_Statement
              | Iir_Kind_Elsif
              | Iir_Kind_Case_Statement =>
               null;
            when others =>
               --  FIXME: should emit a message for label mismatch.
               Error_Msg_Sem (+Stmt, "exit/next must be inside a loop");
               exit;
         end case;
      end loop;
   end Sem_Exit_Next_Statement;

   -- Process is the scope, this is also the process for which drivers can
   -- be created.
   procedure Sem_Sequential_Statements_Internal (First_Stmt : Iir)
   is
      Stmt: Iir;
   begin
      Stmt := First_Stmt;
      while Stmt /= Null_Iir loop
         case Get_Kind (Stmt) is
            when Iir_Kind_Null_Statement =>
               null;
            when Iir_Kind_If_Statement =>
               declare
                  Clause: Iir := Stmt;
                  Cond: Iir;
               begin
                  while Clause /= Null_Iir loop
                     Cond := Get_Condition (Clause);
                     if Cond /= Null_Iir then
                        Cond := Sem_Condition (Cond);
                        Set_Condition (Clause, Cond);
                     end if;
                     Sem_Sequential_Statements_Internal
                       (Get_Sequential_Statement_Chain (Clause));
                     Clause := Get_Else_Clause (Clause);
                  end loop;
               end;
            when Iir_Kind_For_Loop_Statement =>
               declare
                  Iterator: Iir;
               begin
                  --  LRM 10.1 Declarative region
                  --  9. A loop statement.
                  Open_Declarative_Region;

                  Set_Is_Within_Flag (Stmt, True);
                  Iterator := Get_Parameter_Specification (Stmt);
                  Sem_Scopes.Add_Name (Iterator);
                  Sem_Iterator (Iterator, None);
                  Set_Visible_Flag (Iterator, True);
                  Sem_Sequential_Statements_Internal
                    (Get_Sequential_Statement_Chain (Stmt));
                  Set_Is_Within_Flag (Stmt, False);

                  Close_Declarative_Region;
               end;
            when Iir_Kind_While_Loop_Statement =>
               declare
                  Cond: Iir;
               begin
                  Cond := Get_Condition (Stmt);
                  if Cond /= Null_Iir then
                     Cond := Sem_Condition (Cond);
                     Set_Condition (Stmt, Cond);
                  end if;
                  Sem_Sequential_Statements_Internal
                    (Get_Sequential_Statement_Chain (Stmt));
               end;
            when Iir_Kind_Simple_Signal_Assignment_Statement
              | Iir_Kind_Conditional_Signal_Assignment_Statement =>
               Sem_Signal_Assignment (Stmt);
               if Current_Concurrent_Statement /= Null_Iir and then
                 Get_Kind (Current_Concurrent_Statement)
                 in Iir_Kinds_Process_Statement
                 and then Get_Passive_Flag (Current_Concurrent_Statement)
               then
                  Error_Msg_Sem
                    (+Stmt, "signal statement forbidden in passive process");
               end if;
            when Iir_Kind_Variable_Assignment_Statement
              | Iir_Kind_Conditional_Variable_Assignment_Statement =>
               Sem_Variable_Assignment (Stmt);
            when Iir_Kind_Return_Statement =>
               Sem_Return_Statement (Stmt);
            when Iir_Kind_Assertion_Statement =>
               Sem_Assertion_Statement (Stmt);
            when Iir_Kind_Report_Statement =>
               Sem_Report_Statement (Stmt);
            when Iir_Kind_Case_Statement =>
               Sem_Case_Statement (Stmt);
            when Iir_Kind_Wait_Statement =>
               Sem_Wait_Statement (Stmt);
            when Iir_Kind_Procedure_Call_Statement =>
               declare
                  Call : constant Iir := Get_Procedure_Call (Stmt);
                  Imp : Iir;
               begin
                  Sem_Procedure_Call (Call, Stmt);

                  --  Set suspend flag, if calling a suspendable procedure
                  --  from a procedure or from a process.
                  Imp := Get_Implementation (Call);
                  if Imp /= Null_Iir
                    and then Get_Kind (Imp) = Iir_Kind_Procedure_Declaration
                    and then Get_Suspend_Flag (Imp)
                    and then (Get_Kind (Get_Current_Subprogram)
                                /= Iir_Kind_Function_Declaration)
                    and then (Get_Kind (Get_Current_Subprogram)
                                /= Iir_Kind_Sensitized_Process_Statement)
                  then
                     Set_Suspend_Flag (Stmt, True);
                     Mark_Suspendable (Stmt);
                  end if;
               end;
            when Iir_Kind_Next_Statement
              | Iir_Kind_Exit_Statement =>
               Sem_Exit_Next_Statement (Stmt);
            when others =>
               Error_Kind ("sem_sequential_statements_Internal", Stmt);
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Sem_Sequential_Statements_Internal;

   procedure Sem_Sequential_Statements (Decl : Iir; Body_Parent : Iir)
   is
      Outer_Subprogram: Iir;
   begin
      Outer_Subprogram := Current_Subprogram;
      Current_Subprogram := Decl;

      -- Sem declarations
      Sem_Sequential_Labels (Get_Sequential_Statement_Chain (Body_Parent));
      Sem_Declaration_Chain (Body_Parent);
      Sem_Specification_Chain (Body_Parent, Null_Iir);

      -- Sem statements.
      Sem_Sequential_Statements_Internal
        (Get_Sequential_Statement_Chain (Body_Parent));

      Check_Full_Declaration (Body_Parent, Body_Parent);

      Current_Subprogram := Outer_Subprogram;
   end Sem_Sequential_Statements;

   --  Sem the instantiated unit of STMT and return the node constaining
   --  ports and generics (either a entity_declaration or a component
   --  declaration).
   function Sem_Instantiated_Unit
     (Stmt : Iir_Component_Instantiation_Statement) return Iir
   is
      Inst : constant Iir := Get_Instantiated_Unit (Stmt);
      Comp_Name : Iir;
      Comp : Iir;
   begin
      if Get_Kind (Inst) in Iir_Kinds_Denoting_Name then
         Comp := Get_Named_Entity (Inst);
         if Comp /= Null_Iir then
            --  Already analyzed before, while trying to separate
            --  concurrent procedure calls from instantiation stmts.
            pragma Assert (Get_Kind (Comp) = Iir_Kind_Component_Declaration);
            return Comp;
         end if;
         --  The component may be an entity or a configuration.
         Comp_Name := Sem_Denoting_Name (Inst);
         Set_Instantiated_Unit (Stmt, Comp_Name);
         Comp := Get_Named_Entity (Comp_Name);
         if Get_Kind (Comp) /= Iir_Kind_Component_Declaration then
            Error_Class_Match (Comp_Name, "component");
            return Null_Iir;
         end if;
         return Comp;
      else
         return Sem_Entity_Aspect (Inst);
      end if;
   end Sem_Instantiated_Unit;

   procedure Sem_Component_Instantiation_Statement
     (Stmt: Iir_Component_Instantiation_Statement; Is_Passive : Boolean)
   is
      Decl : Iir;
      Entity_Unit : Iir_Design_Unit;
      Bind : Iir_Binding_Indication;
   begin
      --  FIXME: move this check in parse ?
      if Is_Passive then
         Error_Msg_Sem (+Stmt, "component instantiation forbidden in entity");
      end if;

      -- Check for label.
      --  This cannot be moved in parse since a procedure_call may be revert
      --  into a component instantiation.
      if Get_Label (Stmt) = Null_Identifier then
         Error_Msg_Sem (+Stmt, "component instantiation requires a label");
      end if;

      --  Look for the component.
      Decl := Sem_Instantiated_Unit (Stmt);
      if Decl = Null_Iir then
         return;
      end if;

      -- The association
      Sem_Generic_Port_Association_Chain (Decl, Stmt);

      --  FIXME: add sources for signals, in order to detect multiple sources
      --  to unresolved signals.
      --  What happen if the component is not bound ?

      --  Create a default binding indication if necessary.
      if Get_Component_Configuration (Stmt) = Null_Iir
        and then Get_Kind (Decl) = Iir_Kind_Component_Declaration
      then
         Entity_Unit := Get_Visible_Entity_Declaration (Decl);
         if Entity_Unit = Null_Iir then
            if Is_Warning_Enabled (Warnid_Default_Binding)
              and then not Flags.Flag_Elaborate
            then
               Warning_Msg_Sem
                 (Warnid_Default_Binding, +Stmt,
                  "no default binding for instantiation of %n", +Decl);
               Explain_No_Visible_Entity (Decl);
            end if;
         elsif Flags.Flag_Elaborate
           and then (Flags.Flag_Elaborate_With_Outdated
                     or else Get_Date (Entity_Unit) in Date_Valid)
         then
            Bind := Sem_Create_Default_Binding_Indication
              (Decl, Entity_Unit, Stmt, False);
            Set_Default_Binding_Indication (Stmt, Bind);
         end if;
      end if;
   end Sem_Component_Instantiation_Statement;

   --  Note: a statement such as
   --    label1: name;
   --  can be parsed as a procedure call statement or as a
   --  component instantiation statement.
   --  Check now and revert in case of error.
   function Sem_Concurrent_Procedure_Call_Statement
     (Stmt : Iir; Is_Passive : Boolean) return Iir
   is
      Call : Iir_Procedure_Call;
      Decl : Iir;
      Label : Name_Id;
      N_Stmt : Iir_Component_Instantiation_Statement;
      Imp : Iir;
   begin
      Call := Get_Procedure_Call (Stmt);
      if Get_Parameter_Association_Chain (Call) = Null_Iir then
         Imp := Get_Prefix (Call);
         Sem_Name (Imp);
         Set_Prefix (Call, Imp);

         Decl := Get_Named_Entity (Imp);
         if Get_Kind (Decl) = Iir_Kind_Component_Declaration then
            N_Stmt := Create_Iir (Iir_Kind_Component_Instantiation_Statement);
            Label := Get_Label (Stmt);
            Set_Label (N_Stmt, Label);
            Set_Parent (N_Stmt, Get_Parent (Stmt));
            Set_Instantiated_Unit (N_Stmt, Finish_Sem_Name (Imp));
            Location_Copy (N_Stmt, Stmt);

            if Label /= Null_Identifier then
               --  A component instantiation statement must have
               --  a label, this condition is checked during the
               --  sem of the statement.
               Sem_Scopes.Replace_Name (Label, Stmt, N_Stmt);
            end if;

            Free_Iir (Stmt);
            Free_Iir (Call);

            Sem_Component_Instantiation_Statement (N_Stmt, Is_Passive);
            return N_Stmt;
         end if;
      end if;
      Sem_Procedure_Call (Call, Stmt);

      if Is_Passive then
         Imp := Get_Implementation (Call);
         if Imp /= Null_Iir
           and then Get_Kind (Imp) = Iir_Kind_Procedure_Declaration
         then
            Decl := Get_Interface_Declaration_Chain (Imp);
            while Decl /= Null_Iir loop
               if Get_Mode (Decl) in Iir_Out_Modes then
                  Error_Msg_Sem (+Stmt, "%n is not passive", +Imp);
                  exit;
               end if;
               Decl := Get_Chain (Decl);
            end loop;
         end if;
      end if;

      return Stmt;
   end Sem_Concurrent_Procedure_Call_Statement;

   procedure Sem_Block_Statement (Stmt: Iir_Block_Statement)
   is
      Expr: Iir;
      Guard : Iir_Guard_Signal_Declaration;
      Header : Iir_Block_Header;
      Generic_Chain : Iir;
      Port_Chain : Iir;
   begin
      --  LRM 10.1 Declarative region.
      --  7. A block statement.
      Open_Declarative_Region;

      Set_Is_Within_Flag (Stmt, True);

      Header := Get_Block_Header (Stmt);
      if Header /= Null_Iir then
         Generic_Chain := Get_Generic_Chain (Header);
         Sem_Interface_Chain (Generic_Chain, Generic_Interface_List);
         Port_Chain := Get_Port_Chain (Header);
         Sem_Interface_Chain (Port_Chain, Port_Interface_List);

         --  LRM 9.1
         --  Such actuals are evaluated in the context of the enclosing
         --  declarative region.
         --  GHDL: close the declarative region...
         Set_Is_Within_Flag (Stmt, False);
         Close_Declarative_Region;

         Sem_Generic_Port_Association_Chain (Header, Header);

         --  ... and reopen-it.
         Open_Declarative_Region;
         Set_Is_Within_Flag (Stmt, True);
         Add_Declarations_From_Interface_Chain (Generic_Chain);
         Add_Declarations_From_Interface_Chain (Port_Chain);
      end if;

      --  LRM93 9.1
      --  If a guard expression appears after the reserved word BLOCK, then a
      --  signal with the simple name GUARD of predefined type BOOLEAN is
      --  implicitly declared at the beginning of the declarative part of the
      --  block, and the guard expression defined the value of that signal at
      --  any given time.
      Guard := Get_Guard_Decl (Stmt);
      if Guard /= Null_Iir then
         --  LRM93 9.1
         --  The type of the guard expression must be type BOOLEAN.
         --  GHDL: guard expression must be analyzed before creating the
         --   implicit GUARD signal, since the expression may reference GUARD.
         Set_Expr_Staticness (Guard, None);
         Set_Name_Staticness (Guard, Locally);
         Expr := Get_Guard_Expression (Guard);
         Expr := Sem_Condition (Expr);
         if Expr /= Null_Iir then
            Set_Guard_Expression (Guard, Expr);
         end if;

         --  FIXME: should extract sensivity now and set the has_active flag
         --  on signals, since the guard expression is evaluated when one of
         --  its signal is active.  However, how can a bug be introduced by
         --  evaluating only when signals have events ?

         --  the guard expression is an implicit definition of a signal named
         --  GUARD.  Create this definition.  This is necessary for the type.
         Set_Identifier (Guard, Std_Names.Name_Guard);
         Set_Type (Guard, Boolean_Type_Definition);
         Set_Block_Statement (Guard, Stmt);
         Sem_Scopes.Add_Name (Guard);
         Set_Visible_Flag (Guard, True);
      end if;

      Sem_Block (Stmt, True);
      Set_Is_Within_Flag (Stmt, False);
      Close_Declarative_Region;
   end Sem_Block_Statement;

   procedure Sem_Generate_Statement_Body (Bod : Iir) is
   begin
      Set_Is_Within_Flag (Bod, True);
      Sem_Block (Bod, True); -- Flags.Vhdl_Std /= Vhdl_87);
      Set_Is_Within_Flag (Bod, False);
   end Sem_Generate_Statement_Body;

   procedure Sem_For_Generate_Statement (Stmt : Iir)
   is
      Param : Iir;
   begin
      --  LRM93 10.1 Declarative region.
      --  12. A generate statement.
      Open_Declarative_Region;
      Set_Is_Within_Flag (Stmt, True);

      Param := Get_Parameter_Specification (Stmt);
      Sem_Scopes.Add_Name (Param);
      --  LRM93 7.4.2 (Globally Static Primaries)
      --   4. a generate parameter;
      Sem_Iterator (Param, Globally);
      Set_Visible_Flag (Param, True);
      --  LRM93 9.7
      --  The discrete range in a generation scheme of the first form must
      --  be a static discrete range;
      if Get_Type (Param) /= Null_Iir
        and then Get_Type_Staticness (Get_Type (Param)) < Globally
      then
         Error_Msg_Sem (+Stmt, "range must be a static discrete range");
      end if;

      --  In the same declarative region.
      Sem_Generate_Statement_Body (Get_Generate_Statement_Body (Stmt));

      Set_Is_Within_Flag (Stmt, True);
      Close_Declarative_Region;
   end Sem_For_Generate_Statement;

   procedure Sem_If_Case_Generate_Statement_Body (Bod : Iir)
   is
      Alt_Label : Name_Id;
   begin
      Alt_Label := Get_Alternative_Label (Bod);
      if Alt_Label /= Null_Identifier then
         --  Declare label.  This doesn't appear in the LRM (bug ?), but
         --  used here to detect duplicated labels.
         Sem_Scopes.Add_Name (Bod);
         Xref_Decl (Bod);
      end if;

      --  Contrary to the LRM, a new declarative region is declared.  This
      --  is required so that declarations in a generate statement body are
      --  not in the scope of the following generate bodies.
      Open_Declarative_Region;
      Sem_Generate_Statement_Body (Bod);
      Close_Declarative_Region;
   end Sem_If_Case_Generate_Statement_Body;

   procedure Sem_If_Generate_Statement (Stmt : Iir)
   is
      Clause : Iir;
      Condition : Iir;
   begin
      --  LRM93 10.1 Declarative region.
      --  12. A generate statement.
      Open_Declarative_Region;
      Set_Is_Within_Flag (Stmt, True);

      Clause := Stmt;
      while Clause /= Null_Iir loop
         Condition := Get_Condition (Clause);

         if Condition /= Null_Iir then
            Condition := Sem_Condition (Condition);
            --  LRM93 9.7
            --  the condition in a generation scheme of the second form must be
            --  a static expression.
            if Condition /= Null_Iir
              and then Get_Expr_Staticness (Condition) < Globally
            then
               Error_Msg_Sem
                 (+Condition, "condition must be a static expression");
            else
               Set_Condition (Clause, Condition);
            end if;
         else
            --  No condition for the last 'else' part.
            pragma Assert (Get_Generate_Else_Clause (Clause) = Null_Iir);
            null;
         end if;

         Sem_If_Case_Generate_Statement_Body
           (Get_Generate_Statement_Body (Clause));

         Clause := Get_Generate_Else_Clause (Clause);
      end loop;

      Set_Is_Within_Flag (Stmt, False);
      Close_Declarative_Region;
   end Sem_If_Generate_Statement;

   procedure Sem_Case_Generate_Statement (Stmt : Iir)
   is
      Expr : Iir;
      Chain : Iir;
      El : Iir;
   begin
      --  LRM93 10.1 Declarative region.
      --  12. A generate statement.
      Open_Declarative_Region;
      Set_Is_Within_Flag (Stmt, True);

      Expr := Get_Expression (Stmt);
      Chain := Get_Case_Statement_Alternative_Chain (Stmt);
      -- FIXME: overload.
      Expr := Sem_Case_Expression (Expr);
      if Expr /= Null_Iir then
         Check_Read (Expr);
         Set_Expression (Stmt, Expr);

         if Get_Expr_Staticness (Expr) < Globally then
            Error_Msg_Sem
              (+Expr, "case expression must be a static expression");
         end if;

         Sem_Case_Choices (Expr, Chain, Get_Location (Stmt));
         Set_Case_Statement_Alternative_Chain (Stmt, Chain);
      end if;

      El := Chain;
      while El /= Null_Iir loop
         if not Get_Same_Alternative_Flag (El) then
            Sem_If_Case_Generate_Statement_Body (Get_Associated_Block (El));
         end if;
         El := Get_Chain (El);
      end loop;

      Set_Is_Within_Flag (Stmt, False);
      Close_Declarative_Region;
   end Sem_Case_Generate_Statement;

   procedure Sem_Process_Statement (Proc: Iir) is
   begin
      Set_Is_Within_Flag (Proc, True);

      --  LRM 10.1
      --  8. A process statement
      Open_Declarative_Region;

      -- Sem declarations
      Sem_Sequential_Statements (Proc, Proc);

      Close_Declarative_Region;

      Set_Is_Within_Flag (Proc, False);

      if Get_Kind (Proc) = Iir_Kind_Sensitized_Process_Statement
        and then Get_Callees_List (Proc) /= Null_Iir_List
      then
         --  Check there is no wait statement in subprograms called.
         --  Also in the case of all-sensitized process, check that package
         --  subprograms don't read signals.
         Sem.Add_Analysis_Checks_List (Proc);
      end if;
   end Sem_Process_Statement;

   procedure Sem_Sensitized_Process_Statement
     (Proc: Iir_Sensitized_Process_Statement) is
   begin
      Sem_Sensitivity_List (Get_Sensitivity_List (Proc));
      Sem_Process_Statement (Proc);
   end Sem_Sensitized_Process_Statement;

   procedure Sem_Concurrent_Selected_Signal_Assignment (Stmt: Iir)
   is
      Expr: Iir;
      Chain : Iir;
   begin
      --  LRM 9.5  Concurrent Signal Assgnment Statements.
      --  The process statement equivalent to a concurrent signal assignment
      --  statement [...] is constructed as follows: [...]
      --
      --  LRM 9.5.2  Selected Signal Assignment
      --  The characteristics of the selected expression, the waveforms and
      --  the choices in the selected assignment statement must be such that
      --  the case statement in the equivalent statement is a legal
      --  statement

      --  Target and waveforms.
      Sem_Signal_Assignment (Stmt);

      --  The choices.
      Chain := Get_Selected_Waveform_Chain (Stmt);
      Expr := Sem_Case_Expression (Get_Expression (Stmt));
      if Expr /= Null_Iir then
         Check_Read (Expr);
         Set_Expression (Stmt, Expr);
         Sem_Case_Choices (Expr, Chain, Get_Location (Stmt));
         Set_Selected_Waveform_Chain (Stmt, Chain);
      end if;

      Sem_Guard (Stmt);
   end Sem_Concurrent_Selected_Signal_Assignment;

   procedure Simple_Simultaneous_Statement (Stmt : Iir) is
      Left, Right : Iir;
      Res_Type : Iir;
   begin
      Left := Get_Simultaneous_Left (Stmt);
      Right := Get_Simultaneous_Right (Stmt);

      Left := Sem_Expression_Ov (Left, Null_Iir);
      Right := Sem_Expression_Ov (Right, Null_Iir);

      --  Give up in case of error
      if Left = Null_Iir or else Right = Null_Iir then
         return;
      end if;

      Res_Type := Search_Compatible_Type (Get_Type (Left), Get_Type (Right));
      if Res_Type = Null_Iir then
         Error_Msg_Sem
           (+Stmt, "types of left and right expressions are incompatible");
         return;
      end if;

      --  FIXME: check for nature type...
   end Simple_Simultaneous_Statement;

   procedure Sem_Concurrent_Statement_Chain (Parent : Iir)
   is
      Is_Passive : constant Boolean :=
        Get_Kind (Parent) = Iir_Kind_Entity_Declaration;
      El: Iir;
      New_El : Iir;
      Next_El : Iir;

      procedure No_Generate_Statement is
      begin
         if Is_Passive then
            Error_Msg_Sem (+El, "generate statement forbidden in entity");
         end if;
      end No_Generate_Statement;

      Prev_El : Iir;
      Prev_Concurrent_Statement : Iir;
      Prev_Psl_Default_Clock : Iir;
   begin
      Prev_Concurrent_Statement := Current_Concurrent_Statement;
      Prev_Psl_Default_Clock := Current_Psl_Default_Clock;

      El := Get_Concurrent_Statement_Chain (Parent);
      Prev_El := Null_Iir;
      while El /= Null_Iir loop
         Current_Concurrent_Statement := El;
         New_El := El;
         Next_El := Get_Chain (El);

         case Get_Kind (El) is
            when Iir_Kind_Concurrent_Simple_Signal_Assignment
              | Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
               if Is_Passive then
                  Error_Msg_Sem (+El, "signal assignment forbidden in entity");
               end if;
               Sem_Signal_Assignment (El);
            when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
               if Is_Passive then
                  Error_Msg_Sem (+El, "signal assignment forbidden in entity");
               end if;
               Sem_Concurrent_Selected_Signal_Assignment (El);
            when Iir_Kind_Sensitized_Process_Statement =>
               Set_Passive_Flag (El, Is_Passive);
               Sem_Sensitized_Process_Statement (El);
            when Iir_Kind_Process_Statement =>
               Set_Passive_Flag (El, Is_Passive);
               Sem_Process_Statement (El);
            when Iir_Kind_Component_Instantiation_Statement =>
               Sem_Component_Instantiation_Statement (El, Is_Passive);
            when Iir_Kind_Concurrent_Assertion_Statement =>
               --  FIXME: must check assertion expressions does not contain
               --  non-passive subprograms ??
               Sem_Assertion_Statement (El);
            when Iir_Kind_Block_Statement =>
               if Is_Passive then
                  Error_Msg_Sem (+El, "block forbidden in entity");
               end if;
               Sem_Block_Statement (El);
            when Iir_Kind_If_Generate_Statement =>
               No_Generate_Statement;
               Sem_If_Generate_Statement (El);
            when Iir_Kind_For_Generate_Statement =>
               No_Generate_Statement;
               Sem_For_Generate_Statement (El);
            when Iir_Kind_Case_Generate_Statement =>
               No_Generate_Statement;
               Sem_Case_Generate_Statement (El);
            when Iir_Kind_Concurrent_Procedure_Call_Statement =>
               New_El := Sem_Concurrent_Procedure_Call_Statement
                 (El, Is_Passive);
            when Iir_Kind_Psl_Declaration =>
               Sem_Psl.Sem_Psl_Declaration (El);
            when Iir_Kind_Psl_Endpoint_Declaration =>
               Sem_Psl.Sem_Psl_Endpoint_Declaration (El);
            when Iir_Kind_Psl_Assert_Statement =>
               New_El := Sem_Psl.Sem_Psl_Assert_Statement (El);
            when Iir_Kind_Psl_Cover_Statement =>
               Sem_Psl.Sem_Psl_Cover_Statement (El);
            when Iir_Kind_Psl_Default_Clock =>
               Sem_Psl.Sem_Psl_Default_Clock (El);
            when Iir_Kind_Simple_Simultaneous_Statement =>
               Simple_Simultaneous_Statement (El);
            when others =>
               Error_Kind ("sem_concurrent_statement_chain", El);
         end case;

         if New_El /= El then
            --  Replace this node.
            if Prev_El = Null_Iir then
               Set_Concurrent_Statement_Chain (Parent, New_El);
            else
               Set_Chain (Prev_El, New_El);
            end if;
            Set_Chain (New_El, Next_El);
            Set_Parent (New_El, Parent);
            Prev_El := New_El;
         else
            Prev_El := El;
            pragma Assert (Get_Parent (El) = Parent);
         end if;

         El := Next_El;
      end loop;

      Current_Concurrent_Statement := Prev_Concurrent_Statement;
      Current_Psl_Default_Clock := Prev_Psl_Default_Clock;
   end Sem_Concurrent_Statement_Chain;

   --  Put labels in declarative region.
   procedure Sem_Labels_Chain (Parent : Iir)
   is
      Stmt: Iir;
      Label: Name_Id;
   begin
      Stmt := Get_Concurrent_Statement_Chain (Parent);
      while Stmt /= Null_Iir loop

         case Get_Kind (Stmt) is
            when Iir_Kind_Psl_Declaration
              | Iir_Kind_Psl_Endpoint_Declaration =>
               --  Special case for in-lined PSL declarations.
               null;
            when others =>
               Label := Get_Label (Stmt);

               if Label /= Null_Identifier then
                  Sem_Scopes.Add_Name (Stmt);
                  Name_Visible (Stmt);
                  Xref_Decl (Stmt);
               end if;
         end case;

         --  INT-1991/issue report 27
         --  Generate statements represent declarative region and have
         --  implicit declarative part.
         if False
           and then Flags.Vhdl_Std = Vhdl_87
           and then
           (Get_Kind (Stmt) = Iir_Kind_For_Generate_Statement
              or else Get_Kind (Stmt) = Iir_Kind_If_Generate_Statement)
         then
            Sem_Labels_Chain (Stmt);
         end if;

         Stmt := Get_Chain (Stmt);
      end loop;
   end Sem_Labels_Chain;

   procedure Sem_Block (Blk: Iir; Sem_Decls : Boolean)
   is
      Implicit : Implicit_Signal_Declaration_Type;
   begin
      Push_Signals_Declarative_Part (Implicit, Blk);

      if Sem_Decls then
         Sem_Labels_Chain (Blk);
         Sem_Declaration_Chain (Blk);
      end if;

      Sem_Concurrent_Statement_Chain (Blk);

      if Sem_Decls then
         --  FIXME: do it only if there is conf. spec. in the declarative
         --  part.
         Sem_Specification_Chain (Blk, Blk);
         Check_Full_Declaration (Blk, Blk);
      end if;

      Pop_Signals_Declarative_Part (Implicit);
   end Sem_Block;

   --  Add a driver for SIG.
   --  STMT is used in case of error (it is the statement that creates the
   --   driver).
   --  Do nothing if:
   --    The current statement list does not belong to a process,
   --    SIG is a formal signal interface.
   procedure Sem_Add_Driver (Sig : Iir; Stmt : Iir)
   is
      Sig_Object : Iir;
      Sig_Object_Type : Iir;
   begin
      if Sig = Null_Iir then
         return;
      end if;
      Sig_Object := Get_Object_Prefix (Sig);
      Sig_Object_Type := Get_Type (Sig_Object);

      --  LRM 4.3.1.2 Signal Declaration
      --  It is an error if, after the elaboration of a description, a
      --  signal has multiple sources and it is not a resolved signal.

      --  Check for multiple driver for a unresolved signal declaration.
      --  Do this only if the object is a non-composite signal declaration.
      --  NOTE: THIS IS DISABLED, since the assignment may be within a
      --  generate statement.
      if False
        and then Get_Kind (Sig_Object) = Iir_Kind_Signal_Declaration
        and then Get_Kind (Sig_Object_Type)
        not in Iir_Kinds_Composite_Type_Definition
        and then not Get_Resolved_Flag (Sig_Object_Type)
      then
         if Get_Signal_Driver (Sig_Object) /= Null_Iir and then
           Get_Signal_Driver (Sig_Object) /= Current_Concurrent_Statement
         then
            Error_Msg_Sem (+Stmt, "unresolved %n has already a driver at %l",
                           (+Sig_Object, +Get_Signal_Driver (Sig_Object)));
         else
            Set_Signal_Driver (Sig_Object, Current_Concurrent_Statement);
         end if;
      end if;

      --  LRM 8.4.1
      --  If a given procedure is declared by a declarative item that is not
      --  contained within a process statement, and if a signal assignment
      --  statement appears in that procedure, then the target of the
      --  assignment statement must be a formal parameter of the given
      --  procedure or of a parent of that procedure, or an aggregate of such
      --  formal parameters.
      --  Similarly, if a given procedure is declared by a declarative item
      --  that is not contained within a process statement and if a signal is
      --  associated with an INOUT or OUT mode signal parameter in a
      --  subprogram call within that procedure, then the signal so associated
      --  must be a formal parameter of the given procedure or of a parent of
      --  that procedure.
      if Current_Concurrent_Statement = Null_Iir
        or else (Get_Kind (Current_Concurrent_Statement)
                 not in Iir_Kinds_Process_Statement)
      then
         --  Not within a process statement.
         if Current_Subprogram = Null_Iir then
            --  not within a subprogram: concurrent statement.
            return;
         end if;

         --  Within a subprogram.
         if Get_Kind (Sig_Object) = Iir_Kind_Signal_Declaration
           or else (Get_Kind (Get_Parent (Sig_Object))
                    /= Iir_Kind_Procedure_Declaration)
         then
            Error_Msg_Sem (+Stmt, "%n is not a formal parameter", +Sig_Object);
         end if;
      end if;
   end Sem_Add_Driver;
end Sem_Stmts;
