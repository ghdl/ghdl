--  Semantic analysis pass for PSL.
--  Copyright (C) 2009 Tristan Gingold
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

with Types; use Types;
with PSL.Nodes; use PSL.Nodes;
with PSL.Subsets;
with PSL.Hash;

with Sem_Expr;
with Sem_Stmts; use Sem_Stmts;
with Sem_Scopes;
with Sem_Names;
with Std_Names;
with Iirs_Utils; use Iirs_Utils;
with Evaluation; use Evaluation;
with Std_Package;
with Ieee.Std_Logic_1164;
with Errorout; use Errorout;
with Xrefs; use Xrefs;

package body Sem_Psl is
   procedure Sem_Psl_Directive_Clock (Stmt : Iir; Prop : in out Node);

   --  Return TRUE iff Atype is a PSL boolean type.
   --  See PSL1.1 5.1.2  Boolean expressions
   function Is_Psl_Bool_Type (Atype : Iir) return Boolean
   is
      Btype : Iir;
   begin
      if Atype = Null_Iir then
         return False;
      end if;
      Btype := Get_Base_Type (Atype);
      return Btype = Std_Package.Boolean_Type_Definition
        or else Btype = Std_Package.Bit_Type_Definition
        or else Btype = Ieee.Std_Logic_1164.Std_Ulogic_Type;
   end Is_Psl_Bool_Type;

   --  Return TRUE if EXPR type is a PSL boolean type.
   function Is_Psl_Bool_Expr (Expr : Iir) return Boolean is
   begin
      return Is_Psl_Bool_Type (Get_Type (Expr));
   end Is_Psl_Bool_Expr;

   --  Convert VHDL and/or/not nodes to PSL nodes.
   function Convert_Bool (Expr : Iir) return Node
   is
      use Std_Names;
      Impl : Iir;
   begin
      case Get_Kind (Expr) is
         when Iir_Kinds_Dyadic_Operator =>
            declare
               Left : Iir;
               Right : Iir;

               function Build_Op (Kind : Nkind) return Node
               is
                  N : Node;
               begin
                  N := Create_Node (Kind);
                  Set_Location (N, Get_Location (Expr));
                  Set_Left (N, Convert_Bool (Left));
                  Set_Right (N, Convert_Bool (Right));
                  Free_Iir (Expr);
                  return N;
               end Build_Op;
            begin
               Impl := Get_Implementation (Expr);
               Left := Get_Left (Expr);
               Right := Get_Right (Expr);
               if Impl /= Null_Iir
                 and then Is_Psl_Bool_Expr (Left)
                 and then Is_Psl_Bool_Expr (Right)
               then
                  if Get_Identifier (Impl) = Name_And then
                     return Build_Op (N_And_Bool);
                  elsif Get_Identifier (Impl) = Name_Or then
                     return Build_Op (N_Or_Bool);
                  end if;
               end if;
            end;
         when Iir_Kinds_Monadic_Operator =>
            declare
               Operand : Iir;

               function Build_Op (Kind : Nkind) return Node
               is
                  N : Node;
               begin
                  N := Create_Node (Kind);
                  Set_Location (N, Get_Location (Expr));
                  Set_Boolean (N, Convert_Bool (Operand));
                  Free_Iir (Expr);
                  return N;
               end Build_Op;
            begin
               Impl := Get_Implementation (Expr);
               Operand := Get_Operand (Expr);
               if Impl /= Null_Iir
                 and then Is_Psl_Bool_Expr (Operand)
               then
                  if Get_Identifier (Impl) = Name_Not then
                     return Build_Op (N_Not_Bool);
                  end if;
               end if;
            end;
         when Iir_Kinds_Name =>
            --  Get the named entity for names in order to hash it.
            declare
               Name : Iir;
            begin
               Name := Get_Named_Entity (Expr);
               if Name /= Null_Iir then
                  return PSL.Hash.Get_PSL_Node (HDL_Node (Name));
               end if;
            end;
         when others =>
            null;
      end case;
      return PSL.Hash.Get_PSL_Node (HDL_Node (Expr));
   end Convert_Bool;

   --  Analyze an HDL expression.  This may mostly a wrapper except in the
   --  case when the expression is in fact a PSL expression.
   function Sem_Hdl_Expr (N : Node) return Node
   is
      use Sem_Names;

      Expr : Iir;
      Name : Iir;
      Decl : Node;
      Res : Node;
   begin
      Expr := Get_HDL_Node (N);
      if Get_Kind (Expr) in Iir_Kinds_Name then
         Sem_Name (Expr);
         Expr := Finish_Sem_Name (Expr);
         Set_HDL_Node (N, Expr);

         Name := Strip_Denoting_Name (Expr);

         case Get_Kind (Name) is
            when Iir_Kind_Error =>
               return N;
            when Iir_Kind_Overload_List =>
               --  FIXME: todo.
               raise Internal_Error;
            when Iir_Kind_Psl_Declaration =>
               Decl := Get_Psl_Declaration (Name);
               case Get_Kind (Decl) is
                  when N_Sequence_Declaration =>
                     Res := Create_Node (N_Sequence_Instance);
                  when N_Property_Declaration =>
                     Res := Create_Node (N_Property_Instance);
                  when N_Boolean_Parameter
                    | N_Sequence_Parameter
                    | N_Const_Parameter
                    | N_Property_Parameter =>
                     --  FIXME: create a n_name
                     Free_Node (N);
                     Free_Iir (Expr);
                     return Decl;
                  when others =>
                     Error_Kind ("sem_hdl_expr(2)", Decl);
               end case;
               Set_Location (Res, Get_Location (N));
               Set_Declaration (Res, Decl);
               if Get_Parameter_List (Decl) /= Null_Node then
                  Error_Msg_Sem (+Res, "no actual for instantiation");
               end if;
               Free_Node (N);
               Free_Iir (Expr);
               return Res;
            when Iir_Kind_Psl_Expression =>
               --  Remove the two bridge nodes: from PSL to HDL and from
               --  HDL to PSL.
               Free_Node (N);
               Res := Get_Psl_Expression (Name);
               Free_Iir (Expr);
               if Name /= Expr then
                  Free_Iir (Name);
               end if;
               return Res;
            when Iir_Kind_Function_Call =>
               Expr := Name;
            when others =>
               Expr := Name_To_Expression (Expr, Null_Iir);
         end case;
      else
         Expr := Sem_Expr.Sem_Expression (Expr, Null_Iir);
      end if;

      if Expr = Null_Iir then
         return N;
      end if;
      Free_Node (N);
      if not Is_Psl_Bool_Expr (Expr) then
         Error_Msg_Sem (+Expr, "type of expression must be boolean");
         return PSL.Hash.Get_PSL_Node (HDL_Node (Expr));
      else
         return Convert_Bool (Expr);
      end if;
   end Sem_Hdl_Expr;

   --  Sem a boolean node.
   function Sem_Boolean (Bool : Node) return Node is
   begin
      case Get_Kind (Bool) is
         when N_HDL_Expr =>
            return Sem_Hdl_Expr (Bool);
         when N_And_Bool
           | N_Or_Bool =>
            Set_Left (Bool, Sem_Boolean (Get_Left (Bool)));
            Set_Right (Bool, Sem_Boolean (Get_Right (Bool)));
            return Bool;
         when others =>
            Error_Kind ("psl.sem_boolean", Bool);
      end case;
   end Sem_Boolean;

   procedure Sem_Boolean (N : Node)
   is
      Bool : Node;
   begin
      Bool := Get_Boolean (N);
      Bool := Sem_Boolean (Bool);
      Set_Boolean (N, Bool);
   end Sem_Boolean;

   --  Used by Sem_Property to rewrite a property logical operator to a
   --  boolean logical operator.
   function Reduce_Logic_Node (Prop : Node; Bool_Kind : Nkind) return Node
   is
      Res : Node;
   begin
      Res := Create_Node (Bool_Kind);
      Set_Location (Res, Get_Location (Prop));
      Set_Left (Res, Get_Left (Prop));
      Set_Right (Res, Get_Right (Prop));
      Free_Node (Prop);
      return Res;
   end Reduce_Logic_Node;

   function Sem_Sequence (Seq : Node) return Node
   is
      Res : Node;
      L, R : Node;
   begin
      case Get_Kind (Seq) is
         when N_Braced_SERE =>
            Res := Sem_Sequence (Get_SERE (Seq));
            Set_SERE (Seq, Res);
            return Seq;
         when N_Clocked_SERE =>
            Res := Sem_Sequence (Get_SERE (Seq));
            Set_SERE (Seq, Res);
            Sem_Boolean (Seq);
            return Seq;
         when N_Concat_SERE
           | N_Fusion_SERE
           | N_Within_SERE
           | N_Or_Seq
           | N_And_Seq
           | N_Match_And_Seq =>
            L := Sem_Sequence (Get_Left (Seq));
            Set_Left (Seq, L);
            R := Sem_Sequence (Get_Right (Seq));
            Set_Right (Seq, R);
            return Seq;
         when N_Star_Repeat_Seq =>
            Res := Get_Sequence (Seq);
            if Res /= Null_Node then
               Res := Sem_Sequence (Get_Sequence (Seq));
               Set_Sequence (Seq, Res);
            end if;
            --  FIXME: range.
            return Seq;
         when N_Plus_Repeat_Seq =>
            Res := Get_Sequence (Seq);
            if Res /= Null_Node then
               Res := Sem_Sequence (Get_Sequence (Seq));
               Set_Sequence (Seq, Res);
            end if;
            return Seq;
         when N_And_Bool
           | N_Or_Bool
           | N_Not_Bool =>
            return Sem_Boolean (Seq);
         when N_HDL_Expr =>
            Res := Sem_Hdl_Expr (Seq);
            case Get_Kind (Res) is
               when N_Sequence_Instance
                 | N_Endpoint_Instance
                 | N_Boolean_Parameter
                 | N_Booleans =>
                  null;
               when N_Property_Instance =>
                  Error_Msg_Sem
                    (+Res, "property instance not allowed in PSL sequence");
               when others =>
                  Error_Kind ("psl.sem_sequence.hdl", Res);
            end case;
            return Res;
         when others =>
            Error_Kind ("psl.sem_sequence", Seq);
      end case;
   end Sem_Sequence;

   function Sem_Property (Prop : Node; Top : Boolean := False) return Node;

   procedure Sem_Property (N : Node; Top : Boolean := False)
   is
      Prop : Node;
   begin
      Prop := Get_Property (N);
      Prop := Sem_Property (Prop, Top);
      Set_Property (N, Prop);
   end Sem_Property;

   procedure Sem_Number (N : Node)
   is
      Num : Node;
   begin
      Num := Get_Number (N);
      --  FIXME: todo
      null;
      Set_Number (N, Num);
   end Sem_Number;

   function Sem_Property (Prop : Node; Top : Boolean := False) return Node
   is
      Res : Node;
      L, R : Node;
   begin
      case Get_Kind (Prop) is
         when N_Braced_SERE =>
            return Sem_Sequence (Prop);
         when N_Always
           | N_Never =>
            --  By extension, clock_event is allowed within outermost
            --  always/never.
            Sem_Property (Prop, Top);
            return Prop;
         when N_Eventually =>
            Sem_Property (Prop);
            return Prop;
         when N_Clock_Event =>
            Sem_Property (Prop);
            Sem_Boolean (Prop);
            if not Top then
               Error_Msg_Sem (+Prop, "inner clock event not supported");
            end if;
            return Prop;
         when N_Abort =>
            Sem_Property (Prop);
            Sem_Boolean (Prop);
            return Prop;
         when N_Until
           | N_Before =>
            Res := Sem_Property (Get_Left (Prop));
            Set_Left (Prop, Res);
            Res := Sem_Property (Get_Right (Prop));
            Set_Right (Prop, Res);
            return Prop;
         when N_Log_Imp_Prop
           | N_And_Prop
           | N_Or_Prop =>
            L := Sem_Property (Get_Left (Prop));
            Set_Left (Prop, L);
            R := Sem_Property (Get_Right (Prop));
            Set_Right (Prop, R);
            if Get_Psl_Type (L) = Type_Boolean
              and then Get_Psl_Type (R) = Type_Boolean
            then
               case Get_Kind (Prop) is
                  when N_And_Prop =>
                     return Reduce_Logic_Node (Prop, N_And_Bool);
                  when N_Or_Prop =>
                     return Reduce_Logic_Node (Prop, N_Or_Bool);
                  when N_Log_Imp_Prop =>
                     return Reduce_Logic_Node (Prop, N_Imp_Bool);
                  when others =>
                     Error_Kind ("psl.sem_property(log)", Prop);
               end case;
            end if;
            return Prop;
         when N_Overlap_Imp_Seq
           | N_Imp_Seq =>
            Res := Sem_Sequence (Get_Sequence (Prop));
            Set_Sequence (Prop, Res);
            Sem_Property (Prop);
            return Prop;
         when N_Next =>
            Sem_Number (Prop);
            Sem_Property (Prop);
            return Prop;
         when N_Next_A =>
            --  FIXME: range.
            Sem_Property (Prop);
            return Prop;
         when N_Next_Event =>
            Sem_Number (Prop);
            Sem_Boolean (Prop);
            Sem_Property (Prop);
            return Prop;
         when N_HDL_Expr =>
            Res := Sem_Hdl_Expr (Prop);
            if not Top and then Get_Kind (Res) = N_Property_Instance then
               declare
                  Decl : constant Node := Get_Declaration (Res);
               begin
                  if Decl /= Null_Node
                    and then Get_Global_Clock (Decl) /= Null_Node
                  then
                     Error_Msg_Sem
                       (+Prop, "property instance already has a clock");
                  end if;
               end;
            end if;
            return Res;
         when others =>
            Error_Kind ("psl.sem_property", Prop);
      end case;
   end Sem_Property;

   --  Extract the clock from PROP.
   procedure Extract_Clock (Prop : in out Node; Clk : out Node)
   is
      Child : Node;
   begin
      Clk := Null_Node;
      case Get_Kind (Prop) is
         when N_Clock_Event =>
            Clk := Get_Boolean (Prop);
            Prop := Get_Property (Prop);
         when N_Clocked_SERE =>
            Clk := Get_Boolean (Prop);
            Prop := Get_SERE (Prop);
         when N_Always
           | N_Never =>
            Child := Get_Property (Prop);
            if Get_Kind (Child) = N_Clock_Event then
               Set_Property (Prop, Get_Property (Child));
               Clk := Get_Boolean (Child);
            end if;
         when N_Property_Instance =>
            Child := Get_Declaration (Prop);
            Clk := Get_Global_Clock (Child);
         when others =>
            null;
      end case;
   end Extract_Clock;

   --  Sem a property/sequence/endpoint declaration.
   procedure Sem_Psl_Declaration (Stmt : Iir)
   is
      use Sem_Scopes;
      Decl : constant Node := Get_Psl_Declaration (Stmt);
      Prop : Node;
      Clk : Node;
      Formal : Node;
      El : Iir;
   begin
      Sem_Scopes.Add_Name (Stmt);
      Xref_Decl (Stmt);

      Open_Declarative_Region;

      --  Make formal parameters visible.
      Formal := Get_Parameter_List (Decl);
      while Formal /= Null_Node loop
         El := Create_Iir (Iir_Kind_Psl_Declaration);
         Set_Location (El, Get_Location (Formal));
         Set_Identifier (El, Get_Identifier (Formal));
         Set_Psl_Declaration (El, Formal);

         Sem_Scopes.Add_Name (El);
         Xref_Decl (El);
         Set_Visible_Flag (El, True);

         Formal := Get_Chain (Formal);
      end loop;

      case Get_Kind (Decl) is
         when N_Property_Declaration =>
            --  FIXME: sem formal list
            Prop := Get_Property (Decl);
            Prop := Sem_Property (Prop, True);
            Extract_Clock (Prop, Clk);
            Set_Property (Decl, Prop);
            Set_Global_Clock (Decl, Clk);
            --  Check simple subset restrictions.
            PSL.Subsets.Check_Simple (Prop);
         when N_Sequence_Declaration
           | N_Endpoint_Declaration =>
            --  FIXME: sem formal list, do not allow property parameter.
            Prop := Get_Sequence (Decl);
            Prop := Sem_Sequence (Prop);
            Set_Sequence (Decl, Prop);
            PSL.Subsets.Check_Simple (Prop);
         when others =>
            Error_Kind ("sem_psl_declaration", Decl);
      end case;
      Set_Visible_Flag (Stmt, True);

      Close_Declarative_Region;
   end Sem_Psl_Declaration;

   procedure Sem_Psl_Endpoint_Declaration (Stmt : Iir)
   is
      Decl : constant Node := Get_Psl_Declaration (Stmt);
      Prop : Node;
   begin
      Sem_Scopes.Add_Name (Stmt);
      Xref_Decl (Stmt);

      pragma Assert (Get_Parameter_List (Decl) = Null_Node);
      pragma Assert (Get_Kind (Decl) = N_Endpoint_Declaration);

      Prop := Get_Sequence (Decl);
      Prop := Sem_Sequence (Prop);
      Sem_Psl_Directive_Clock (Stmt, Prop);
      Set_Sequence (Decl, Prop);

      PSL.Subsets.Check_Simple (Prop);

      --  Endpoints are considered as an HDL declaration and must have a
      --  type.
      Set_Type (Stmt, Std_Package.Boolean_Type_Definition);
      Set_Expr_Staticness (Stmt, None);

      Set_Visible_Flag (Stmt, True);
   end Sem_Psl_Endpoint_Declaration;

   function Rewrite_As_Boolean_Expression (Prop : Node) return Iir
   is
      function Rewrite_Dyadic_Operator
        (Expr : Node; Kind : Iir_Kind) return Iir
      is
         Res : Iir;
      begin
         Res := Create_Iir (Kind);
         Set_Location (Res, Get_Location (Expr));
         Set_Left (Res, Rewrite_As_Boolean_Expression (Get_Left (Expr)));
         Set_Right (Res, Rewrite_As_Boolean_Expression (Get_Right (Expr)));
         return Res;
      end Rewrite_Dyadic_Operator;

      function Rewrite_Monadic_Operator
        (Expr : Node; Kind : Iir_Kind) return Iir
      is
         Res : Iir;
      begin
         Res := Create_Iir (Kind);
         Set_Location (Res, Get_Location (Expr));
         Set_Operand (Res, Rewrite_As_Boolean_Expression (Get_Boolean (Expr)));
         return Res;
      end Rewrite_Monadic_Operator;
   begin
      case Get_Kind (Prop) is
         when N_HDL_Expr =>
            return Get_HDL_Node (Prop);
         when N_And_Bool =>
            return Rewrite_Dyadic_Operator (Prop, Iir_Kind_And_Operator);
         when N_Or_Bool =>
            return Rewrite_Dyadic_Operator (Prop, Iir_Kind_Or_Operator);
         when N_Not_Bool =>
            return Rewrite_Monadic_Operator (Prop, Iir_Kind_Not_Operator);
         when others =>
            Error_Kind ("rewrite_as_boolean_expression", Prop);
      end case;
   end Rewrite_As_Boolean_Expression;

   function Rewrite_As_Concurrent_Assertion (Stmt : Iir) return Iir
   is
      Res : Iir;
      Cond : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Concurrent_Assertion_Statement);
      Set_Location (Res, Get_Location (Stmt));
      Cond := Rewrite_As_Boolean_Expression (Get_Psl_Property (Stmt));
      if Get_Type (Cond) = Null_Iir then
         Cond := Sem_Expr.Sem_Condition (Cond);
      elsif Get_Base_Type (Get_Type (Cond))
        /= Std_Package.Boolean_Type_Definition
      then
         Cond := Sem_Expr.Insert_Condition_Operator (Cond);
      end if;
      Cond := Eval_Expr_If_Static (Cond);
      Set_Assertion_Condition (Res, Cond);
      Set_Label (Res, Get_Label (Stmt));
      Set_Severity_Expression (Res, Get_Severity_Expression (Stmt));
      Set_Report_Expression (Res, Get_Report_Expression (Stmt));
      Set_Postponed_Flag (Res, Get_Postponed_Flag (Stmt));
      return Res;
   end Rewrite_As_Concurrent_Assertion;

   --  Return True iff EXPR is a boolean expression.
   function Is_Boolean_Assertion (Expr : Node) return Boolean is
   begin
      case Get_Kind (Expr) is
         when N_HDL_Expr =>
            return True;
         when N_And_Bool | N_Or_Bool | N_Not_Bool =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Boolean_Assertion;

   procedure Sem_Psl_Directive_Clock (Stmt : Iir; Prop : in out Node)
   is
      Clk : Node;
   begin
      Extract_Clock (Prop, Clk);
      if Clk = Null_Node then
         if Current_Psl_Default_Clock = Null_Iir then
            Error_Msg_Sem (+Stmt, "no clock for PSL directive");
            Clk := Null_Node;
         else
            Clk := Get_Psl_Boolean (Current_Psl_Default_Clock);
         end if;
      end if;
      Set_PSL_Clock (Stmt, Clk);
   end Sem_Psl_Directive_Clock;

   function Sem_Psl_Assert_Statement (Stmt : Iir) return Iir
   is
      Prop : Node;
      Res : Iir;
   begin
      pragma Assert (Get_Kind (Stmt) = Iir_Kind_Psl_Assert_Statement);

      --  Sem report and severity expressions.
      Sem_Report_Statement (Stmt);

      Prop := Get_Psl_Property (Stmt);
      Prop := Sem_Property (Prop, True);
      Set_Psl_Property (Stmt, Prop);

      if Is_Boolean_Assertion (Prop) then
         --  This is a simple assertion.  Convert to a non-PSL statement, as
         --  the handling is simpler (and the assertion doesn't need a clock).
         Res := Rewrite_As_Concurrent_Assertion (Stmt);
         Free_Iir (Stmt);
         return Res;
      end if;

      if Get_Postponed_Flag (Stmt) then
         Error_Msg_Sem (+Stmt, "PSL assertions cannot be postponed");
         Set_Postponed_Flag (Stmt, False);
      end if;

      --  Properties must be clocked.
      Sem_Psl_Directive_Clock (Stmt, Prop);
      Set_Psl_Property (Stmt, Prop);

      --  Check simple subset restrictions.
      PSL.Subsets.Check_Simple (Prop);

      return Stmt;
   end Sem_Psl_Assert_Statement;

   procedure Sem_Psl_Cover_Statement (Stmt : Iir)
   is
      Seq : Node;
   begin
      --  Sem report and severity expressions.
      Sem_Report_Statement (Stmt);

      Seq := Get_Psl_Sequence (Stmt);
      Seq := Sem_Sequence (Seq);

      --  Properties must be clocked.
      Sem_Psl_Directive_Clock (Stmt, Seq);
      Set_Psl_Sequence (Stmt, Seq);

      --  Check simple subset restrictions.
      PSL.Subsets.Check_Simple (Seq);
   end Sem_Psl_Cover_Statement;

   procedure Sem_Psl_Default_Clock (Stmt : Iir)
   is
      Expr : Node;
   begin
      if Current_Psl_Default_Clock /= Null_Iir
        and then Get_Parent (Current_Psl_Default_Clock) = Get_Parent (Stmt)
      then
         Error_Msg_Sem
           (+Stmt, "redeclaration of PSL default clock in the same region",
            Cont => True);
         Error_Msg_Sem
           (+Current_Psl_Default_Clock,
            " (previous default clock declaration)");
      end if;
      Expr := Sem_Boolean (Get_Psl_Boolean (Stmt));
      Set_Psl_Boolean (Stmt, Expr);
      Current_Psl_Default_Clock := Stmt;
   end Sem_Psl_Default_Clock;

   function Sem_Psl_Instance_Name (Name : Iir) return Iir
   is
      Prefix : constant Iir := Get_Prefix (Name);
      Ent : constant Iir := Get_Named_Entity (Prefix);
      Decl : constant Node := Get_Psl_Declaration (Ent);
      Formal : Node;
      Assoc : Iir;
      Res : Node;
      Last_Assoc : Node;
      Assoc2 : Node;
      Actual : Iir;
      Psl_Actual : Node;
      Res2 : Iir;
   begin
      pragma Assert (Get_Kind (Ent) = Iir_Kind_Psl_Declaration
                       or Get_Kind (Ent) = Iir_Kind_Psl_Endpoint_Declaration);
      case Get_Kind (Decl) is
         when N_Property_Declaration =>
            Res := Create_Node (N_Property_Instance);
         when N_Sequence_Declaration =>
            Res := Create_Node (N_Sequence_Instance);
         when N_Endpoint_Declaration =>
            Res := Create_Node (N_Endpoint_Instance);
         when others =>
            Error_Msg_Sem (+Name, "can only instantiate a psl declaration");
            return Null_Iir;
      end case;
      Set_Declaration (Res, Decl);
      Set_Location (Res, Get_Location (Name));
      Formal := Get_Parameter_List (Decl);
      Assoc := Get_Association_Chain (Name);
      Last_Assoc := Null_Node;

      while Formal /= Null_Node loop
         if Assoc = Null_Iir then
            Error_Msg_Sem (+Name, "not enough association");
            exit;
         end if;
         if Get_Kind (Assoc) /= Iir_Kind_Association_Element_By_Expression then
            Error_Msg_Sem
              (+Assoc, "open or individual association not allowed");
         elsif Get_Formal (Assoc) /= Null_Iir then
            Error_Msg_Sem (+Assoc, "named association not allowed in psl");
         else
            Actual := Get_Actual (Assoc);
            --  FIXME: currently only boolean are parsed.
            Actual := Sem_Expr.Sem_Expression (Actual, Null_Iir);
            if Get_Kind (Actual) in Iir_Kinds_Name then
               Actual := Get_Named_Entity (Actual);
            end if;
            Psl_Actual := PSL.Hash.Get_PSL_Node (HDL_Node (Actual));
         end if;

         Assoc2 := Create_Node (N_Actual);
         Set_Location (Assoc2, Get_Location (Assoc));
         Set_Formal (Assoc2, Formal);
         Set_Actual (Assoc2, Psl_Actual);
         if Last_Assoc = Null_Node then
            Set_Association_Chain (Res, Assoc2);
         else
            Set_Chain (Last_Assoc, Assoc2);
         end if;
         Last_Assoc := Assoc2;

         Formal := Get_Chain (Formal);
         Assoc := Get_Chain (Assoc);
      end loop;
      if Assoc /= Null_Iir then
         Error_Msg_Sem (+Name, "too many association");
      end if;

      Res2 := Create_Iir (Iir_Kind_Psl_Expression);
      Set_Psl_Expression (Res2, Res);
      Location_Copy (Res2, Name);
      return Res2;
   end Sem_Psl_Instance_Name;

   --  Called by sem_names to analyze a psl name.
   function Sem_Psl_Name (Name : Iir) return Iir is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Parenthesis_Name =>
            return Sem_Psl_Instance_Name (Name);
         when others =>
            Error_Kind ("sem_psl_name", Name);
      end case;
      return Null_Iir;
   end Sem_Psl_Name;

end Sem_Psl;
