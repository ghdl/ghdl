--  Semantic analysis pass for PSL.
--  Copyright (C) 2009 Tristan Gingold
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
with Errorout; use Errorout;
with Libraries;

with PSL.Types; use PSL.Types;
with PSL.Nodes; use PSL.Nodes;
with PSL.Subsets;
with PSL.Hash;
with PSL.Rewrites;
with PSL.Errors; use PSL.Errors;

with Vhdl.Sem_Expr;
with Vhdl.Sem_Stmts; use Vhdl.Sem_Stmts;
with Vhdl.Sem_Scopes;
with Vhdl.Sem_Names;
with Vhdl.Sem_Lib;
with Vhdl.Sem_Decls;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Evaluation; use Vhdl.Evaluation;
with Vhdl.Std_Package;
with Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Xrefs; use Vhdl.Xrefs;

package body Vhdl.Sem_Psl is
   procedure Sem_Psl_Directive_Clock (Stmt : Iir; Prop : in out PSL_Node);

   --  Return TRUE iff Atype is a PSL boolean type.
   --  See PSL1.1 5.1.2  Boolean expressions
   function Is_Psl_Boolean_Type (Atype : Iir) return Boolean
   is
      Btype : Iir;
   begin
      if Atype = Null_Iir then
         return False;
      end if;
      Btype := Get_Base_Type (Atype);
      return Btype = Vhdl.Std_Package.Boolean_Type_Definition
        or else Btype = Vhdl.Std_Package.Bit_Type_Definition
        or else Btype = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_Type;
   end Is_Psl_Boolean_Type;

   --  Return TRUE if EXPR type is a PSL boolean type.
   function Is_Psl_Boolean_Expr (Expr : Iir) return Boolean
   is
      Etype : constant Iir := Get_Type (Expr);
   begin
      --  In case of overload, consider the expression not as a PSL boolean.
      --  It couldn't be resolved, so there will be an error, unless the whole
      --  property is used in an assertion and the assertion is not considered
      --  as a PSL assertion.
      if Sem_Names.Is_Overload_List (Etype) then
         return False;
      end if;

      return Is_Psl_Boolean_Type (Etype);
   end Is_Psl_Boolean_Expr;

   function Is_Psl_Bit_Type (Atype : Iir) return Boolean
   is
      Btype : constant Iir := Get_Base_Type (Atype);
   begin
      return Btype = Vhdl.Std_Package.Bit_Type_Definition
        or else Btype = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_Type;
   end Is_Psl_Bit_Type;

   function Is_Psl_Bitvector_Type (Atype : Iir) return Boolean is
   begin
      if not Is_One_Dimensional_Array_Type (Atype) then
         return False;
      end if;
      return Is_Psl_Bit_Type (Get_Element_Subtype (Atype));
   end Is_Psl_Bitvector_Type;

   function Sem_Prev_Builtin (Call : Iir; Atype : Iir) return Iir
   is
      use Vhdl.Sem_Expr;
      use Vhdl.Std_Package;
      Expr  : Iir;
      Count : Iir;
      Clock : Iir;
      First : Boolean;
   begin
      Expr := Get_Expression (Call);
      First := Is_Expr_Not_Analyzed (Expr);
      Expr := Sem_Expression_Ov (Expr, Atype);
      if Expr /= Null_Iir then
         Set_Expression (Call, Expr);
         Set_Type (Call, Get_Type (Expr));
         Set_Expr_Staticness (Call, None);
      end if;

      if First then
         --  Analyze count and clock only once.
         Count := Get_Count_Expression (Call);
         if Count /= Null_Iir then
            Count := Sem_Expression_Wildcard
              (Count, Wildcard_Any_Integer_Type);
            Count := Eval_Expr (Count);
            Set_Count_Expression (Call, Count);
         end if;

         Clock := Get_Clock_Expression (Call);
         if Clock /= Null_Iir then
            Clock := Sem_Expression_Wildcard (Clock, Wildcard_Psl_Bit_Type);
            Set_Clock_Expression (Call, Clock);
         else
            if Current_Psl_Default_Clock = Null_Iir then
               Error_Msg_Sem (+Call, "no clock for PSL prev builtin");
            else
               Set_Default_Clock (Call, Current_Psl_Default_Clock);
            end if;
         end if;
      end if;

      return Call;
   end Sem_Prev_Builtin;

   function Sem_Clock_Builtin (Call : Iir) return Iir
   is
      use Vhdl.Sem_Expr;
      use Vhdl.Std_Package;
      Expr  : Iir;
      Clock : Iir;
      First : Boolean;
   begin
      Expr := Get_Expression (Call);
      First := Is_Expr_Not_Analyzed (Expr);
      Expr := Sem_Expression (Expr, Null_Iir);
      if Expr /= Null_Iir then
         Set_Expression (Call, Expr);
         Set_Type (Call, Vhdl.Std_Package.Boolean_Type_Definition);
         Set_Expr_Staticness (Call, None);
      end if;

      if First then
         --  Analyze clock only once.
         Clock := Get_Clock_Expression (Call);
         if Clock /= Null_Iir then
            Clock := Sem_Expression_Wildcard (Clock, Wildcard_Psl_Bit_Type);
            Set_Clock_Expression (Call, Clock);
         else
            if Current_Psl_Default_Clock = Null_Iir then
               Error_Msg_Sem (+Call, "no clock for %n", +Call);
            else
               Set_Default_Clock (Call, Current_Psl_Default_Clock);
            end if;
         end if;
      end if;

      return Call;
   end Sem_Clock_Builtin;

   function Sem_Onehot_Builtin (Call : Iir) return Iir
   is
      use Vhdl.Sem_Expr;
      use Vhdl.Std_Package;
      Expr : Iir;
   begin
      Expr := Get_Expression (Call);
      Expr := Sem_Expression (Expr, Null_Iir);
      if Expr /= Null_Iir then
         Set_Expression (Call, Expr);
         Set_Type (Call, Vhdl.Std_Package.Boolean_Type_Definition);
         Set_Expr_Staticness (Call, None);
         if not Is_Psl_Bitvector_Type (Get_Type (Expr)) then
            Error_Msg_Sem (+Call, "type of parameter must be bitvector");
         end if;
      end if;
      return Call;
   end Sem_Onehot_Builtin;

   --  Convert VHDL and/or/not nodes to PSL nodes.
   function Convert_Bool (Expr : Iir) return PSL_Node;

   function Convert_Bool_Dyadic_Operator (Expr : Iir; Kind : PSL.Nodes.Nkind)
                                         return PSL_Node
   is
      Left  : constant Iir := Get_Left (Expr);
      Right : constant Iir := Get_Right (Expr);
      N : PSL_Node;
   begin
      if Is_Psl_Boolean_Expr (Left)
        and then Is_Psl_Boolean_Expr (Right)
      then
         N := Create_Node (Kind);
         Set_Location (N, Get_Location (Expr));
         Set_Left (N, Convert_Bool (Left));
         Set_Right (N, Convert_Bool (Right));
         Free_Iir (Expr);
         return N;
      else
         return Null_PSL_Node;
      end if;
   end Convert_Bool_Dyadic_Operator;

   function Convert_Bool_Monadic_Operator (Expr : Iir; Kind : PSL.Nodes.Nkind)
                                          return PSL_Node
   is
      Operand : constant Iir := Get_Operand (Expr);
      N : PSL_Node;
   begin
      if Is_Psl_Boolean_Expr (Operand) then
         N := Create_Node (Kind);
         Set_Location (N, Get_Location (Expr));
         Set_Boolean (N, Convert_Bool (Operand));
         Free_Iir (Expr);
         return N;
      else
         return Null_PSL_Node;
      end if;
   end Convert_Bool_Monadic_Operator;

   --  Convert VHDL and/or/not nodes to PSL nodes.
   function Convert_Bool (Expr : Iir) return PSL_Node
   is
      Res : PSL_Node;
   begin
      case Get_Kind (Expr) is
         when Iir_Kind_And_Operator =>
            Res := Convert_Bool_Dyadic_Operator (Expr, N_And_Bool);
            if Res /= Null_PSL_Node then
               return Res;
            end if;
         when Iir_Kind_Or_Operator =>
            Res := Convert_Bool_Dyadic_Operator (Expr, N_Or_Bool);
            if Res /= Null_PSL_Node then
               return Res;
            end if;
         when Iir_Kind_Not_Operator =>
            Res := Convert_Bool_Monadic_Operator (Expr, N_Not_Bool);
            if Res /= Null_PSL_Node then
               return Res;
            end if;
         when Iir_Kinds_Name =>
            --  Get the named entity for names in order to hash it.
            declare
               Name : Iir;
               Hnode : PSL_Node;
               N : PSL_Node;
            begin
               Name := Get_Named_Entity (Expr);
               if Name /= Null_Iir then
                  Hnode := PSL.Hash.Get_PSL_Node
                    (HDL_Node (Name), Get_Location (Name));
                  N := Create_Node (N_HDL_Expr);
                  Set_Location (N, Get_Location (Expr));
                  Set_HDL_Node (N, HDL_Node (Expr));
                  Set_HDL_Hash (N, Hnode);
                  return N;
               end if;
            end;
         when others =>
            null;
      end case;

      --  Default.
      return PSL.Hash.Get_PSL_Node (HDL_Node (Expr), Get_Location (Expr));
   end Convert_Bool;

   --  Analyze an HDL expression.  This may mostly a wrapper except in the
   --  case when the expression is in fact a PSL expression.
   function Sem_Hdl_Expr (N : PSL_Node) return PSL_Node
   is
      use Sem_Names;

      Expr : Iir;
      Expr_Type : Iir;
      Name : Iir;
      Decl : PSL_Node;
      Res : PSL_Node;
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
            when Iir_Kind_Psl_Declaration
              | Iir_Kind_Psl_Boolean_Parameter =>
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
               if Get_Parameter_List (Decl) /= Null_PSL_Node then
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
            when Iir_Kind_Function_Call
              | Iir_Kind_Indexed_Name
              | Iir_Kind_Selected_Element
              | Iir_Kinds_Expression_Attribute =>
               Expr := Name;
            when others =>
               Expr := Name_To_Expression (Expr, Null_Iir);
         end case;
      else
         Expr := Sem_Expr.Sem_Expression_Wildcard
           (Expr, Std_Package.Wildcard_Psl_Boolean_Type);
      end if;

      if Expr = Null_Iir then
         return N;
      end if;
      Expr_Type := Get_Type (Expr);
      if Expr_Type = Null_Iir then
         return N;
      end if;
      Free_Node (N);
      if not Is_Overload_List (Expr_Type)
        and then not Is_Psl_Boolean_Type (Expr_Type)
      then
         Error_Msg_Sem (+Expr, "type of expression must be boolean");
         return PSL.Hash.Get_PSL_Node (HDL_Node (Expr), Get_Location (Expr));
      else
         return Convert_Bool (Expr);
      end if;
   end Sem_Hdl_Expr;

   --  Sem a boolean node.
   function Sem_Boolean (Bool : PSL_Node) return PSL_Node is
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

   procedure Sem_Boolean (N : PSL_Node)
   is
      Bool : PSL_Node;
   begin
      Bool := Get_Boolean (N);
      Bool := Sem_Boolean (Bool);
      Set_Boolean (N, Bool);
   end Sem_Boolean;

   --  Used by Sem_Property to rewrite a property logical operator to a
   --  boolean logical operator.
   function Reduce_Logic_Binary_Node (Prop : PSL_Node; Bool_Kind : Nkind)
                                     return PSL_Node
   is
      Res : PSL_Node;
   begin
      Res := Create_Node (Bool_Kind);
      Set_Location (Res, Get_Location (Prop));
      Set_Left (Res, Get_Left (Prop));
      Set_Right (Res, Get_Right (Prop));
      Free_Node (Prop);
      return Res;
   end Reduce_Logic_Binary_Node;

   function Reduce_Logic_Unary_Node (Prop : PSL_Node; Bool_Kind : Nkind)
                                    return PSL_Node
   is
      Res : PSL_Node;
   begin
      Res := Create_Node (Bool_Kind);
      Set_Location (Res, Get_Location (Prop));
      Set_Boolean (Res, Get_Property (Prop));
      Free_Node (Prop);
      return Res;
   end Reduce_Logic_Unary_Node;

   function Sem_Sequence (Seq : PSL_Node) return PSL_Node
   is
      Res : PSL_Node;
      L, R : PSL_Node;
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
         when N_Star_Repeat_Seq
            | N_Plus_Repeat_Seq =>
            Res := Get_Sequence (Seq);
            if Res /= Null_PSL_Node then
               Res := Sem_Sequence (Res);
               Set_Sequence (Seq, Res);
            end if;
            return Seq;
         when N_Equal_Repeat_Seq
            | N_Goto_Repeat_Seq =>
            Res := Get_Boolean (Seq);
            if Res /= Null_PSL_Node then
               Res := Sem_Boolean (Res);
               Set_Boolean (Seq, Res);
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

   function Sem_Property (Prop : PSL_Node; Top : Boolean := False)
                         return PSL_Node;

   procedure Sem_Property (N : PSL_Node; Top : Boolean := False)
   is
      Prop : PSL_Node;
   begin
      Prop := Get_Property (N);
      Prop := Sem_Property (Prop, Top);
      Set_Property (N, Prop);
   end Sem_Property;

   procedure Sem_Number (N : PSL_Node)
   is
      Num : PSL_Node;
   begin
      Num := Get_Number (N);
      --  FIXME: todo
      null;
      Set_Number (N, Num);
   end Sem_Number;

   function Sem_Property (Prop : PSL_Node; Top : Boolean := False)
                         return PSL_Node
   is
      Res : PSL_Node;
   begin
      case Get_Kind (Prop) is
         when N_Braced_SERE
            | N_Clocked_SERE =>
            return Sem_Sequence (Prop);
         when N_Star_Repeat_Seq
            | N_Plus_Repeat_Seq =>
            declare
               Seq : PSL_Node;
            begin
               Seq := Get_Sequence (Prop);
               if Seq /= Null_PSL_Node then
                  Seq := Sem_Sequence (Seq);
                  Set_Sequence (Prop, Seq);
               end if;
               return Prop;
            end;
         when N_Equal_Repeat_Seq
            | N_Goto_Repeat_Seq =>
            declare
               B : PSL_Node;
            begin
               B := Get_Boolean (Prop);
               B := Sem_Boolean (B);
               Set_Boolean (Prop, B);
               return Prop;
            end;
         when N_Always
            | N_Never =>
            --  By extension, clock_event is allowed within outermost
            --  always/never.
            Sem_Property (Prop, Top);
            return Prop;
         when N_Eventually
            | N_Strong =>
            Sem_Property (Prop);
            return Prop;
         when N_Clock_Event =>
            Sem_Property (Prop);
            Sem_Boolean (Prop);
            if not Top then
               Error_Msg_Sem (+Prop, "inner clock event not supported");
            end if;
            return Prop;
         when N_Abort
            | N_Async_Abort
            | N_Sync_Abort =>
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
            | N_Log_Equiv_Prop
            | N_And_Prop
            | N_Or_Prop =>
            declare
               L, R : PSL_Node;
            begin
               L := Sem_Property (Get_Left (Prop));
               Set_Left (Prop, L);
               R := Sem_Property (Get_Right (Prop));
               Set_Right (Prop, R);
               if Get_Psl_Type (L) = Type_Boolean
                 and then Get_Psl_Type (R) = Type_Boolean
               then
                  case Get_Kind (Prop) is
                     when N_And_Prop =>
                        return Reduce_Logic_Binary_Node (Prop, N_And_Bool);
                     when N_Or_Prop =>
                        return Reduce_Logic_Binary_Node (Prop, N_Or_Bool);
                     when N_Log_Imp_Prop =>
                        return Reduce_Logic_Binary_Node (Prop, N_Imp_Bool);
                     when N_Log_Equiv_Prop =>
                        return Reduce_Logic_Binary_Node (Prop, N_Equiv_Bool);
                     when others =>
                        Error_Kind ("psl.sem_property(log)", Prop);
                  end case;
               else
                  return Prop;
               end if;
            end;
         when N_Overlap_Imp_Seq
           | N_Imp_Seq =>
            Res := Sem_Sequence (Get_Sequence (Prop));
            Set_Sequence (Prop, Res);
            Sem_Property (Prop);
            return Prop;
         when N_Paren_Prop =>
            declare
               Op : PSL_Node;
            begin
               Op := Get_Property (Prop);
               Op := Sem_Property (Op);
               Set_Property (Prop, Op);
               if Get_Psl_Type (Op) = Type_Boolean then
                  return Reduce_Logic_Unary_Node (Prop, N_Paren_Bool);
               else
                  return Prop;
               end if;
            end;
         when N_Next =>
            Sem_Number (Prop);
            Sem_Property (Prop);
            return Prop;
         when N_Next_A | N_Next_E =>
            --  FIXME: range.
            Sem_Property (Prop);
            return Prop;
         when N_Next_Event =>
            Sem_Number (Prop);
            Sem_Boolean (Prop);
            Sem_Property (Prop);
            return Prop;
         when N_Next_Event_A | N_Next_Event_E =>
            --  FIXME: range.
            Sem_Boolean (Prop);
            Sem_Property (Prop);
            return Prop;
         when N_HDL_Expr =>
            Res := Sem_Hdl_Expr (Prop);
            if not Top and then Get_Kind (Res) = N_Property_Instance then
               declare
                  Decl : constant PSL_Node := Get_Declaration (Res);
               begin
                  if Decl /= Null_PSL_Node
                    and then Get_Global_Clock (Decl) /= Null_PSL_Node
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
   procedure Extract_Clock (Prop : in out PSL_Node; Clk : out PSL_Node)
   is
      Child : PSL_Node;
   begin
      Clk := Null_PSL_Node;
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
            elsif Get_Kind (Child) = N_Clocked_SERE then
               Clk := Get_Boolean (Child);
               Set_Property (Prop, Get_SERE (Child));
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
      Decl : constant PSL_Node := Get_Psl_Declaration (Stmt);
      Prop : PSL_Node;
      Clk : PSL_Node;
      Formal : PSL_Node;
      El : Iir;
   begin
      Sem_Scopes.Add_Name (Stmt);
      Xref_Decl (Stmt);

      Open_Declarative_Region;

      --  Make formal parameters visible.
      Formal := Get_Parameter_List (Decl);
      while Formal /= Null_PSL_Node loop
         if Get_Kind (Formal) = N_Boolean_Parameter then
            El := Create_Iir (Iir_Kind_Psl_Boolean_Parameter);
            Set_Type (El, Std_Package.Boolean_Type_Definition);
         else
            El := Create_Iir (Iir_Kind_Psl_Declaration);
         end if;
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
      Decl : constant PSL_Node := Get_Psl_Declaration (Stmt);
      Prop : PSL_Node;
   begin
      Sem_Scopes.Add_Name (Stmt);
      Xref_Decl (Stmt);

      pragma Assert (Get_Parameter_List (Decl) = Null_PSL_Node);
      pragma Assert (Get_Kind (Decl) = N_Endpoint_Declaration);

      Prop := Get_Sequence (Decl);
      Prop := Sem_Sequence (Prop);
      Sem_Psl_Directive_Clock (Stmt, Prop);
      Set_Sequence (Decl, Prop);

      PSL.Subsets.Check_Simple (Prop);

      --  Endpoints are considered as an HDL declaration and must have a
      --  type.
      Set_Type (Stmt, Vhdl.Std_Package.Boolean_Type_Definition);
      Set_Expr_Staticness (Stmt, None);

      Set_Visible_Flag (Stmt, True);
   end Sem_Psl_Endpoint_Declaration;

   function Rewrite_As_Boolean_Expression (Prop : PSL_Node) return Iir
   is
      function Rewrite_Dyadic_Operator
        (Expr : PSL_Node; Kind : Iir_Kind) return Iir
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
        (Expr : PSL_Node; Kind : Iir_Kind) return Iir
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
         when N_HDL_Expr
           | N_HDL_Bool =>
            return Get_HDL_Node (Prop);
         when N_And_Bool =>
            return Rewrite_Dyadic_Operator (Prop, Iir_Kind_And_Operator);
         when N_Or_Bool =>
            return Rewrite_Dyadic_Operator (Prop, Iir_Kind_Or_Operator);
         when N_Not_Bool =>
            return Rewrite_Monadic_Operator (Prop, Iir_Kind_Not_Operator);
         when N_Paren_Bool =>
            declare
               Expr : constant PSL_Node := Get_Boolean (Prop);
               Hexpr : Iir;
               Res : Iir;
            begin
               Res := Create_Iir (Iir_Kind_Parenthesis_Expression);
               Set_Location (Res, Get_Location (Prop));
               case Get_Kind (Expr) is
                  when N_HDL_Expr
                    | N_HDL_Bool =>
                     Hexpr := Get_HDL_Node (Expr);
                     Set_Expression (Res, Hexpr);
                     Set_Type (Res, Get_Type (Hexpr));
                  when others =>
                     Hexpr := Rewrite_As_Boolean_Expression (Expr);
                     Set_Expression (Res, Hexpr);
               end case;
               return Res;
            end;
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
      else
         Cond := Sem_Expr.Sem_Condition_Pass2 (Cond);
      end if;
      Cond := Eval_Expr_If_Static (Cond);

      Set_Assertion_Condition (Res, Cond);
      Set_Label (Res, Get_Label (Stmt));
      Set_Severity_Expression (Res, Get_Severity_Expression (Stmt));
      Set_Report_Expression (Res, Get_Report_Expression (Stmt));
      Set_Postponed_Flag (Res, Get_Postponed_Flag (Stmt));

      Set_Parent (Res, Get_Parent (Stmt));
      Set_Chain (Res, Get_Chain (Stmt));
      return Res;
   end Rewrite_As_Concurrent_Assertion;

   --  Return True iff EXPR is a boolean expression.
   function Is_Boolean_Assertion (Expr : PSL_Node) return Boolean is
   begin
      case Get_Kind (Expr) is
         when N_HDL_Expr
           | N_HDL_Bool =>
            return True;
         when N_And_Bool | N_Or_Bool | N_Not_Bool | N_Paren_Bool =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Boolean_Assertion;

   procedure Sem_Psl_Directive_Clock (Stmt : Iir; Prop : in out PSL_Node)
   is
      Clk : PSL_Node;
   begin
      Extract_Clock (Prop, Clk);
      if Clk = Null_PSL_Node then
         if Current_Psl_Default_Clock = Null_Iir then
            Error_Msg_Sem (+Stmt, "no clock for PSL directive");
            Clk := Null_PSL_Node;
         else
            Clk := Get_Psl_Boolean (Current_Psl_Default_Clock);
         end if;
      end if;
      Set_PSL_Clock (Stmt, Clk);
   end Sem_Psl_Directive_Clock;

   function Sem_Psl_Assert_Directive
     (Stmt : Iir; Can_Rewrite : Boolean) return Iir
   is
      Prop : PSL_Node;
      Res : Iir;
   begin
      pragma Assert (Get_Kind (Stmt) = Iir_Kind_Psl_Assert_Directive);

      --  Sem report and severity expressions.
      Sem_Report_Statement (Stmt);

      Prop := Get_Psl_Property (Stmt);
      Prop := Sem_Property (Prop, True);
      Set_Psl_Property (Stmt, Prop);

      if Can_Rewrite and then Is_Boolean_Assertion (Prop) then
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
   end Sem_Psl_Assert_Directive;

   procedure Sem_Psl_Assume_Directive (Stmt : Iir)
   is
      Prop : PSL_Node;
   begin
      Prop := Get_Psl_Property (Stmt);
      Prop := Sem_Property (Prop, True);
      Set_Psl_Property (Stmt, Prop);

      --  Properties must be clocked.
      Sem_Psl_Directive_Clock (Stmt, Prop);
      Set_Psl_Property (Stmt, Prop);

      --  Check simple subset restrictions.
      PSL.Subsets.Check_Simple (Prop);
   end Sem_Psl_Assume_Directive;

   procedure Sem_Psl_Sequence (Stmt : Iir)
   is
      Seq : PSL_Node;
   begin
      Seq := Get_Psl_Sequence (Stmt);
      Seq := Sem_Sequence (Seq);

      --  Expect a pure sequence.
      case Get_Kind (Seq) is
         when N_Sequence_Instance
           |  N_Star_Repeat_Seq
           |  N_Goto_Repeat_Seq
           |  N_Plus_Repeat_Seq
           |  N_Equal_Repeat_Seq
           |  N_Braced_SERE
           |  N_Clocked_SERE =>
            null;
         when others =>
            Error_Msg_Sem (+Seq, "sequence expected here");
      end case;

      --  Properties must be clocked.
      Sem_Psl_Directive_Clock (Stmt, Seq);
      Set_Psl_Sequence (Stmt, Seq);

      --  Check simple subset restrictions.
      PSL.Subsets.Check_Simple (Seq);
   end Sem_Psl_Sequence;

   procedure Sem_Psl_Cover_Directive (Stmt : Iir) is
   begin
      Sem_Report_Expression (Stmt);

      Sem_Psl_Sequence (Stmt);
   end Sem_Psl_Cover_Directive;

   procedure Sem_Psl_Restrict_Directive (Stmt : Iir) is
   begin
      Sem_Psl_Sequence (Stmt);
   end Sem_Psl_Restrict_Directive;

   procedure Sem_Psl_Default_Clock (Stmt : Iir)
   is
      Expr : PSL_Node;
   begin
      if Current_Psl_Default_Clock /= Null_Iir
        and then Get_Parent (Current_Psl_Default_Clock) = Get_Parent (Stmt)
      then
         Report_Start_Group;
         Error_Msg_Sem
           (+Stmt, "redeclaration of PSL default clock in the same region");
         Error_Msg_Sem
           (+Current_Psl_Default_Clock,
            " (previous default clock declaration)");
         Report_End_Group;
      end if;
      Expr := Sem_Boolean (Get_Psl_Boolean (Stmt));
      Expr := PSL.Rewrites.Rewrite_Boolean (Expr);
      Set_Psl_Boolean (Stmt, Expr);
      Current_Psl_Default_Clock := Stmt;
   end Sem_Psl_Default_Clock;

   procedure Sem_Psl_Inherit_Spec (Item : Iir)
   is
      Name : Iir;
      Unit : Iir;
   begin
      --  Resolve name
      Name := Get_Name (Item);
      if Get_Kind (Name) = Iir_Kind_Simple_Name then
         Unit := Sem_Lib.Load_Primary_Unit
           (Libraries.Work_Library, Get_Identifier (Name), Item);
         if Unit = Null_Iir then
            Error_Msg_Sem (+Name, "unit %n was not analyzed", +Name);
            return;
         end if;
         Unit := Get_Library_Unit (Unit);
         Set_Named_Entity (Name, Unit);
      else
         Name := Sem_Names.Sem_Denoting_Name (Name);
         Unit := Get_Named_Entity (Name);
      end if;

      if Get_Kind (Unit) not in Iir_Kinds_Verification_Unit then
         Error_Msg_Sem (+Name, "%n must denote a verification unit", +Name);
         Set_Named_Entity (Name, Null_Iir);
         return;
      end if;

      --  Add items.
      Sem_Scopes.Add_Inherit_Spec (Item);
   end Sem_Psl_Inherit_Spec;

   function Sem_Psl_Instance_Name (Name : Iir) return Iir
   is
      Prefix : constant Iir := Get_Prefix (Name);
      Ent : constant Iir := Get_Named_Entity (Prefix);
      Decl : constant PSL_Node := Get_Psl_Declaration (Ent);
      Formal : PSL_Node;
      Assoc : Iir;
      Res : PSL_Node;
      Last_Assoc : PSL_Node;
      Assoc2 : PSL_Node;
      Actual : Iir;
      Psl_Actual : PSL_Node;
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
      Last_Assoc := Null_PSL_Node;

      while Formal /= Null_PSL_Node loop
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
            Psl_Actual := PSL.Hash.Get_PSL_Node
              (HDL_Node (Actual), Get_Location (Actual));
         end if;

         Assoc2 := Create_Node (N_Actual);
         Set_Location (Assoc2, Get_Location (Assoc));
         Set_Formal (Assoc2, Formal);
         Set_Actual (Assoc2, Psl_Actual);
         if Last_Assoc = Null_PSL_Node then
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

   procedure Sem_Hierarchical_Name (Hier_Name : Iir; Unit : Iir)
   is
      Entity_Name : Iir;
      Design_Entity : Iir;
      Lib_Entity : Iir;
      Arch_Name : Iir;
      Arch : Iir;
      Library : Iir_Library_Declaration;
   begin
      Entity_Name := Get_Entity_Name (Hier_Name);

      Library := Get_Library (Get_Design_File (Get_Design_Unit (Unit)));

      Design_Entity := Sem_Lib.Load_Primary_Unit
        (Library, Get_Identifier (Entity_Name), Entity_Name);
      if Design_Entity = Null_Iir then
         Error_Msg_Sem (+Entity_Name,
                        "entity %n was not analysed", +Entity_Name);
         return;
      end if;
      Lib_Entity := Get_Library_Unit (Design_Entity);

      if Get_Kind (Lib_Entity) /= Iir_Kind_Entity_Declaration then
         Error_Msg_Sem (+Entity_Name,
                        "name %i does not denote an entity", +Entity_Name);
         return;
      end if;

      Set_Named_Entity (Entity_Name, Lib_Entity);
      Xrefs.Xref_Ref (Entity_Name, Lib_Entity);

      Arch_Name := Get_Architecture (Hier_Name);
      if Arch_Name /= Null_Iir then
         Arch := Sem_Lib.Load_Secondary_Unit
           (Design_Entity, Get_Identifier (Arch_Name), Arch_Name);
         if Arch /= Null_Iir then
            Set_Named_Entity (Arch_Name, Get_Library_Unit (Arch));
         end if;
      end if;
   end Sem_Hierarchical_Name;

   procedure Sem_Psl_Verification_Unit (Unit : Iir)
   is
      Hier_Name : constant Iir := Get_Hierarchical_Name (Unit);
      Entity : Iir;
      Arch : Iir;
      Item : Iir;
      Prev_Item : Iir;
      Attr_Spec_Chain : Iir;
   begin
      if Hier_Name /= Null_Iir then
         --  Hierarchical name is optional.
         --  If the unit is not bound, the names are not bound too.
         Sem_Hierarchical_Name (Hier_Name, Unit);

         --  Import declarations.
         Entity := Get_Entity_Name (Hier_Name);
         if Entity = Null_Iir then
            return;
         end if;
         Entity := Get_Named_Entity (Entity);
         if Entity = Null_Iir then
            return;
         end if;

         Arch := Get_Architecture (Hier_Name);
         if Arch /= Null_Iir then
            Arch := Get_Named_Entity (Arch);
            if Arch = Null_Iir then
               return;
            end if;
         end if;

         Sem_Scopes.Add_Context_Clauses (Get_Design_Unit (Entity));
      else
         Entity := Null_Iir;
         Arch := Null_Iir;
      end if;

      Sem_Scopes.Open_Declarative_Region;

      if Entity /= Null_Iir then
         Set_Is_Within_Flag (Entity, True);
         Sem_Scopes.Add_Entity_Declarations (Entity);

         if Arch /= Null_Iir then
            Sem_Scopes.Open_Scope_Extension;
            Sem_Scopes.Extend_Scope_Of_Block_Declarations (Arch);
         end if;
      end if;

      Attr_Spec_Chain := Null_Iir;
      Prev_Item := Null_Iir;
      Item := Get_Vunit_Item_Chain (Unit);
      while Item /= Null_Iir loop
         case Get_Kind (Item) is
            when Iir_Kind_PSL_Inherit_Spec =>
               Sem_Psl_Inherit_Spec (Item);
            when Iir_Kind_Psl_Default_Clock =>
               Sem_Psl_Default_Clock (Item);
            when Iir_Kind_Psl_Assert_Directive =>
               Item := Sem_Psl_Assert_Directive (Item, False);
            when Iir_Kind_Psl_Assume_Directive =>
               Sem_Psl_Assume_Directive (Item);
            when Iir_Kind_Psl_Restrict_Directive =>
               Sem_Psl_Restrict_Directive (Item);
            when Iir_Kind_Psl_Cover_Directive =>
               Sem_Psl_Cover_Directive (Item);
            when Iir_Kind_Psl_Declaration =>
               Sem_Psl_Declaration (Item);
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
               Sem_Decls.Sem_Declaration
                 (Item, Prev_Item, False, Attr_Spec_Chain);
            when Iir_Kinds_Concurrent_Signal_Assignment
               | Iir_Kinds_Process_Statement
               | Iir_Kinds_Generate_Statement
               | Iir_Kind_Block_Statement
               | Iir_Kind_Concurrent_Procedure_Call_Statement
               | Iir_Kind_Component_Instantiation_Statement =>
               Sem_Stmts.Sem_Concurrent_Statement (Item, False);
            when others =>
               Error_Kind ("sem_psl_verification_unit", Item);
         end case;

         if Prev_Item = Null_Iir then
            Set_Vunit_Item_Chain (Unit, Item);
         else
            Set_Chain (Prev_Item, Item);
         end if;
         Prev_Item := Item;
         Item := Get_Chain (Item);
      end loop;

      if Arch /= Null_Iir then
         Sem_Scopes.Close_Scope_Extension;
      end if;

      Sem_Scopes.Close_Declarative_Region;
      if Entity /= Null_Iir then
         Set_Is_Within_Flag (Entity, False);
      end if;
   end Sem_Psl_Verification_Unit;

end Vhdl.Sem_Psl;
