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
with Std_Package;
with Ieee.Std_Logic_1164;
with Errorout; use Errorout;
with Xrefs; use Xrefs;

package body Sem_Psl is
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

   --  Semantize an HDL expression.  This may mostly a wrapper except in the
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
         Sem_Name (Expr, False);
         Name := Get_Named_Entity (Expr);
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
                  when N_Endpoint_Declaration =>
                     Res := Create_Node (N_Endpoint_Instance);
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
                  Error_Msg_Sem ("no actual for instantiation", Res);
               end if;
               Free_Node (N);
               Free_Iir (Expr);
               return Res;
            when Iir_Kind_Psl_Expression =>
               Free_Node (N);
               Free_Iir (Expr);
               return Get_Psl_Expression (Name);
            when others =>
               Expr := Name;
         end case;
      else
         Expr := Sem_Expr.Sem_Expression (Expr, Null_Iir);
      end if;

      if Expr = Null_Iir then
         return N;
      end if;
      Free_Node (N);
      if not Is_Psl_Bool_Expr (Expr) then
         Error_Msg_Sem ("type of expression must be boolean", Expr);
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
            return Sem_Hdl_Expr (Seq);
         when others =>
            Error_Kind ("psl.sem_sequence", Seq);
      end case;
   end Sem_Sequence;

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
            Res := Sem_Property (Get_Property (Prop), Top);
            Set_Property (Prop, Res);
            return Prop;
         when N_Eventually =>
            Res := Sem_Property (Get_Property (Prop));
            Set_Property (Prop, Res);
            return Prop;
         when N_Clock_Event =>
            Res := Sem_Property (Get_Property (Prop));
            Set_Property (Prop, Res);
            Res := Sem_Boolean (Get_Boolean (Prop));
            Set_Boolean (Prop, Res);
            if not Top then
               Error_Msg_Sem ("inner clock event not supported", Prop);
            end if;
            return Prop;
         when N_Abort =>
            Res := Sem_Property (Get_Property (Prop));
            Set_Property (Prop, Res);
            Res := Sem_Boolean (Get_Boolean (Prop));
            Set_Boolean (Prop, Res);
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
            Res := Sem_Property (Get_Property (Prop));
            Set_Property (Prop, Res);
            return Prop;
         when N_Next =>
            --  FIXME: number.
            Res := Sem_Property (Get_Property (Prop));
            Set_Property (Prop, Res);
            return Prop;
         when N_Next_A =>
            --  FIXME: range.
            Res := Sem_Property (Get_Property (Prop));
            Set_Property (Prop, Res);
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
                     Error_Msg_Sem ("property instance already has a clock",
                                    Prop);
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
      Decl : Node;
      Prop : Node;
      Clk : Node;
      Formal : Node;
      El : Iir;
   begin
      Sem_Scopes.Add_Name (Stmt);
      Xref_Decl (Stmt);

      Decl := Get_Psl_Declaration (Stmt);

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

   procedure Sem_Psl_Assert_Statement (Stmt : Iir)
   is
      Prop : Node;
      Clk : Node;
   begin
      Prop := Get_Psl_Property (Stmt);
      Prop := Sem_Property (Prop, True);
      Extract_Clock (Prop, Clk);
      Set_Psl_Property (Stmt, Prop);

      --  Properties must be clocked.
      if Clk = Null_Node then
         if Current_Psl_Default_Clock = Null_Iir then
            Error_Msg_Sem ("no clock for PSL assert", Stmt);
            Clk := Null_Node;
         else
            Clk := Get_Psl_Boolean (Current_Psl_Default_Clock);
         end if;
      end if;
      Set_PSL_Clock (Stmt, Clk);

      --  Check simple subset restrictions.
      PSL.Subsets.Check_Simple (Prop);
   end Sem_Psl_Assert_Statement;

   procedure Sem_Psl_Default_Clock (Stmt : Iir)
   is
      Expr : Node;
   begin
      if Current_Psl_Default_Clock /= Null_Iir
        and then Get_Parent (Current_Psl_Default_Clock) = Get_Parent (Stmt)
      then
         Error_Msg_Sem
           ("redeclaration of PSL default clock in the same region", Stmt);
         Error_Msg_Sem (" (previous default clock declaration)",
                        Current_Psl_Default_Clock);
      end if;
      Expr := Sem_Boolean (Get_Psl_Boolean (Stmt));
      Set_Psl_Boolean (Stmt, Expr);
      Current_Psl_Default_Clock := Stmt;
   end Sem_Psl_Default_Clock;

   function Sem_Psl_Instance_Name (Name : Iir) return Iir
   is
      Prefix : Iir;
      Ent : Iir;
      Decl : Node;
      Formal : Node;
      Assoc : Iir;
      Res : Node;
      Last_Assoc : Node;
      Assoc2 : Node;
      Actual : Iir;
      Psl_Actual : Node;
      Res2 : Iir;
   begin
      Prefix := Get_Prefix (Name);
      Ent := Get_Named_Entity (Prefix);
      pragma Assert (Get_Kind (Ent) = Iir_Kind_Psl_Declaration);
      Decl := Get_Psl_Declaration (Ent);
      case Get_Kind (Decl) is
         when N_Property_Declaration =>
            Res := Create_Node (N_Property_Instance);
         when N_Sequence_Declaration =>
            Res := Create_Node (N_Sequence_Instance);
         when N_Endpoint_Declaration =>
            Res := Create_Node (N_Endpoint_Instance);
         when others =>
            Error_Msg_Sem ("can only instantiate a psl declaration", Name);
            return Null_Iir;
      end case;
      Set_Declaration (Res, Decl);
      Set_Location (Res, Get_Location (Name));
      Formal := Get_Parameter_List (Decl);
      Assoc := Get_Association_Chain (Name);
      Last_Assoc := Null_Node;

      while Formal /= Null_Node loop
         if Assoc = Null_Iir then
            Error_Msg_Sem ("not enough association", Name);
            exit;
         end if;
         if Get_Kind (Assoc) /= Iir_Kind_Association_Element_By_Expression then
            Error_Msg_Sem
              ("open or individual association not allowed", Assoc);
         elsif Get_Formal (Assoc) /= Null_Iir then
            Error_Msg_Sem ("named association not allowed in psl", Assoc);
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
         Error_Msg_Sem ("too many association", Name);
      end if;

      Res2 := Create_Iir (Iir_Kind_Psl_Expression);
      Set_Psl_Expression (Res2, Res);
      Location_Copy (Res2, Name);
      return Res2;
   end Sem_Psl_Instance_Name;

   --  Called by sem_names to semantize a psl name.
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
