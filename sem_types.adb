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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Libraries;
with Flags;
with Types; use Types;
with Errorout; use Errorout;
with Evaluation; use Evaluation;
with Sem;
with Sem_Expr; use Sem_Expr;
with Sem_Scopes; use Sem_Scopes;
with Sem_Names; use Sem_Names;
with Sem_Decls;
with Std_Names;
with Iirs_Utils; use Iirs_Utils;
with Std_Package; use Std_Package;
with Xrefs; use Xrefs;

package body Sem_Types is
   procedure Set_Type_Has_Signal (Atype : Iir)
   is
   begin
      --  Sanity check.
      if not Get_Signal_Type_Flag (Atype) then
         --  Do not crash since this may be called on an erroneous design.
         return;
      end if;

      --  If the type is already marked, nothing to do.
      if Get_Has_Signal_Flag (Atype) then
         return;
      end if;

      Set_Has_Signal_Flag (Atype, True);

      case Get_Kind (Atype) is
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Floating_Type_Definition =>
            null;
         when Iir_Kinds_Subtype_Definition =>
            declare
               Func : Iir_Function_Declaration;
               Mark : Iir;
            begin
               Set_Type_Has_Signal (Get_Base_Type (Atype));
               --  Mark the resolution function (this may be required by the
               --  back-end to generate resolver).
               if Get_Resolved_Flag (Atype) then
                  Func := Get_Resolution_Function (Atype);
                  --  Maybe the type is resolved through its elements.
                  if Func /= Null_Iir then
                     Func := Get_Named_Entity (Func);
                     Set_Resolution_Function_Flag (Func, True);
                  end if;
               end if;
               Mark := Get_Type_Mark (Atype);
               if Mark /= Null_Iir then
                  Set_Type_Has_Signal (Mark);
               end if;
            end;
         when Iir_Kind_Array_Type_Definition =>
            Set_Type_Has_Signal (Get_Element_Subtype (Atype));
         when Iir_Kind_Record_Type_Definition =>
            declare
               El : Iir;
            begin
               El := Get_Element_Declaration_Chain (Atype);
               while El /= Null_Iir loop
                  Set_Type_Has_Signal (Get_Type (El));
                  El := Get_Chain (El);
               end loop;
            end;
         when Iir_Kind_Error =>
            null;
         when Iir_Kind_Incomplete_Type_Definition =>
            --  No need to copy the flag.
            null;
         when others =>
            Error_Kind ("set_type_has_signal(2)", Atype);
      end case;
   end Set_Type_Has_Signal;

   --  Sem a range expression.
   --  Both left and right bounds must be of the same type kind, ie
   --  integer types, or if INT_ONLY is false, real types.
   --  However, the two bounds need not have the same type.
   function Sem_Range_Expression (Expr : Iir; Int_Only : Boolean) return Iir
   is
      Left, Right: Iir;
      Bt_L_Kind, Bt_R_Kind : Iir_Kind;
   begin
      Left := Sem_Expression_Universal (Get_Left_Limit (Expr));
      Right := Sem_Expression_Universal (Get_Right_Limit (Expr));
      if Left = Null_Iir or Right = Null_Iir then
         return Null_Iir;
      end if;
      Set_Left_Limit (Expr, Left);
      Set_Right_Limit (Expr, Right);

      Set_Expr_Staticness (Expr, Min (Get_Expr_Staticness (Left),
                                      Get_Expr_Staticness (Right)));

      Bt_L_Kind := Get_Kind (Get_Base_Type (Get_Type (Left)));
      Bt_R_Kind := Get_Kind (Get_Base_Type (Get_Type (Right)));

      if Int_Only then
         if Bt_L_Kind /= Iir_Kind_Integer_Type_Definition
           and then Bt_R_Kind = Iir_Kind_Integer_Type_Definition
         then
            Error_Msg_Sem ("left bound must be an integer expression", Left);
            return Null_Iir;
         end if;
         if Bt_R_Kind /= Iir_Kind_Integer_Type_Definition
           and then Bt_L_Kind = Iir_Kind_Integer_Type_Definition
         then
            Error_Msg_Sem ("right bound must be an integer expression", Left);
            return Null_Iir;
         end if;
         if Bt_R_Kind /= Iir_Kind_Integer_Type_Definition
           and then Bt_L_Kind /= Iir_Kind_Integer_Type_Definition
         then
            Error_Msg_Sem ("each bound must be an integer expression", Expr);
            return Null_Iir;
         end if;
      else
         if Bt_L_Kind /= Bt_R_Kind then
            Error_Msg_Sem ("left and right bounds must be of the same type",
                           Expr);
            return Null_Iir;
         end if;
         case Bt_L_Kind is
            when Iir_Kind_Integer_Type_Definition
              | Iir_Kind_Floating_Type_Definition =>
               null;
         when others =>
            --  Enumeration range are not allowed to define a new type.
            Error_Msg_Sem
              ("bad range type, only integer or float is allowed", Expr);
            return Null_Iir;
         end case;
      end if;

      return Expr;
   end Sem_Range_Expression;

   function Create_Integer_Type (Loc : Iir; Constraint : Iir; Decl : Iir)
     return Iir
   is
      Ntype: Iir_Integer_Subtype_Definition;
      Ndef: Iir_Integer_Type_Definition;
   begin
      Ntype := Create_Iir (Iir_Kind_Integer_Subtype_Definition);
      Location_Copy (Ntype, Loc);
      Ndef := Create_Iir (Iir_Kind_Integer_Type_Definition);
      Location_Copy (Ndef, Loc);
      Set_Base_Type (Ndef, Ndef);
      Set_Type_Declarator (Ndef, Decl);
      Set_Type_Staticness (Ndef, Locally);
      Set_Signal_Type_Flag (Ndef, True);
      Set_Base_Type (Ntype, Ndef);
      Set_Type_Declarator (Ntype, Decl);
      Set_Range_Constraint (Ntype, Constraint);
      Set_Type_Staticness (Ntype, Get_Expr_Staticness (Constraint));
      Set_Resolved_Flag (Ntype, False);
      Set_Signal_Type_Flag (Ntype, True);
      if Get_Type_Staticness (Ntype) /= Locally then
         Error_Msg_Sem ("range constraint of type must be locally static",
                        Decl);
      end if;
      return Ntype;
   end Create_Integer_Type;

   function Range_Expr_To_Type_Definition (Expr : Iir; Decl: Iir)
     return Iir
   is
      Left, Right : Iir;
   begin
      if Sem_Range_Expression (Expr, False) = Null_Iir then
         return Null_Iir;
      end if;
      Left := Get_Left_Limit (Expr);
      Right := Get_Right_Limit (Expr);
      if Get_Expr_Staticness (Expr) = Locally then
         Left := Eval_Expr (Left);
         Set_Left_Limit (Expr, Left);
         Right := Eval_Expr (Right);
         Set_Right_Limit (Expr, Right);
      end if;

      case Get_Kind (Get_Base_Type (Get_Type (Left))) is
         when Iir_Kind_Integer_Type_Definition =>
            return Create_Integer_Type (Expr, Expr, Decl);
         when Iir_Kind_Floating_Type_Definition =>
            declare
               Ntype: Iir_Floating_Subtype_Definition;
               Ndef: Iir_Floating_Type_Definition;
            begin
               Ntype := Create_Iir (Iir_Kind_Floating_Subtype_Definition);
               Location_Copy (Ntype, Expr);
               Ndef := Create_Iir (Iir_Kind_Floating_Type_Definition);
               Location_Copy (Ndef, Expr);
               Set_Base_Type (Ndef, Ndef);
               Set_Type_Declarator (Ndef, Decl);
               Set_Type_Staticness (Ndef, Get_Expr_Staticness (Expr));
               Set_Signal_Type_Flag (Ndef, True);
               Set_Base_Type (Ntype, Ndef);
               Set_Type_Declarator (Ntype, Decl);
               Set_Range_Constraint (Ntype, Expr);
               Set_Resolved_Flag (Ntype, False);
               Set_Type_Staticness (Ntype, Get_Expr_Staticness (Expr));
               Set_Signal_Type_Flag (Ntype, True);
               return Ntype;
            end;
         when others =>
            --  sem_range_expression should catch such errors.
            raise Internal_Error;
      end case;
   end Range_Expr_To_Type_Definition;

   function Create_Physical_Literal (Val : Iir_Int64; Unit : Iir) return Iir
   is
      Lit : Iir;
   begin
      Lit := Create_Iir (Iir_Kind_Physical_Int_Literal);
      Set_Value (Lit, Val);
      Set_Unit_Name (Lit, Unit);
      Set_Expr_Staticness (Lit, Locally);
      Set_Type (Lit, Get_Type (Unit));
      Location_Copy (Lit, Unit);
      return Lit;
   end Create_Physical_Literal;

   -- Sem a physical type definition.  Create a subtype.
   function Sem_Physical_Type_Definition (Range_Expr: Iir; Decl : Iir)
      return Iir_Physical_Subtype_Definition
   is
      Unit: Iir_Unit_Declaration;
      Def : Iir_Physical_Type_Definition;
      Sub_Type: Iir_Physical_Subtype_Definition;
      Range_Expr1: Iir;
      Val : Iir;
      Lit : Iir_Physical_Int_Literal;
   begin
      Def := Get_Type (Range_Expr);

      --  LRM93 §4.1
      --  The simple name declared by a type declaration denotes the
      --  declared type, unless the type declaration declares both a base
      --  type and a subtype of the base type, in which case the simple name
      --  denotes the subtype, and the base type is anonymous.
      Set_Type_Declarator (Def, Decl);
      Set_Base_Type (Def, Def);
      Set_Resolved_Flag (Def, False);
      Set_Type_Staticness (Def, Locally);
      Set_Signal_Type_Flag (Def, True);

      --  LRM93 §3.1.3
      --  Each bound of a range constraint that is used in a physical type
      --  definition must be a locally static expression of some integer type
      --  but the two bounds need not have the same integer type.
      case Get_Kind (Range_Expr) is
         when Iir_Kind_Range_Expression =>
            Range_Expr1 := Sem_Range_Expression (Range_Expr, True);
         when others =>
            Error_Kind ("sem_physical_type_definition", Range_Expr);
      end case;
      if Range_Expr1 /= Null_Iir then
         if Get_Expr_Staticness (Range_Expr1) /= Locally then
            Error_Msg_Sem
              ("range constraint for a physical type must be static",
               Range_Expr1);
            Range_Expr1 := Null_Iir;
         else
            Range_Expr1 := Eval_Expr (Range_Expr1);
         end if;
      end if;

      --  Create the subtype.
      Sub_Type := Create_Iir (Iir_Kind_Physical_Subtype_Definition);
      Location_Copy (Sub_Type, Range_Expr);
      Set_Base_Type (Sub_Type, Def);
      Set_Signal_Type_Flag (Sub_Type, True);

      --  Sem primary units.
      Unit := Get_Unit_Chain (Def);

      Lit := Create_Physical_Literal (1, Unit);
      Set_Physical_Unit_Value (Unit, Lit);

      Add_Name (Unit);
      Set_Type (Unit, Def);
      Set_Expr_Staticness (Unit, Locally);
      Set_Visible_Flag (Unit, True);
      Xref_Decl (Unit);

      --  Sem secondary units.
      Unit := Get_Chain (Unit);
      while Unit /= Null_Iir loop
         --  Val := Sem_Physical_Literal (Get_Multiplier (Unit));
         Val := Sem_Expression (Get_Physical_Literal (Unit), Def);
         if Val /= Null_Iir then
            Val := Eval_Expr (Val);
            Set_Physical_Literal (Unit, Val);
            if Get_Kind (Val) = Iir_Kind_Unit_Declaration then
               Val := Create_Physical_Literal (1, Val);
            end if;
            Set_Physical_Unit_Value (Unit, Val);

            --  LRM93 §3.1
            --  The position number of unit names need not lie within the range
            --  specified by the range constraint.
            --  GHDL: this was not true in VHDL87.
            --  GHDL: This is not so simple if 1 is not included in the range.
            if False and then Flags.Vhdl_Std = Vhdl_87
              and then Range_Expr1 /= Null_Iir
            then
               if not Eval_Int_In_Range (Get_Value (Unit), Range_Expr1) then
                  Error_Msg_Sem
                    ("physical literal does not lie within the range", Unit);
               end if;
            end if;
         else
            --  Avoid errors storm.
            Set_Physical_Literal (Unit, Get_Primary_Unit (Def));
            Set_Physical_Unit_Value (Unit, Lit);
         end if;

         Sem_Scopes.Add_Name (Unit);
         Set_Type (Unit, Def);
         Set_Expr_Staticness (Unit, Locally);
         Sem_Scopes.Name_Visible (Unit);
         Xref_Decl (Unit);
         Unit := Get_Chain (Unit);
      end loop;

      if Range_Expr1 /= Null_Iir then
         declare
            --  Convert an integer literal to a physical literal.
            --  This is used to convert bounds.
            function Lit_To_Phys_Lit (Lim : Iir_Integer_Literal)
              return Iir_Physical_Int_Literal
            is
               Res : Iir_Physical_Int_Literal;
            begin
               Res := Create_Iir (Iir_Kind_Physical_Int_Literal);
               Location_Copy (Res, Lim);
               Set_Type (Res, Def);
               Set_Value (Res, Get_Value (Lim));
               Set_Unit_Name (Res, Get_Primary_Unit (Def));
               Set_Expr_Staticness (Res, Locally);
               Set_Literal_Origin (Res, Lim);
               return Res;
            end Lit_To_Phys_Lit;

            Phys_Range : Iir_Range_Expression;
         begin
            --  Create the physical range.
            Phys_Range := Create_Iir (Iir_Kind_Range_Expression);
            Location_Copy (Phys_Range, Range_Expr1);
            Set_Type (Phys_Range, Def);
            Set_Direction (Phys_Range, Get_Direction (Range_Expr1));
            Set_Left_Limit
              (Phys_Range, Lit_To_Phys_Lit (Get_Left_Limit (Range_Expr1)));
            Set_Right_Limit
              (Phys_Range, Lit_To_Phys_Lit (Get_Right_Limit (Range_Expr1)));
            Set_Expr_Staticness
              (Phys_Range, Get_Expr_Staticness (Range_Expr1));

            Set_Range_Constraint (Sub_Type, Phys_Range);
            --  This must be locally...
            Set_Type_Staticness (Sub_Type, Get_Expr_Staticness (Range_Expr1));
         end;
      end if;
      Set_Resolved_Flag (Sub_Type, False);

      return Sub_Type;
   end Sem_Physical_Type_Definition;

   --  Return true iff decl is std.textio.text
   function Is_Text_Type_Declaration (Decl : Iir_Type_Declaration)
     return Boolean
   is
      use Std_Names;
      P : Iir;
   begin
      if Get_Identifier (Decl) /= Name_Text then
         return False;
      end if;
      P := Get_Parent (Decl);
      if Get_Kind (P) /= Iir_Kind_Package_Declaration
        or else Get_Identifier (P) /= Name_Textio
      then
         return False;
      end if;
      --  design_unit, design_file, library_declaration.
      P := Get_Library (Get_Design_File (Get_Design_Unit (P)));
      if P /= Libraries.Std_Library then
         return False;
      end if;
      return True;
   end Is_Text_Type_Declaration;

   procedure Check_No_File_Type (El_Type : Iir; Loc : Iir) is
   begin
      case Get_Kind (El_Type) is
         when Iir_Kind_File_Type_Definition =>
            Error_Msg_Sem
              ("element of file type is not allowed in a composite type", Loc);
         when others =>
            null;
      end case;
   end Check_No_File_Type;

   --  Semantize the array_element type of DEF.
   --  Set type_staticness and resolved_flag of DEF.
   --  type_staticness of DEF (before calling this function) must be the
   --  staticness of the array indexes.
   procedure Sem_Array_Element (Def : Iir)
   is
      El_Type : Iir;
   begin
      El_Type := Get_Element_Subtype (Def);
      El_Type := Sem_Subtype_Indication (El_Type);
      if El_Type = Null_Iir then
         Set_Type_Staticness (Def, None);
         Set_Resolved_Flag (Def, False);
         Set_Element_Subtype (Def, Error_Type);
         return;
      end if;
      Set_Element_Subtype (Def, El_Type);
      Check_No_File_Type (El_Type, Def);
      Set_Signal_Type_Flag (Def, Get_Signal_Type_Flag (El_Type));

      --  LRM93 §3.2.1.1
      --  The same requirement exists [must define a constrained
      --  array subtype] [...] for the element subtype indication
      --  of an array type definition, if the type of the array
      --  element is itself an array type.
      if not Sem_Is_Constrained (El_Type) then
         Error_Msg_Sem ("array element of unconstrained "
                        & Disp_Node (El_Type) & " is not allowed", Def);
      end if;
      Set_Type_Staticness (Def, Min (Get_Type_Staticness (El_Type),
                                     Get_Type_Staticness (Def)));
      Set_Resolved_Flag (Def, Get_Resolved_Flag (El_Type));
   end Sem_Array_Element;

   procedure Sem_Protected_Type_Declaration (Type_Decl : Iir_Type_Declaration)
   is
      Decl : Iir_Protected_Type_Declaration;
      El : Iir;
   begin
      Decl := Get_Type (Type_Decl);
      Set_Base_Type (Decl, Decl);
      Set_Resolved_Flag (Decl, False);
      Set_Signal_Type_Flag (Decl, False);
      Set_Type_Staticness (Decl, None);

      --  LRM 10.3 Visibility
      --  [...] except in the declaration of a design_unit or a protected type
      --  declaration, in which case it starts immediatly after the reserved
      --  word is occuring after the identifier of the design unit or
      --  protected type declaration.
      Set_Visible_Flag (Type_Decl, True);

      --  LRM 10.1
      --  n) A protected type declaration, together with the corresponding
      --     body.
      Open_Declarative_Region;

      Sem_Decls.Sem_Declaration_Chain (Decl, False);
      El := Get_Declaration_Chain (Decl);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Use_Clause
              | Iir_Kind_Attribute_Specification =>
               null;
            when Iir_Kind_Procedure_Declaration
              | Iir_Kind_Function_Declaration =>
               declare
                  Inter : Iir;
                  Inter_Type : Iir;
               begin
                  Inter := Get_Interface_Declaration_Chain (El);
                  while Inter /= Null_Iir loop
                     Inter_Type := Get_Type (Inter);
                     if Inter_Type /= Null_Iir
                       and then Get_Signal_Type_Flag (Inter_Type) = False
                       and then Get_Kind (Inter_Type)
                       /= Iir_Kind_Protected_Type_Declaration
                     then
                        Error_Msg_Sem
                          ("formal parameter method must not be "
                           & "access or file type", Inter);
                     end if;
                     Inter := Get_Chain (Inter);
                  end loop;
                  if Get_Kind (El) = Iir_Kind_Function_Declaration then
                     Inter_Type := Get_Return_Type (El);
                     if Inter_Type /= Null_Iir
                       and then Get_Signal_Type_Flag (Inter_Type) = False
                     then
                        Error_Msg_Sem
                          ("method return type must not be access of file",
                           El);
                     end if;
                  end if;
               end;
            when others =>
               Error_Msg_Sem
                 (Disp_Node (El)
                  & " are not allowed in protected type declaration", El);
         end case;
         El := Get_Chain (El);
      end loop;

      Close_Declarative_Region;
   end Sem_Protected_Type_Declaration;

   procedure Sem_Protected_Type_Body (Bod : Iir)
   is
      Inter : Name_Interpretation_Type;
      Type_Decl : Iir;
      Decl : Iir;
      El : Iir;
   begin
      --  LRM 3.5 Protected types.
      --  Each protected type declaration appearing immediatly within a given
      --  declaration region must have exactly one corresponding protected type
      --  body appearing immediatly within the same declarative region and
      --  textually subsequent to the protected type declaration.
      --
      --  Similarly, each protected type body appearing immediatly within a
      --  given declarative region must have exactly one corresponding
      --  protected type declaration appearing immediatly within the same
      --  declarative region and textually prior to the protected type body.
      Inter := Get_Interpretation (Get_Identifier (Bod));
      if Valid_Interpretation (Inter)
        and then Is_In_Current_Declarative_Region (Inter)
      then
         Type_Decl := Get_Declaration (Inter);
         if Get_Kind (Type_Decl) = Iir_Kind_Type_Declaration then
            Decl := Get_Type (Type_Decl);
         else
            Decl := Null_Iir;
         end if;
      else
         Decl := Null_Iir;
      end if;

      if Decl /= Null_Iir
        and then Get_Kind (Decl) = Iir_Kind_Protected_Type_Declaration
      then
         Set_Protected_Type_Declaration (Bod, Decl);
         if Get_Protected_Type_Body (Decl) /= Null_Iir then
            Error_Msg_Sem
              ("protected type body already declared for "
               & Disp_Node (Decl), Bod);
            Error_Msg_Sem
              ("(previous body)", Get_Protected_Type_Body (Decl));
            Decl := Null_Iir;
         elsif not Get_Visible_Flag (Type_Decl) then
            --  Can this happen ?
            Error_Msg_Sem
              ("protected type declaration not yet visible", Bod);
            Error_Msg_Sem
              ("(location of protected type declaration)", Decl);
            Decl := Null_Iir;
         else
            Set_Protected_Type_Body (Decl, Bod);
         end if;
      else
         Error_Msg_Sem
           ("no protected type declaration for this body", Bod);
         if Decl /= Null_Iir then
            Error_Msg_Sem
              ("(found " & Disp_Node (Decl) & " declared here)", Decl);
            Decl := Null_Iir;
         end if;
      end if;

      --  LRM 10.1
      --  n) A protected type declaration, together with the corresponding
      --     body.
      Open_Declarative_Region;

      if Decl /= Null_Iir then
         Xref_Body (Bod, Decl);
         Add_Protected_Type_Declarations (Decl);
      end if;

      Sem_Decls.Sem_Declaration_Chain (Bod, False);

      El := Get_Declaration_Chain (Bod);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Procedure_Declaration
              | Iir_Kind_Function_Declaration
              | Iir_Kind_Implicit_Procedure_Declaration
              | Iir_Kind_Implicit_Function_Declaration =>
               null;
            when Iir_Kind_Procedure_Body
              | Iir_Kind_Function_Body =>
               null;
            when Iir_Kind_Type_Declaration
              | Iir_Kind_Anonymous_Type_Declaration =>
               null;
            when Iir_Kind_Subtype_Declaration
              | Iir_Kind_Constant_Declaration
              | Iir_Kind_Variable_Declaration
              | Iir_Kind_File_Declaration =>
               null;
            when Iir_Kind_Object_Alias_Declaration
              | Iir_Kind_Non_Object_Alias_Declaration =>
               null;
            when Iir_Kind_Attribute_Declaration
              | Iir_Kind_Attribute_Specification
              | Iir_Kind_Use_Clause
              | Iir_Kind_Group_Template_Declaration
              | Iir_Kind_Group_Declaration =>
               null;
            when others =>
               Error_Msg_Sem
                 (Disp_Node (El) & " not allowed in a protected type body",
                  El);
         end case;
         El := Get_Chain (El);
      end loop;
      Sem_Decls.Check_Full_Declaration (Bod, Bod);

      --  LRM 3.5.2 Protected type bodies
      --  Each subprogram declaration appearing in a given protected type
      --  declaration shall have a corresponding subprogram body appearing in
      --  the corresponding protected type body.
      if Decl /= Null_Iir then
         Sem_Decls.Check_Full_Declaration (Decl, Bod);
      end if;

      Close_Declarative_Region;
   end Sem_Protected_Type_Body;

   function Sem_Type_Definition (Def: Iir; Decl: Iir) return Iir
   is
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Enumeration_Type_Definition =>
            Set_Base_Type (Def, Def);
            Set_Type_Staticness (Def, Locally);
            Set_Signal_Type_Flag (Def, True);

            Create_Range_Constraint_For_Enumeration_Type (Def);

            --  Makes all literal visible.
            declare
               El: Iir;
               Literal_List: Iir_List;
            begin
               Literal_List := Get_Enumeration_Literal_List (Def);
               for I in Natural loop
                  El := Get_Nth_Element (Literal_List, I);
                  exit when El = Null_Iir;
                  Set_Expr_Staticness (El, Locally);
                  Set_Name_Staticness (El, Locally);
                  Set_Base_Name (El, El);
                  Set_Type (El, Def);
                  Set_Enumeration_Decl (El, El);
                  Sem.Compute_Subprogram_Hash (El);
                  Sem_Scopes.Add_Name (El);
                  Name_Visible (El);
                  Xref_Decl (El);
               end loop;
            end;
            Set_Resolved_Flag (Def, False);
            return Def;

         when Iir_Kind_Range_Expression =>
            if Get_Type (Def) /= Null_Iir then
               return Sem_Physical_Type_Definition (Def, Decl);
            else
               return Range_Expr_To_Type_Definition (Def, Decl);
            end if;

         when Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Attribute_Name
           | Iir_Kind_Parenthesis_Name =>
            if Get_Type (Def) /= Null_Iir then
               return Sem_Physical_Type_Definition (Def, Decl);
            end if;
            --  Nb: the attribute is expected to be a 'range or
            --  a 'reverse_range attribute.
            declare
               Res : Iir;
            begin
               Res := Sem_Discrete_Range_Expression (Def, Null_Iir, True);
               if Res = Null_Iir then
                  return Null_Iir;
               end if;
               --  This cannot be a floating range.
               return Create_Integer_Type (Def, Res, Decl);
            end;

         when Iir_Kind_Array_Subtype_Definition =>
            declare
               Index_Type : Iir;
               Index_List : Iir_List;
               Base_Index_List : Iir_List;
               Staticness : Iir_Staticness;

               -- array_type_definition, which is the same as the subtype,
               -- but without any constraint in the indexes.
               Base_Type: Iir;
            begin
               -- FIXME: all indexes must be either constrained or
               -- unconstrained.
               -- If all indexes are unconstrained, this is really a type
               -- otherwise, this is a subtype.

               -- Create a definition for the base type of subtype DEF.
               Base_Type := Create_Iir (Iir_Kind_Array_Type_Definition);
               Location_Copy (Base_Type, Def);
               Set_Base_Type (Base_Type, Base_Type);
               Set_Type_Declarator (Base_Type, Decl);
               Base_Index_List := Create_Iir_List;
               Set_Index_Subtype_List (Base_Type, Base_Index_List);

               Staticness := Locally;
               Index_List := Get_Index_Subtype_List (Def);
               for I in Natural loop
                  Index_Type := Get_Nth_Element (Index_List, I);
                  exit when Index_Type = Null_Iir;

                  Index_Type := Sem_Discrete_Range_Integer (Index_Type);
                  if Index_Type /= Null_Iir then
                     Index_Type := Range_To_Subtype_Definition (Index_Type);
                  else
                     --  Avoid errors.
                     Index_Type := Natural_Subtype_Definition;
                  end if;

                  Replace_Nth_Element (Index_List, I, Index_Type);
                  Staticness := Min (Staticness,
                                     Get_Type_Staticness (Index_Type));

                  -- Set the index type in the array type.
                  -- must "unconstraint" the subtype.
                  Append_Element (Base_Index_List, Index_Type);
               end loop;
               Set_Type_Staticness (Def, Staticness);

               -- Element type.
               Sem_Array_Element (Def);

               Set_Element_Subtype (Base_Type, Get_Element_Subtype (Def));
               Set_Signal_Type_Flag (Base_Type, Get_Signal_Type_Flag (Def));
               --  According to LRM93 §7.4.1, an unconstrained array type
               --  is not static.
               Set_Type_Staticness (Base_Type, None);
               Set_Type_Declarator (Base_Type, Decl);
               Set_Resolved_Flag (Base_Type, Get_Resolved_Flag (Def));

               Set_Base_Type (Def, Base_Type);
               Set_Type_Mark (Def, Base_Type);
               return Def;
            end;

         when Iir_Kind_Array_Type_Definition =>
            declare
               Index_Type : Iir;
               Index_List : Iir_List;
            begin
               Set_Base_Type (Def, Def);
               Index_List := Get_Index_Subtype_List (Def);

               for I in Natural loop
                  Index_Type := Get_Nth_Element (Index_List, I);
                  exit when Index_Type = Null_Iir;

                  Index_Type := Sem_Subtype_Indication (Index_Type);
                  if Index_Type /= Null_Iir then
                     if Get_Kind (Index_Type) not in
                       Iir_Kinds_Discrete_Type_Definition
                     then
                        Error_Msg_Sem
                          ("index type of an array must be discrete",
                           Index_Type);
                     end if;
                  else
                     --  Avoid errors.
                     Index_Type := Natural_Subtype_Definition;
                  end if;

                  Replace_Nth_Element (Index_List, I, Index_Type);
               end loop;

               --  According to LRM93 §7.4.1, an unconstrained array type
               --  is not static.
               Set_Type_Staticness (Def, None);

               Sem_Array_Element (Def);
               return Def;
            end;

         when Iir_Kind_Record_Type_Definition =>
            declare
               --  Non semantized type of previous element.
               Last_El_Type : Iir;
               --  Semantized type of previous element
               Last_Type : Iir;

               El: Iir;
               El_Type : Iir;
               Resolved_Flag : Boolean;
               Staticness : Iir_Staticness;
            begin
               --  LRM 10.1
               --  5. A record type declaration,
               Open_Declarative_Region;

               Resolved_Flag := True;
               Last_El_Type := Null_Iir;
               Last_Type := Null_Iir;
               Staticness := Locally;
               Set_Signal_Type_Flag (Def, True);
               El := Get_Element_Declaration_Chain (Def);
               while El /= Null_Iir loop
                  El_Type := Get_Type (El);
                  if El_Type /= Last_El_Type then
                     --  Be careful for a declaration list (r,g,b: integer).
                     Last_El_Type := El_Type;
                     El_Type := Sem_Subtype_Indication (El_Type);
                     Last_Type := El_Type;
                  else
                     El_Type := Last_Type;
                  end if;
                  if El_Type /= Null_Iir then
                     Set_Type (El, El_Type);
                     Check_No_File_Type (El_Type, El);
                     if not Get_Signal_Type_Flag (El_Type) then
                        Set_Signal_Type_Flag (Def, False);
                     end if;

                     --  LRM93 §3.2.1.1
                     --  The same requirement [must define a constrained array
                     --  subtype] exits for the subtype indication of an
                     --  element declaration, if the type of the record
                     --  element is an array type.
                     if not Sem_Is_Constrained (El_Type) then
                        Error_Msg_Sem
                          ("element declaration of unconstrained "
                           & Disp_Node (El_Type) & " is not allowed", El);
                     end if;
                     Resolved_Flag :=
                       Resolved_Flag and Get_Resolved_Flag (El_Type);
                     Staticness := Min (Staticness,
                                        Get_Type_Staticness (El_Type));
                  else
                     Staticness := None;
                  end if;
                  Sem_Scopes.Add_Name (El);
                  Name_Visible (El);
                  Xref_Decl (El);
                  El := Get_Chain (El);
               end loop;
               Close_Declarative_Region;
               Set_Base_Type (Def, Def);
               Set_Resolved_Flag (Def, Resolved_Flag);
               Set_Type_Staticness (Def, Staticness);
               return Def;
            end;

         when Iir_Kind_Access_Type_Definition =>
            declare
               D_Type : Iir;
            begin
               D_Type := Sem_Subtype_Indication (Get_Designated_Type (Def),
                                                 True);
               if D_Type /= Null_Iir then
                  case Get_Kind (D_Type) is
                     when Iir_Kind_Incomplete_Type_Definition =>
                        Append_Element
                          (Get_Incomplete_Type_List (D_Type), Def);
                     when Iir_Kind_File_Type_Definition =>
                        --  LRM 3.3
                        --  The designated type must not be a file type.
                        Error_Msg_Sem
                          ("designated type must not be a file type", Def);
                     when others =>
                        null;
                  end case;
                  Set_Designated_Type (Def, D_Type);
               end if;
               Set_Base_Type (Def, Def);
               Set_Type_Staticness (Def, None);
               Set_Resolved_Flag (Def, False);
               Set_Signal_Type_Flag (Def, False);
               return Def;
            end;

         when Iir_Kind_File_Type_Definition =>
            declare
               Type_Mark : Iir;
            begin
               Type_Mark := Sem_Subtype_Indication (Get_Type_Mark (Def));
               Set_Type_Mark (Def, Type_Mark);
               if Type_Mark /= Null_Iir then
                  if Get_Signal_Type_Flag (Type_Mark) = False then
                     --  LRM 3.4
                     --  The base type of this subtype must not be a file type
                     --  or an access type.
                     --  If the base type is a composite type, it must not
                     --  contain a subelement of an access type.
                     Error_Msg_Sem
                       (Disp_Node (Type_Mark) & " cannot be a file type", Def);
                  elsif Get_Kind (Type_Mark) in Iir_Kinds_Array_Type_Definition
                  then
                     --  LRM 3.4
                     --  If the base type is an array type, it must be a one
                     --  dimensional array type.
                     if not Is_Unidim_Array_Type (Type_Mark) then
                        Error_Msg_Sem
                          ("multi-dimensional " & Disp_Node (Type_Mark)
                           & " cannot be a file type", Def);
                     end if;
                  end if;
               end if;
               Set_Base_Type (Def, Def);
               Set_Resolved_Flag (Def, False);
               Set_Text_File_Flag (Def, Is_Text_Type_Declaration (Decl));
               Set_Signal_Type_Flag (Def, False);
               Set_Type_Staticness (Def, None);
               return Def;
            end;

         when Iir_Kind_Protected_Type_Declaration =>
            Sem_Protected_Type_Declaration (Decl);
            return Def;

         when others =>
            Error_Kind ("sem_type_definition", Def);
            return Def;
      end case;
   end Sem_Type_Definition;

   --  Convert a range expression to a subtype definition whose constraint is
   --  A_RANGE.
   --  This function extract the type of the range expression.
   function Range_To_Subtype_Definition (A_Range: Iir) return Iir
   is
      Sub_Type: Iir;
      Range_Type : Iir;
   begin
      case Get_Kind (A_Range) is
         when Iir_Kind_Range_Expression
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute =>
            --  Create a sub type.
            Range_Type := Get_Type (A_Range);
         when Iir_Kinds_Discrete_Type_Definition =>
            --  A_RANGE is already a subtype definition.
            return A_Range;
         when others =>
            Error_Kind ("range_to_subtype_definition", A_Range);
            return Null_Iir;
      end case;

      case Get_Kind (Range_Type) is
         when Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            Sub_Type := Create_Iir (Iir_Kind_Enumeration_Subtype_Definition);
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Integer_Subtype_Definition =>
            Sub_Type := Create_Iir (Iir_Kind_Integer_Subtype_Definition);
         when others =>
            raise Internal_Error;
      end case;
      Location_Copy (Sub_Type, A_Range);
      Set_Range_Constraint (Sub_Type, A_Range);
      Set_Base_Type (Sub_Type, Get_Base_Type (Range_Type));
      Set_Type_Staticness (Sub_Type, Get_Expr_Staticness (A_Range));
      Set_Signal_Type_Flag (Sub_Type, True);
      return Sub_Type;
   end Range_To_Subtype_Definition;

   -- Return TRUE iff FUNC is a resolution function for ATYPE.
   function Is_A_Resolution_Function (Func: Iir; Atype: Iir) return Boolean
   is
      Decl: Iir;
      Decl_Type : Iir;
      Ret_Type : Iir;
   begin
      -- LRM93 2.4
      --  A resolution function must be a [pure] function;
      if Get_Kind (Func) not in Iir_Kinds_Function_Declaration then
         return False;
      end if;
      Decl := Get_Interface_Declaration_Chain (Func);
      -- LRM93 2.4
      --  moreover, it must have a single input parameter of class constant
      if Decl = Null_Iir or else Get_Chain (Decl) /= Null_Iir then
         return False;
      end if;
      if Get_Kind (Decl) /= Iir_Kind_Constant_Interface_Declaration then
         return False;
      end if;
      -- LRM93 2.4
      --  that is a one-dimensional, unconstrained array
      Decl_Type := Get_Type (Decl);
      if Get_Kind (Decl_Type) /= Iir_Kind_Array_Type_Definition then
         return False;
      end if;
      if Get_Nbr_Elements (Get_Index_Subtype_List (Decl_Type)) /= 1 then
         return False;
      end if;
      -- LRM93 2.4
      --  whose element type is that of the resolved signal.
      --  The type of the return value of the function must also be that of
      --  the signal.
      Ret_Type := Get_Return_Type (Func);
      if Get_Base_Type (Get_Element_Subtype (Decl_Type))
        /= Get_Base_Type (Ret_Type)
      then
         return False;
      end if;
      if Atype /= Null_Iir
        and then Get_Base_Type (Ret_Type) /= Get_Base_Type (Atype)
      then
         return False;
      end if;
      -- LRM93 2.4
      --  A resolution function must be a [pure] function;
      if Flags.Vhdl_Std >= Vhdl_93 and then Get_Pure_Flag (Func) = False then
         if Atype /= Null_Iir then
            Error_Msg_Sem
              ("resolution " & Disp_Node (Func) & " must be pure", Atype);
         end if;
         return False;
      end if;
      return True;
   end Is_A_Resolution_Function;

   --  Note: this sets resolved_flag.
   procedure Sem_Resolution_Function (Decl: Iir)
   is
      Func: Iir;
      Name : Iir;
      Res: Iir;
      El : Iir;
      List : Iir_List;
      Has_Error : Boolean;
   begin
      Name := Get_Resolution_Function (Decl);
      if Name = Null_Iir then
         -- This is not a resolved type.
         return;
      end if;

      -- FIXME: add this check (maybe based on resolved_flag ?)
      --if Get_Kind (Name) in Iir_Kinds_Function_Declaration then
      --   --  The resolution function was already semantized.
      --   --  This can happen if comes from an unconstrained array subtype.
      --   return;
      --end if;

      Sem_Name (Name, False);
      Func := Get_Named_Entity (Name);
      if Func = Error_Mark then
         return;
      end if;

      Res := Null_Iir;

      if Is_Overload_List (Func) then
         List := Get_Overload_List (Func);
         Has_Error := False;
         for I in Natural loop
            El := Get_Nth_Element (List, I);
            exit when El = Null_Iir;
            if Is_A_Resolution_Function (El, Decl) then
               if Res /= Null_Iir then
                  if not Has_Error then
                     Has_Error := True;
                     Error_Msg_Sem
                       ("can't resolve overload for resolution function",
                        Decl);
                     Error_Msg_Sem ("candidate functions are:", Decl);
                     Error_Msg_Sem (" " & Disp_Subprg (Func), Func);
                  end if;
                  Error_Msg_Sem (" " & Disp_Subprg (El), El);
               else
                  Res := El;
               end if;
            end if;
         end loop;
         if Has_Error then
            return;
         end if;
      else
         if Is_A_Resolution_Function (Func, Decl) then
            Res := Func;
         end if;
      end if;

      if Res = Null_Iir then
         Error_Msg_Sem ("no matching resolution function for "
                        & Disp_Node (Name), Decl);
      else
         Set_Named_Entity (Name, Res);
         Set_Use_Flag (Res, True);
         Set_Resolved_Flag (Decl, True);
         Xref_Name (Name);
      end if;
   end Sem_Resolution_Function;

   --  Semantize array_subtype_definition DEF using TYPE_MARK as the base type
   --  of DEF.
   --  DEF must have an index list and may have a resolution function.
   --  Return DEF.
   function Sem_Array_Subtype_Indication (Type_Mark : Iir; Def : Iir)
     return Iir
   is
      Type_Index, Subtype_Index: Iir;
      Base_Type : Iir;
      El_Type : Iir;
      Staticness : Iir_Staticness;
      Error_Seen : Boolean;
      Type_Index_List : Iir_List;
      Subtype_Index_List : Iir_List;
   begin
      case Get_Kind (Type_Mark) is
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Unconstrained_Array_Subtype_Definition =>
            null;
         when others =>
            Error_Msg_Sem
              (Disp_Node (Type_Mark) & " cannot be constrained", Def);
            --  Continue as if BASE_TYPE is really a base type, it is safe.
      end case;

      Base_Type := Get_Base_Type (Type_Mark);
      Set_Base_Type (Def, Base_Type);
      El_Type := Get_Element_Subtype (Base_Type);
      Staticness := Get_Type_Staticness (El_Type);
      Error_Seen := False;
      Type_Index_List := Get_Index_Subtype_List (Base_Type);
      Subtype_Index_List := Get_Index_Subtype_List (Def);
      for I in Natural loop
         Type_Index := Get_Nth_Element (Type_Index_List, I);
         Subtype_Index := Get_Nth_Element (Subtype_Index_List, I);
         exit when Type_Index = Null_Iir and Subtype_Index = Null_Iir;

         if Type_Index = Null_Iir then
            Error_Msg_Sem ("subtype has more indexes than "
                           & Disp_Node (Type_Mark)
                           & " defined at " & Disp_Location (Type_Mark),
                           Subtype_Index);
            --  Forget extra indexes.
            Set_Nbr_Elements (Subtype_Index_List, I);
            exit;
         end if;
         if Subtype_Index = Null_Iir then
            if not Error_Seen then
               Error_Msg_Sem ("subtype has less indexes than "
                              & Disp_Node (Type_Mark)
                              & " defined at " & Disp_Location (Type_Mark),
                              Def);
               Error_Seen := True;
            end if;
            --  Use type_index as a fake subtype
            --  FIXME: it is too fake.
            Append_Element (Subtype_Index_List, Type_Index);
            Staticness := None;
         else
            Subtype_Index := Sem_Discrete_Range_Expression
              (Subtype_Index, Type_Index, True);
            if Subtype_Index /= Null_Iir then
               Subtype_Index := Range_To_Subtype_Definition (Subtype_Index);
               Staticness := Min (Staticness,
                                  Get_Type_Staticness (Subtype_Index));
            end if;
            if Subtype_Index = Null_Iir then
               --  Create a fake subtype from type_index.
               --  FIXME: It is too fake.
               Subtype_Index := Type_Index;
               Staticness := None;
            end if;
            Replace_Nth_Element (Subtype_Index_List, I, Subtype_Index);
         end if;
      end loop;
      Set_Type_Staticness (Def, Staticness);
      Set_Element_Subtype (Def, El_Type);
      Sem_Resolution_Function (Def);
      if Get_Resolved_Flag (Def) or else Get_Resolved_Flag (El_Type) then
         Set_Resolved_Flag (Def, True);
      else
         Set_Resolved_Flag (Def, False);
      end if;
      Set_Type_Mark (Def, Type_Mark);
      Set_Signal_Type_Flag (Def, Get_Signal_Type_Flag (Type_Mark));
      return Def;
   end Sem_Array_Subtype_Indication;

   --  Semantize a subtype indication.
   --  DEF can be either a name or an iir_subtype_definition.
   --  Return a new (an anonymous) subtype definition (with the correct kind),
   --  or an already defined type definition (if DEF is a name).
   function Sem_Subtype_Indication (Def: Iir; Incomplete : Boolean := False)
     return Iir
   is
      Type_Mark: Iir;
      Res: Iir;
      Decl_Kind : Decl_Kind_Type;
   begin
      if Incomplete then
         Decl_Kind := Decl_Incomplete_Type;
      else
         Decl_Kind := Decl_Type;
      end if;

      -- Simple case that correspond to no indication except a subtype
      -- identifier
      if Get_Kind (Def) in Iir_Kinds_Name then
         Type_Mark := Find_Declaration (Def, Decl_Kind);
         if Type_Mark = Null_Iir then
            return Create_Error_Type (Def);
         else
            return Type_Mark;
         end if;
      end if;

      --  Semantize the type mark.
      Type_Mark := Find_Declaration (Get_Type_Mark (Def), Decl_Kind);
      if Type_Mark = Null_Iir then
         --  FIXME: handle inversion such as "subtype BASETYPE RESOLV", which
         --  should emit "resolution function must precede type name".
         return Create_Error_Type (Get_Type_Mark (Def));
      end if;
      Set_Type_Mark (Def, Type_Mark);

      --  Check constraint.
      case Get_Kind (Def) is
         when Iir_Kind_Array_Subtype_Definition =>
            case Get_Kind (Type_Mark) is
               when Iir_Kind_Unconstrained_Array_Subtype_Definition
                 | Iir_Kind_Array_Type_Definition
                 | Iir_Kind_Access_Type_Definition =>
                  null;
               when others =>
                  --  LRM 3.2.1.1  Index Constraints and Discrete Ranges
                  --  If an index constraint appears after a type mark [...]
                  --  The type mark must denote either an unconstrained array
                  --  type, or an access type whose designated type is such
                  --  an array type.
                  Error_Msg_Sem
                    ("only unconstrained array type may be contrained "
                     &"by index", Def);
                  Error_Msg_Sem
                    (" (type mark is " & Disp_Node (Type_Mark) & ")",
                     Type_Mark);
                  return Type_Mark;
            end case;
         when Iir_Kind_Subtype_Definition =>
            case Get_Kind (Type_Mark) is
               when Iir_Kind_Integer_Subtype_Definition
                 | Iir_Kind_Floating_Subtype_Definition
                 | Iir_Kind_Enumeration_Subtype_Definition
                 | Iir_Kind_Physical_Subtype_Definition =>
                  null;
               when Iir_Kind_Enumeration_Type_Definition =>
                  null;
               when others =>
                  --  FIXME: find the correct sentence from LRM
                  --  GHDL: subtype_definition may also be used just to add
                  --    a resolution function.
                  if Get_Range_Constraint (Def) /= Null_Iir then
                     Error_Msg_Sem
                       ("only scalar types may be constrained by range", Def);
                     Error_Msg_Sem
                       (" (type mark is " & Disp_Node (Type_Mark) & ")",
                        Type_Mark);
                     return Type_Mark;
                  end if;
            end case;
         when others =>
            Error_Kind ("sem_subtype_indication", Def);
      end case;

      case Get_Kind (Type_Mark) is
         when Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Unconstrained_Array_Subtype_Definition =>
--             --  If the base type is an unconstrained array subtype, then get
--            --  the *real* base type, and copy the resolution function (since
--             --  a base type has no resolution function).
--             if Get_Kind (Type_Mark) =
--               Iir_Kind_Unconstrained_Array_Subtype_Definition
--               and then Get_Kind (Def) = Iir_Kind_Subtype_Definition
--             then
--                if Get_Resolution_Function (Def) = Null_Iir then
--                   if Get_Range_Constraint (Def) = Null_Iir then
--                      --  In this case, DEF must simply be a name.  There is
--                      --  a parser internal error.
--                      raise Internal_Error;
--                   end if;
--                   Set_Resolution_Function
--                     (Def, Get_Resolution_Function (Type_Mark));
--                end if;
--             end if;

            if Get_Kind (Def) = Iir_Kind_Subtype_Definition then
               -- This is the case of "subtype new_array is [func] old_array".
               -- def must be a constrained array.
               if Get_Range_Constraint (Def) /= Null_Iir then
                  Error_Msg_Sem
                    ("cannot use a range constraint for an array", Def);
                  return Type_Mark;
               end if;
               if Get_Resolution_Function (Def) = Null_Iir then
                  --  In this case, DEF must simply be a name.  There is
                  --  a parser internal error.
                  raise Internal_Error;
               end if;
               case Get_Kind (Type_Mark) is
                  when Iir_Kind_Array_Type_Definition =>
                     Res := Create_Iir
                       (Iir_Kind_Unconstrained_Array_Subtype_Definition);
                  when Iir_Kind_Array_Subtype_Definition =>
                     Res := Create_Iir (Iir_Kind_Array_Subtype_Definition);
                     Set_Element_Subtype
                       (Res, Get_Element_Subtype (Type_Mark));
                     Set_Index_Subtype_List
                       (Res, Get_Index_Subtype_List (Type_Mark));
                  when others =>
                     Error_Kind ("sem_subtype_indication(array)", Type_Mark);
               end case;
               Location_Copy (Res, Def);
               Set_Base_Type (Res, Get_Base_Type (Type_Mark));
               Set_Resolution_Function (Res, Get_Resolution_Function (Def));
               Set_Type_Staticness (Res, Get_Type_Staticness (Type_Mark));
               Sem_Resolution_Function (Res);
               Set_Signal_Type_Flag (Res, Get_Signal_Type_Flag (Type_Mark));
               if Get_Resolved_Flag (Res)
                 or else Get_Resolved_Flag (Get_Element_Subtype (Type_Mark))
               then
                  Set_Resolved_Flag (Res, True);
               else
                  Set_Resolved_Flag (Res, False);
               end if;
               Set_Type_Mark (Res, Type_Mark);
               Free_Name (Def);
               return Res;
            elsif Get_Kind (Def) = Iir_Kind_Array_Subtype_Definition then
               -- Case of a constraint for an array.
               -- Check each index constraint against array type.
               return Sem_Array_Subtype_Indication (Type_Mark, Def);
            else
               Error_Kind ("sem_subtype_indication(1)", Def);
               return Type_Mark;
            end if;

         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition =>
            if Get_Range_Constraint (Def) = Null_Iir
              and then Get_Resolution_Function (Def) = Null_Iir
            then
               --  This defines an alias, and must have been handled just
               --  before the case statment.
               raise Internal_Error;
            end if;
            declare
               A_Range : Iir;
            begin
               -- There are limits.  Create a new subtype.
               Res := Create_Iir (Get_Kind (Type_Mark));
               Location_Copy (Res, Def);
               Set_Base_Type (Res, Get_Base_Type (Type_Mark));
               Set_Type_Mark (Res, Type_Mark);
               Set_Resolution_Function (Res, Get_Resolution_Function (Def));
               A_Range := Get_Range_Constraint (Def);
               if A_Range = Null_Iir then
                  A_Range := Get_Range_Constraint (Type_Mark);
               else
                  A_Range := Sem_Discrete_Range_Expression
                    (A_Range, Type_Mark, True);
                  if A_Range = Null_Iir then
                     --  Avoid error propagation.
                     A_Range := Get_Range_Constraint (Type_Mark);
                  end if;
               end if;
               Set_Range_Constraint (Res, A_Range);
               Set_Type_Staticness (Res, Get_Expr_Staticness (A_Range));
               Free_Name (Def);
               Sem_Resolution_Function (Res);
               Set_Signal_Type_Flag (Res, Get_Signal_Type_Flag (Type_Mark));
               return Res;
            end;

         when Iir_Kind_Enumeration_Type_Definition =>
            if Get_Range_Constraint (Def) = Null_Iir and then
              Get_Resolution_Function (Def) = Null_Iir
            then
               raise Internal_Error;
            end if;

            declare
               Constraint : Iir_Range_Expression;
            begin
               -- There are limits.  Create a new subtype.
               Res := Create_Iir (Iir_Kind_Enumeration_Subtype_Definition);
               Location_Copy (Res, Def);
               Set_Base_Type (Res, Type_Mark);
               Set_Type_Mark (Res, Type_Mark);
               Set_Resolution_Function (Res, Get_Resolution_Function (Def));
               Constraint := Get_Range_Constraint (Def);
               if Constraint = Null_Iir then
                  Constraint := Get_Range_Constraint (Type_Mark);
               else
                  Constraint := Sem_Discrete_Range_Expression
                    (Constraint, Type_Mark, True);
                  -- FIXME: check bounds, check static
               end if;
               Set_Range_Constraint (Res, Constraint);
               Set_Type_Staticness (Res, Get_Expr_Staticness (Constraint));
            end;
            Free_Name (Def);
            Sem_Resolution_Function (Res);
            Set_Signal_Type_Flag (Res, True);
            return Res;

         when Iir_Kind_Record_Type_Definition =>
            declare
               Func: Iir;
            begin
               if Get_Kind (Def) /= Iir_Kind_Subtype_Definition then
                  Error_Kind ("sem_subtype_indication1", Def);
                  return Null_Iir;
               end if;
               Func := Get_Resolution_Function (Def);
               if Func = Null_Iir then
                  --  This is an alias.
                  raise Internal_Error;
               end if;
               Res := Create_Iir (Iir_Kind_Record_Subtype_Definition);
               Location_Copy (Res, Def);
               Set_Base_Type (Res, Type_Mark);
               Set_Type_Staticness (Res, Get_Type_Staticness (Type_Mark));
               Set_Type_Mark (Res, Type_Mark);
               Set_Resolution_Function (Res, Func);
               Sem_Resolution_Function (Res);
               Set_Signal_Type_Flag (Res, Get_Signal_Type_Flag (Type_Mark));
               Free_Name (Def);
               return Res;
            end;

         when Iir_Kind_Access_Type_Definition =>
            --  LRM93 4.2
            --  A subtype indication denoting an access type [or a file type]
            --  may not contain a resolution function.
            if Get_Resolution_Function (Def) /= Null_Iir then
               Error_Msg_Sem
                 ("resolution function not allowed for an access type", Def);
            end if;

            case Get_Kind (Def) is
               when Iir_Kind_Subtype_Definition =>
                  Free_Name (Def);
                  return Type_Mark;
               when Iir_Kind_Array_Subtype_Definition =>
                  --  LRM93 §3.3
                  --  The only form of constraint that is allowed after a name
                  --  of an access type in a subtype indication is an index
                  --  constraint.
                  declare
                     Sub_Type : Iir;
                     pragma Unreferenced (Sub_Type);
                     Base_Type : Iir;
                  begin
                     Base_Type := Get_Designated_Type (Type_Mark);
                     Sub_Type := Sem_Array_Subtype_Indication (Base_Type, Def);
                     Res := Create_Iir (Iir_Kind_Access_Subtype_Definition);
                     Location_Copy (Res, Def);
                     Set_Base_Type (Res, Type_Mark);
                     Set_Signal_Type_Flag (Res, False);
                     Free_Old_Iir (Def);
                     return Res;
                  end;
               when others =>
                  raise Internal_Error;
            end case;

         when Iir_Kind_File_Type_Definition =>
            if Get_Kind (Def) = Iir_Kind_Subtype_Definition then
               Free_Name (Def);
               return Type_Mark;
            else
               raise Internal_Error;
            end if;

         when others =>
            Error_Kind ("sem_subtype_indication", Type_Mark);
            return Def;
      end case;
   end Sem_Subtype_Indication;

   function Sem_Is_Constrained (A_Type: Iir) return Boolean is
   begin
      case Get_Kind (A_Type) is
         when Iir_Kind_Array_Subtype_Definition =>
            return True;
         when Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_Access_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_File_Type_Definition =>
            --| Iir_Kind_File_Subtype_Definition =>
            return True;
         when Iir_Kind_Protected_Type_Declaration =>
            return True;
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Unconstrained_Array_Subtype_Definition =>
            return False;
         when Iir_Kind_Incomplete_Type_Definition =>
            return False;
         when Iir_Kind_Error =>
            return True;
         when others =>
            Error_Kind ("sem_is_constrained", A_Type);
      end case;
   end Sem_Is_Constrained;

end Sem_Types;
