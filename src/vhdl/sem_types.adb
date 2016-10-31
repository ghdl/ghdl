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
with Libraries;
with Flags; use Flags;
with Types; use Types;
with Errorout; use Errorout;
with Evaluation; use Evaluation;
with Sem;
with Sem_Expr; use Sem_Expr;
with Sem_Scopes; use Sem_Scopes;
with Sem_Names; use Sem_Names;
with Sem_Decls;
with Sem_Inst;
with Name_Table;
with Std_Names;
with Iirs_Utils; use Iirs_Utils;
with Std_Package; use Std_Package;
with Ieee.Std_Logic_1164;
with Xrefs; use Xrefs;

package body Sem_Types is
   --  Mark the resolution function (this may be required by the back-end to
   --  generate resolver).
   procedure Mark_Resolution_Function (Subtyp : Iir)
   is
      Func : Iir_Function_Declaration;
   begin
      if not Get_Resolved_Flag (Subtyp) then
         return;
      end if;

      Func := Has_Resolution_Function (Subtyp);
      --  Maybe the type is resolved through its elements.
      if Func /= Null_Iir then
         Set_Resolution_Function_Flag (Func, True);
      end if;
   end Mark_Resolution_Function;

   procedure Set_Type_Has_Signal (Atype : Iir)
   is
      Orig : Iir;
   begin
      --  Sanity check: ATYPE can be a signal type (eg: not an access type)
      if not Get_Signal_Type_Flag (Atype) then
         --  Do not crash since this may be called on an erroneous design.
         return;
      end if;

      --  If the type is already marked, nothing to do.
      if Get_Has_Signal_Flag (Atype) then
         return;
      end if;

      --  This type is used to declare a signal.
      Set_Has_Signal_Flag (Atype, True);

      --  If this type was instantiated, also mark the origin.
      Orig := Sem_Inst.Get_Origin (Atype);
      if Orig /= Null_Iir then
         Set_Type_Has_Signal (Orig);
      end if;

      --  Mark resolution function, and for composite types, also mark type
      --  of elements.
      case Get_Kind (Atype) is
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Floating_Type_Definition =>
            null;
         when Iir_Kinds_Scalar_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            Set_Type_Has_Signal (Get_Base_Type (Atype));
            Mark_Resolution_Function (Atype);
         when Iir_Kind_Array_Subtype_Definition =>
            Set_Type_Has_Signal (Get_Base_Type (Atype));
            Mark_Resolution_Function (Atype);
            Set_Type_Has_Signal (Get_Element_Subtype (Atype));
         when Iir_Kind_Array_Type_Definition =>
            Set_Type_Has_Signal (Get_Element_Subtype (Atype));
         when Iir_Kind_Record_Type_Definition =>
            declare
               El_List : constant Iir_List :=
                 Get_Elements_Declaration_List (Atype);
               El : Iir;
            begin
               for I in Natural loop
                  El := Get_Nth_Element (El_List, I);
                  exit when El = Null_Iir;
                  Set_Type_Has_Signal (Get_Type (El));
               end loop;
            end;
         when Iir_Kind_Error =>
            null;
         when Iir_Kind_Incomplete_Type_Definition =>
            --  No need to copy the flag.
            null;
         when Iir_Kind_Interface_Type_Definition =>
            null;
         when others =>
            Error_Kind ("set_type_has_signal(2)", Atype);
      end case;
   end Set_Type_Has_Signal;

   --  Sem a range expression that appears in an integer, real or physical
   --  type definition.
   --
   --  Both left and right bounds must be of the same type class, ie
   --  integer types, or if INT_ONLY is false, real types.
   --  However, the two bounds need not have the same type.
   function Sem_Type_Range_Expression (Expr : Iir; Int_Only : Boolean)
                                      return Iir
   is
      Left, Right: Iir;
      Bt_L_Kind, Bt_R_Kind : Iir_Kind;
   begin
      Left := Sem_Expression_Universal (Get_Left_Limit_Expr (Expr));
      Right := Sem_Expression_Universal (Get_Right_Limit_Expr (Expr));
      if Left = Null_Iir or Right = Null_Iir then
         return Null_Iir;
      end if;

      --  Emit error message for overflow and replace with a value to avoid
      --  error storm.
      if Get_Kind (Left) = Iir_Kind_Overflow_Literal then
         Error_Msg_Sem (+Left, "overflow in left bound");
         Left := Build_Extreme_Value
           (Get_Direction (Expr) = Iir_Downto, Left);
      end if;
      if Get_Kind (Right) = Iir_Kind_Overflow_Literal then
         Error_Msg_Sem (+Right, "overflow in right bound");
         Right := Build_Extreme_Value
           (Get_Direction (Expr) = Iir_To, Right);
      end if;
      Set_Left_Limit_Expr (Expr, Left);
      Set_Right_Limit_Expr (Expr, Right);
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
            Error_Msg_Sem (+Left, "left bound must be an integer expression");
            return Null_Iir;
         end if;
         if Bt_R_Kind /= Iir_Kind_Integer_Type_Definition
           and then Bt_L_Kind = Iir_Kind_Integer_Type_Definition
         then
            Error_Msg_Sem
              (+Right, "right bound must be an integer expression");
            return Null_Iir;
         end if;
         if Bt_R_Kind /= Iir_Kind_Integer_Type_Definition
           and then Bt_L_Kind /= Iir_Kind_Integer_Type_Definition
         then
            Error_Msg_Sem (+Expr, "each bound must be an integer expression");
            return Null_Iir;
         end if;
      else
         if Bt_L_Kind /= Bt_R_Kind then
            Error_Msg_Sem
              (+Expr, "left and right bounds must be of the same type class");
            return Null_Iir;
         end if;
         case Bt_L_Kind is
            when Iir_Kind_Integer_Type_Definition
              | Iir_Kind_Floating_Type_Definition =>
               null;
         when others =>
            --  Enumeration range are not allowed to define a new type.
            Error_Msg_Sem
              (+Expr, "bad range type, only integer or float is allowed");
            return Null_Iir;
         end case;
      end if;

      return Expr;
   end Sem_Type_Range_Expression;

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
         Error_Msg_Sem
           (+Decl, "range constraint of type must be locally static");
      end if;
      return Ntype;
   end Create_Integer_Type;

   function Range_Expr_To_Type_Definition (Expr : Iir; Decl: Iir)
     return Iir
   is
      Rng : Iir;
      Res : Iir;
      Base_Type : Iir;
   begin
      if Sem_Type_Range_Expression (Expr, False) = Null_Iir then
         return Null_Iir;
      end if;
      Rng := Eval_Range_If_Static (Expr);
      if Get_Expr_Staticness (Rng) /= Locally then
         --  FIXME: create an artificial range to avoid error storm ?
         null;
      end if;

      case Get_Kind (Get_Base_Type (Get_Type (Get_Left_Limit (Rng)))) is
         when Iir_Kind_Integer_Type_Definition =>
            Res := Create_Integer_Type (Expr, Rng, Decl);
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
               Set_Range_Constraint (Ntype, Rng);
               Set_Resolved_Flag (Ntype, False);
               Set_Type_Staticness (Ntype, Get_Expr_Staticness (Expr));
               Set_Signal_Type_Flag (Ntype, True);
               Res := Ntype;
            end;
         when others =>
            --  sem_range_expression should catch such errors.
            raise Internal_Error;
      end case;

      --  A type and a subtype were declared.  The type of the bounds are now
      --  used for the implicit subtype declaration.  But the type of the
      --  bounds aren't of the type of the type declaration (this is 'obvious'
      --  because they exist before the type declaration).  Override their
      --  type.  This is doable without destroying information as they are
      --  either literals (of type convertible_xx_type_definition) or an
      --  evaluated literal.
      --
      --  Overriding makes these implicit subtype homogenous with explicit
      --  subtypes.
      Base_Type := Get_Base_Type (Res);
      Set_Type (Rng, Base_Type);
      Set_Type (Get_Left_Limit (Rng), Base_Type);
      Set_Type (Get_Right_Limit (Rng), Base_Type);

      return Res;
   end Range_Expr_To_Type_Definition;

   function Create_Physical_Literal (Val : Iir_Int64; Unit : Iir) return Iir
   is
      Lit : Iir;
   begin
      Lit := Create_Iir (Iir_Kind_Physical_Int_Literal);
      Set_Value (Lit, Val);
      Set_Physical_Unit (Lit, Unit);
      Set_Expr_Staticness (Lit, Locally);
      Set_Type (Lit, Get_Type (Unit));
      Location_Copy (Lit, Unit);
      return Lit;
   end Create_Physical_Literal;

   --  Analyze a physical type definition.  Create a subtype.
   function Sem_Physical_Type_Definition (Def : Iir; Decl : Iir)
      return Iir_Physical_Subtype_Definition
   is
      Unit: Iir_Unit_Declaration;
      Sub_Type: Iir_Physical_Subtype_Definition;
      Range_Expr : Iir;
      Range_Expr1: Iir;
      Val : Iir;
      Lit : Iir_Physical_Int_Literal;
   begin
      Range_Expr := Get_Range_Constraint (Def);

      --  LRM93 4.1
      --  The simple name declared by a type declaration denotes the
      --  declared type, unless the type declaration declares both a base
      --  type and a subtype of the base type, in which case the simple name
      --  denotes the subtype, and the base type is anonymous.
      Set_Type_Declarator (Def, Decl);
      Set_Base_Type (Def, Def);
      Set_Resolved_Flag (Def, False);
      Set_Type_Staticness (Def, Locally);
      Set_Signal_Type_Flag (Def, True);

      --  LRM93 3.1.3
      --  Each bound of a range constraint that is used in a physical type
      --  definition must be a locally static expression of some integer type
      --  but the two bounds need not have the same integer type.
      case Get_Kind (Range_Expr) is
         when Iir_Kind_Range_Expression =>
            Range_Expr1 := Sem_Type_Range_Expression (Range_Expr, True);
         when Iir_Kind_Attribute_Name =>
            Sem_Name (Range_Expr);
            Range_Expr1 := Name_To_Range (Range_Expr);
         when others =>
            Error_Kind ("sem_physical_type_definition", Range_Expr);
      end case;
      if Range_Expr1 = Null_Iir or else Is_Error (Range_Expr1) then
         --  Avoid cascading errors.
         Range_Expr1 :=
           Get_Range_Constraint (Universal_Integer_Subtype_Definition);
      end if;
      if Get_Expr_Staticness (Range_Expr1) /= Locally then
         Error_Msg_Sem (+Range_Expr1,
                        "range constraint for a physical type must be static");
         Range_Expr1 :=
           Get_Range_Constraint (Universal_Integer_Subtype_Definition);
      else
         Range_Expr1 := Eval_Range_If_Static (Range_Expr1);
      end if;

      --  Create the subtype.
      Sub_Type := Create_Iir (Iir_Kind_Physical_Subtype_Definition);
      Location_Copy (Sub_Type, Range_Expr);
      Set_Base_Type (Sub_Type, Def);
      Set_Signal_Type_Flag (Sub_Type, True);

      --  Analyze the primary unit.
      Unit := Get_Unit_Chain (Def);

      --  Set its value to 1.
      Set_Type (Unit, Def);
      Set_Expr_Staticness (Unit, Locally);
      Set_Name_Staticness (Unit, Locally);
      Lit := Create_Physical_Literal (1, Unit);
      Set_Physical_Literal (Unit, Lit);

      Sem_Scopes.Add_Name (Unit);
      Set_Visible_Flag (Unit, True);
      Xref_Decl (Unit);

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
            Set_Physical_Unit (Res, Get_Primary_Unit (Def));
            Set_Expr_Staticness (Res, Locally);
            Set_Literal_Origin (Res, Lim);
            return Res;
         end Lit_To_Phys_Lit;

         Phys_Range : Iir_Range_Expression;
         Lit : Iir;
      begin
         --  Create the physical range.
         Phys_Range := Create_Iir (Iir_Kind_Range_Expression);
         Location_Copy (Phys_Range, Range_Expr1);
         Set_Type (Phys_Range, Def);
         Set_Direction (Phys_Range, Get_Direction (Range_Expr1));
         Lit := Lit_To_Phys_Lit (Get_Left_Limit (Range_Expr1));
         Set_Left_Limit (Phys_Range, Lit);
         Set_Left_Limit_Expr (Phys_Range, Lit);
         Lit := Lit_To_Phys_Lit (Get_Right_Limit (Range_Expr1));
         Set_Right_Limit (Phys_Range, Lit);
         Set_Right_Limit_Expr (Phys_Range, Lit);
         Set_Expr_Staticness
           (Phys_Range, Get_Expr_Staticness (Range_Expr1));

         Set_Range_Constraint (Sub_Type, Phys_Range);
         --  This must be locally...
         Set_Type_Staticness (Sub_Type, Get_Expr_Staticness (Range_Expr1));

         --  FIXME: the original range is not used.  Reuse it ?
         Free_Iir (Range_Expr);
      end;
      Set_Resolved_Flag (Sub_Type, False);

      --  Analyze secondary units.
      Unit := Get_Chain (Unit);
      while Unit /= Null_Iir loop
         Sem_Scopes.Add_Name (Unit);
         Val := Sem_Expression (Get_Physical_Literal (Unit), Def);
         if Val /= Null_Iir then
            Val := Eval_Physical_Literal (Val);
            Set_Physical_Literal (Unit, Val);

            --  LRM93 �3.1
            --  The position number of unit names need not lie within the range
            --  specified by the range constraint.
            --  GHDL: this was not true in VHDL87.
            --  GHDL: This is not so simple if 1 is not included in the range.
            if False and then Flags.Vhdl_Std = Vhdl_87
              and then Range_Expr1 /= Null_Iir
            then
               if not Eval_Int_In_Range (Get_Value (Unit), Range_Expr1) then
                  Error_Msg_Sem
                    (+Unit, "physical literal does not lie within the range");
               end if;
            end if;
         else
            --  Avoid errors storm.
            Val := Create_Physical_Literal (1, Get_Primary_Unit (Def));
            Set_Literal_Origin (Val, Get_Physical_Literal (Unit));
            Set_Physical_Literal (Unit, Val);
         end if;

         Set_Type (Unit, Def);
         Set_Expr_Staticness (Unit, Locally);
         Set_Name_Staticness (Unit, Locally);
         Sem_Scopes.Name_Visible (Unit);
         Xref_Decl (Unit);
         Unit := Get_Chain (Unit);
      end loop;

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
              (+Loc, "file type element not allowed in a composite type");
         when Iir_Kind_Protected_Type_Declaration =>
            Error_Msg_Sem
              (+Loc, "protected type element not allowed in a composite type");
         when others =>
            null;
      end case;
   end Check_No_File_Type;

   --  Analyze the array_element type of array type DEF.
   --  Set resolved_flag of DEF.
   procedure Sem_Array_Element (Def : Iir)
   is
      El_Type : Iir;
   begin
      El_Type := Get_Element_Subtype_Indication (Def);
      El_Type := Sem_Subtype_Indication (El_Type);
      if El_Type = Null_Iir then
         Set_Type_Staticness (Def, None);
         Set_Resolved_Flag (Def, False);
         return;
      end if;
      Set_Element_Subtype_Indication (Def, El_Type);

      El_Type := Get_Type_Of_Subtype_Indication (El_Type);
      Set_Element_Subtype (Def, El_Type);
      Check_No_File_Type (El_Type, Def);
      Set_Signal_Type_Flag (Def, Get_Signal_Type_Flag (El_Type));

      --  LRM93 �3.2.1.1
      --  The same requirement exists [must define a constrained
      --  array subtype] [...] for the element subtype indication
      --  of an array type definition, if the type of the array
      --  element is itself an array type.
      if Vhdl_Std < Vhdl_08
        and then not Is_Fully_Constrained_Type (El_Type)
      then
         Error_Msg_Sem
           (+Def, "array element of unconstrained %n is not allowed",
            +El_Type);
      end if;
      Set_Resolved_Flag (Def, Get_Resolved_Flag (El_Type));
   end Sem_Array_Element;

   procedure Sem_Protected_Type_Declaration (Type_Decl : Iir_Type_Declaration)
   is
      Decl : Iir_Protected_Type_Declaration;
      El : Iir;
   begin
      Decl := Get_Type_Definition (Type_Decl);
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

      Sem_Decls.Sem_Declaration_Chain (Decl);
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
                          (+Inter, "formal parameter method must not be "
                           & "access or file type");
                     end if;
                     Inter := Get_Chain (Inter);
                  end loop;
                  if Get_Kind (El) = Iir_Kind_Function_Declaration then
                     Inter_Type := Get_Return_Type (El);
                     if Inter_Type /= Null_Iir
                       and then Get_Signal_Type_Flag (Inter_Type) = False
                     then
                        Error_Msg_Sem
                          (+El,
                           "method return type must not be access of file");
                     end if;
                  end if;
               end;
            when others =>
               Error_Msg_Sem
                 (+El, "%n are not allowed in protected type declaration",
                  +El);
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
            Decl := Get_Type_Definition (Type_Decl);
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
              (+Bod, "protected type body already declared for %n",
               (1 => +Decl), Cont => True);
            Error_Msg_Sem
              (+Get_Protected_Type_Body (Decl), "(previous body)");
            Decl := Null_Iir;
         elsif not Get_Visible_Flag (Type_Decl) then
            --  Can this happen ?
            Error_Msg_Sem
              (+Bod, "protected type declaration not yet visible",
               Cont => True);
            Error_Msg_Sem
              (+Decl, "(location of protected type declaration)");
            Decl := Null_Iir;
         else
            Set_Protected_Type_Body (Decl, Bod);
         end if;
      else
         Error_Msg_Sem
           (+Bod, "no protected type declaration for this body");
         if Decl /= Null_Iir then
            Error_Msg_Sem (+Decl, "(found %n declared here)", +Decl);
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

      Sem_Decls.Sem_Declaration_Chain (Bod);

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

   --  Return the constraint state from CONST (the initial state) and ATYPE,
   --  as if ATYPE was a new element of a record.
   function Update_Record_Constraint (Const : Iir_Constraint; Atype : Iir)
                                     return Iir_Constraint is
   begin
      if Get_Kind (Atype) not in Iir_Kinds_Composite_Type_Definition then
         return Const;
      end if;

      case Const is
         when Fully_Constrained
           | Unconstrained =>
            if Get_Constraint_State (Atype) = Const then
               return Const;
            else
               return Partially_Constrained;
            end if;
         when Partially_Constrained =>
            return Partially_Constrained;
      end case;
   end Update_Record_Constraint;

   function Get_Array_Constraint (Def : Iir) return Iir_Constraint
   is
      El_Type : constant Iir := Get_Element_Subtype (Def);
      Index : constant Boolean :=
        Get_Kind (Def) = Iir_Kind_Array_Subtype_Definition
        and then Get_Index_Constraint_Flag (Def);
   begin
      if Get_Kind (El_Type) in Iir_Kinds_Composite_Type_Definition then
         case Get_Constraint_State (El_Type) is
            when Fully_Constrained =>
               if Index then
                  return Fully_Constrained;
               else
                  return Partially_Constrained;
               end if;
            when Partially_Constrained =>
               return Partially_Constrained;
            when Unconstrained =>
               if not Index then
                  return Unconstrained;
               else
                  return Partially_Constrained;
               end if;
         end case;
      else
         if Index then
            return Fully_Constrained;
         else
            return Unconstrained;
         end if;
      end if;
   end Get_Array_Constraint;

   function Sem_Enumeration_Type_Definition  (Def: Iir; Decl: Iir) return Iir
   is
   begin
      Set_Base_Type (Def, Def);
      Set_Type_Staticness (Def, Locally);
      Set_Signal_Type_Flag (Def, True);

      --  Makes all literal visible.
      declare
         El: Iir;
         Literal_List: Iir_List;
         Only_Characters : Boolean := True;
      begin
         Literal_List := Get_Enumeration_Literal_List (Def);
         for I in Natural loop
            El := Get_Nth_Element (Literal_List, I);
            exit when El = Null_Iir;
            Set_Expr_Staticness (El, Locally);
            Set_Name_Staticness (El, Locally);
            Set_Type (El, Def);
            Sem.Compute_Subprogram_Hash (El);
            Sem_Scopes.Add_Name (El);
            Name_Visible (El);
            Xref_Decl (El);
            if Only_Characters
              and then not Name_Table.Is_Character (Get_Identifier (El))
            then
               Only_Characters := False;
            end if;
         end loop;
         Set_Only_Characters_Flag (Def, Only_Characters);
      end;
      Set_Resolved_Flag (Def, False);

      Create_Range_Constraint_For_Enumeration_Type (Def);

      --  Identifier IEEE.Std_Logic_1164.Std_Ulogic.
      if Get_Identifier (Decl) = Std_Names.Name_Std_Ulogic
        and then
        Get_Parent (Decl) = Ieee.Std_Logic_1164.Std_Logic_1164_Pkg
      then
         Ieee.Std_Logic_1164.Std_Ulogic_Type := Def;
      end if;

      return Def;
   end Sem_Enumeration_Type_Definition;

   function Sem_Record_Type_Definition (Def: Iir) return Iir
   is
      --  Analyzed type of previous element
      Last_Type : Iir;

      El_List : constant Iir_List := Get_Elements_Declaration_List (Def);
      El: Iir;
      El_Type : Iir;
      Resolved_Flag : Boolean;
      Staticness : Iir_Staticness;
      Constraint : Iir_Constraint;
   begin
      --  LRM 10.1
      --  5. A record type declaration,
      Open_Declarative_Region;

      Resolved_Flag := True;
      Last_Type := Null_Iir;
      Staticness := Locally;
      Constraint := Fully_Constrained;
      Set_Signal_Type_Flag (Def, True);

      for I in Natural loop
         El := Get_Nth_Element (El_List, I);
         exit when El = Null_Iir;

         El_Type := Get_Subtype_Indication (El);
         if El_Type /= Null_Iir then
            --  Be careful for a declaration list (r,g,b: integer).
            El_Type := Sem_Subtype_Indication (El_Type);
            Set_Subtype_Indication (El, El_Type);
            El_Type := Get_Type_Of_Subtype_Indication (El_Type);
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

            --  LRM93 3.2.1.1
            --  The same requirement [must define a constrained array
            --  subtype] exits for the subtype indication of an
            --  element declaration, if the type of the record
            --  element is an array type.
            if Vhdl_Std < Vhdl_08
              and then not Is_Fully_Constrained_Type (El_Type)
            then
               Error_Msg_Sem
                 (+El,
                  "element declaration of unconstrained %n is not allowed",
                  +El_Type);
            end if;
            Resolved_Flag :=
              Resolved_Flag and Get_Resolved_Flag (El_Type);
            Staticness := Min (Staticness,
                               Get_Type_Staticness (El_Type));
            Constraint := Update_Record_Constraint
              (Constraint, El_Type);
         else
            Staticness := None;
         end if;
         Set_Base_Element_Declaration (El, El);
         Sem_Scopes.Add_Name (El);
         Name_Visible (El);
         Xref_Decl (El);
      end loop;
      Close_Declarative_Region;
      Set_Base_Type (Def, Def);
      Set_Resolved_Flag (Def, Resolved_Flag);
      Set_Type_Staticness (Def, Staticness);
      Set_Constraint_State (Def, Constraint);
      return Def;
   end Sem_Record_Type_Definition;

   function Sem_Unbounded_Array_Type_Definition (Def: Iir) return Iir
   is
      Index_List : constant Iir_List :=
        Get_Index_Subtype_Definition_List (Def);
      Index_Type : Iir;
   begin
      Set_Base_Type (Def, Def);

      for I in Natural loop
         Index_Type := Get_Nth_Element (Index_List, I);
         exit when Index_Type = Null_Iir;

         Index_Type := Sem_Type_Mark (Index_Type);
         Replace_Nth_Element (Index_List, I, Index_Type);

         Index_Type := Get_Type (Index_Type);
         if Get_Kind (Index_Type) not in Iir_Kinds_Discrete_Type_Definition
         then
            Error_Msg_Sem
              (+Index_Type,
               "an index type of an array must be a discrete type");
            --  FIXME: disp type Index_Type ?
         end if;
      end loop;

      Set_Index_Subtype_List (Def, Index_List);

      Sem_Array_Element (Def);
      Set_Constraint_State (Def, Get_Array_Constraint (Def));

      --  According to LRM93 7.4.1, an unconstrained array type is not static.
      Set_Type_Staticness (Def, None);

      return Def;
   end Sem_Unbounded_Array_Type_Definition;

   --  Return the subtype declaration corresponding to the base type of ATYPE
   --  (for integer and real types), or the type for enumerated types.  To say
   --  that differently, it returns the type or subtype which defines the
   --  original range.
   function Get_First_Subtype_Declaration (Atype : Iir) return Iir is
      Base_Type : constant Iir := Get_Base_Type (Atype);
      Base_Decl : constant Iir := Get_Type_Declarator (Base_Type);
   begin
      if Get_Kind (Base_Type) = Iir_Kind_Enumeration_Type_Definition then
         pragma Assert (Get_Kind (Base_Decl) = Iir_Kind_Type_Declaration);
         return Base_Decl;
      else
         return Get_Type_Declarator (Get_Subtype_Definition (Base_Decl));
      end if;
   end Get_First_Subtype_Declaration;

   function Sem_Constrained_Array_Type_Definition (Def: Iir; Decl: Iir)
                                                  return Iir
   is
      Index_Type : Iir;
      Index_Name : Iir;
      Index_List : Iir_List;
      Base_Index_List : Iir_List;
      El_Type : Iir;
      Staticness : Iir_Staticness;

      -- array_type_definition, which is the same as the subtype,
      -- but without any constraint in the indexes.
      Base_Type: Iir;
   begin
      --  LRM08 5.3.2.1  Array types
      --  A constrained array definition similarly defines both an array
      --  type and a subtype of this type.
      --  - The array type is an implicitely declared anonymous type,
      --    this type is defined by an (implicit) unbounded array
      --    definition in which the element subtype indication either
      --    denotes the base type of the subtype denoted by the element
      --    subtype indication of the constrained array definition, if
      --    that subtype is a composite type, or otherwise is the
      --    element subtype indication of the constrained array
      --    definition, and in which the type mark of each index subtype
      --    definition denotes the subtype defined by the corresponding
      --    discrete range.
      --  - The array subtype is the subtype obtained by imposition of
      --    the index constraint on the array type and if the element
      --    subtype indication of the constrained array definition
      --    denotes a fully or partially constrained composite subtype,
      --    imposition of the constraint of that subtype as an array
      --    element constraint on the array type.

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
      Set_Index_Subtype_Definition_List (Base_Type, Base_Index_List);
      Set_Index_Subtype_List (Base_Type, Base_Index_List);

      Staticness := Locally;
      Index_List := Get_Index_Constraint_List (Def);
      for I in Natural loop
         Index_Type := Get_Nth_Element (Index_List, I);
         exit when Index_Type = Null_Iir;

         Index_Name := Sem_Discrete_Range_Integer (Index_Type);
         if Index_Name /= Null_Iir then
            Index_Name := Range_To_Subtype_Indication (Index_Name);
            --  Index_Name is a subtype_indication, which can be a type_mark.
         else
            --  Avoid errors.
            Index_Name :=
              Build_Simple_Name (Natural_Subtype_Declaration, Index_Type);
            Set_Type (Index_Name, Natural_Subtype_Definition);
         end if;

         Replace_Nth_Element (Index_List, I, Index_Name);

         Index_Type := Get_Index_Type (Index_Name);
         Staticness := Min (Staticness, Get_Type_Staticness (Index_Type));

         --  Set the index subtype definition for the array base type.
         if Get_Kind (Index_Name) in Iir_Kinds_Denoting_Name then
            Index_Type := Get_Named_Entity (Index_Name);
         else
            pragma Assert
              (Get_Kind (Index_Name) in Iir_Kinds_Subtype_Definition);
            Index_Type := Get_Subtype_Type_Mark (Index_Name);
            if Index_Type = Null_Iir then
               --  From a range expression like '1 to 4' or from an attribute
               --  name.
               Index_Type := Get_First_Subtype_Declaration (Index_Name);
            else
               Index_Type := Get_Named_Entity (Index_Type);
            end if;
         end if;

         --  Create a new simple_name, as the type_mark is owned by the
         --  index constraint of the array subtype.
         Index_Name := Build_Simple_Name (Index_Type, Index_Name);
         Set_Type (Index_Name, Get_Type (Index_Type));

         Append_Element (Base_Index_List, Index_Name);
      end loop;
      Set_Index_Subtype_List (Def, Index_List);

      --  Element type.  Transfer it to the base type.
      Set_Element_Subtype_Indication
        (Base_Type, Get_Array_Element_Constraint (Def));
      Sem_Array_Element (Base_Type);
      El_Type := Get_Element_Subtype (Base_Type);
      Set_Element_Subtype (Def, El_Type);
      Set_Array_Element_Constraint (Def, Null_Iir);

      Set_Signal_Type_Flag (Def, Get_Signal_Type_Flag (Base_Type));

      --  According to LRM93 �7.4.1, an unconstrained array type
      --  is not static.
      Set_Type_Staticness (Base_Type, None);
      Set_Type_Staticness (Def, Min (Staticness,
                                     Get_Type_Staticness (El_Type)));

      Set_Type_Declarator (Base_Type, Decl);
      Set_Resolved_Flag (Base_Type, Get_Resolved_Flag (Def));
      Set_Index_Constraint_Flag (Def, True);
      Set_Constraint_State (Def, Get_Array_Constraint (Def));
      Set_Constraint_State (Base_Type, Get_Array_Constraint (Base_Type));
      Set_Base_Type (Def, Base_Type);
      Set_Subtype_Type_Mark (Def, Null_Iir);
      return Def;
   end Sem_Constrained_Array_Type_Definition;

   function Sem_Access_Type_Definition (Def: Iir) return Iir
   is
      D_Type : Iir;
   begin
      D_Type := Sem_Subtype_Indication
        (Get_Designated_Subtype_Indication (Def), True);
      Set_Designated_Subtype_Indication (Def, D_Type);

      D_Type := Get_Type_Of_Subtype_Indication (D_Type);
      if D_Type /= Null_Iir then
         case Get_Kind (D_Type) is
            when Iir_Kind_Incomplete_Type_Definition =>
               --  Append on the chain of incomplete type ref
               Set_Incomplete_Type_Ref_Chain
                 (Def, Get_Incomplete_Type_Ref_Chain (D_Type));
               Set_Incomplete_Type_Ref_Chain (D_Type, Def);
            when Iir_Kind_File_Type_Definition =>
               --  LRM 3.3
               --  The designated type must not be a file type.
               Error_Msg_Sem (+Def, "designated type must not be a file type");
            when Iir_Kind_Protected_Type_Declaration =>
               --  LRM02 3.3
               --  [..] or a protected type.
               Error_Msg_Sem
                 (+Def, "designated type must not be a protected type");
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
   end Sem_Access_Type_Definition;

   function Sem_File_Type_Definition (Def: Iir; Decl: Iir) return Iir
   is
      Type_Mark : Iir;
   begin
      Type_Mark := Sem_Type_Mark (Get_File_Type_Mark (Def));
      Set_File_Type_Mark (Def, Type_Mark);

      Type_Mark := Get_Type (Type_Mark);

      if Get_Kind (Type_Mark) = Iir_Kind_Error then
         null;
      elsif Get_Signal_Type_Flag (Type_Mark) = False then
         --  LRM 3.4
         --  The base type of this subtype must not be a file type
         --  or an access type.
         --  If the base type is a composite type, it must not
         --  contain a subelement of an access type.
         Error_Msg_Sem (+Def, "%n cannot be a file type", +Type_Mark);
      elsif Get_Kind (Type_Mark) in Iir_Kinds_Array_Type_Definition then
         --  LRM 3.4
         --  If the base type is an array type, it must be a one
         --  dimensional array type.
         if not Is_One_Dimensional_Array_Type (Type_Mark) then
            Error_Msg_Sem
              (+Def, "multi-dimensional %n cannot be a file type", +Type_Mark);
         end if;
      end if;

      Set_Base_Type (Def, Def);
      Set_Resolved_Flag (Def, False);
      Set_Text_File_Flag (Def, Is_Text_Type_Declaration (Decl));
      Set_Signal_Type_Flag (Def, False);
      Set_Type_Staticness (Def, None);
      return Def;
   end Sem_File_Type_Definition;

   function Sem_Type_Definition (Def: Iir; Decl: Iir) return Iir is
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Enumeration_Type_Definition =>
            return Sem_Enumeration_Type_Definition (Def, Decl);

         when Iir_Kind_Physical_Type_Definition =>
            return Sem_Physical_Type_Definition (Def, Decl);

         when Iir_Kind_Range_Expression =>
            return Range_Expr_To_Type_Definition (Def, Decl);

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
            return Sem_Constrained_Array_Type_Definition (Def, Decl);

         when Iir_Kind_Array_Type_Definition =>
            return Sem_Unbounded_Array_Type_Definition (Def);

         when Iir_Kind_Record_Type_Definition =>
            return Sem_Record_Type_Definition (Def);

         when Iir_Kind_Access_Type_Definition =>
            return Sem_Access_Type_Definition (Def);

         when Iir_Kind_File_Type_Definition =>
            return Sem_File_Type_Definition (Def, Decl);

         when Iir_Kind_Protected_Type_Declaration =>
            Sem_Protected_Type_Declaration (Decl);
            return Def;

         when others =>
            Error_Kind ("sem_type_definition", Def);
            return Def;
      end case;
   end Sem_Type_Definition;

   function Range_To_Subtype_Indication (A_Range: Iir) return Iir
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
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            return A_Range;
         when Iir_Kinds_Discrete_Type_Definition =>
            --  A_RANGE is already a subtype definition.
            return A_Range;
         when others =>
            Error_Kind ("range_to_subtype_indication", A_Range);
            return Null_Iir;
      end case;

      case Get_Kind (Range_Type) is
         when Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            Sub_Type := Create_Iir (Iir_Kind_Enumeration_Subtype_Definition);
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Integer_Subtype_Definition =>
            Sub_Type := Create_Iir (Iir_Kind_Integer_Subtype_Definition);
         when Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Floating_Subtype_Definition =>
            Sub_Type := Create_Iir (Iir_Kind_Floating_Subtype_Definition);
         when others =>
            raise Internal_Error;
      end case;
      Location_Copy (Sub_Type, A_Range);
      Set_Range_Constraint (Sub_Type, A_Range);
      Set_Base_Type (Sub_Type, Get_Base_Type (Range_Type));
      Set_Type_Staticness (Sub_Type, Get_Expr_Staticness (A_Range));
      Set_Signal_Type_Flag (Sub_Type, True);
      return Sub_Type;
   end Range_To_Subtype_Indication;

   -- Return TRUE iff FUNC is a resolution function for ATYPE.
   function Is_A_Resolution_Function (Func: Iir; Atype: Iir) return Boolean
   is
      Decl: Iir;
      Decl_Type : Iir;
      Ret_Type : Iir;
   begin
      -- LRM93 2.4
      --  A resolution function must be a [pure] function;
      if Get_Kind (Func) /= Iir_Kind_Function_Declaration then
         return False;
      end if;
      Decl := Get_Interface_Declaration_Chain (Func);
      -- LRM93 2.4
      --  moreover, it must have a single input parameter of class constant
      if Decl = Null_Iir or else Get_Chain (Decl) /= Null_Iir then
         return False;
      end if;
      if Get_Kind (Decl) /= Iir_Kind_Interface_Constant_Declaration then
         return False;
      end if;
      -- LRM93 2.4
      --  that is a one-dimensional, unconstrained array
      Decl_Type := Get_Type (Decl);
      if Get_Kind (Decl_Type) /= Iir_Kind_Array_Type_Definition then
         return False;
      end if;
      if not Is_One_Dimensional_Array_Type (Decl_Type) then
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
            Error_Msg_Sem (+Atype, "resolution %n must be pure", +Func);
         end if;
         return False;
      end if;
      return True;
   end Is_A_Resolution_Function;

   --  Note: this sets resolved_flag.
   procedure Sem_Resolution_Function (Name : Iir; Atype : Iir)
   is
      Func : Iir;
      Res: Iir;
      El : Iir;
      List : Iir_List;
      Has_Error : Boolean;
      Name1 : Iir;
   begin
      Sem_Name (Name);

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
            if Is_A_Resolution_Function (El, Atype) then
               if Res /= Null_Iir then
                  if not Has_Error then
                     Has_Error := True;
                     Error_Msg_Sem
                       (+Atype,
                        "can't resolve overload for resolution function",
                        Cont => True);
                     Error_Msg_Sem (+Atype, "candidate functions are:");
                     Error_Msg_Sem (+Func, " " & Disp_Subprg (Func));
                  end if;
                  Error_Msg_Sem (+El, " " & Disp_Subprg (El));
               else
                  Res := El;
               end if;
            end if;
         end loop;
         Free_Overload_List (Func);
         if Has_Error then
            return;
         end if;
         Set_Named_Entity (Name, Res);
      else
         if Is_A_Resolution_Function (Func, Atype) then
            Res := Func;
         end if;
      end if;

      if Res = Null_Iir then
         Error_Msg_Sem
           (+Atype, "no matching resolution function for %n", +Name);
      else
         Name1 := Finish_Sem_Name (Name);
         Mark_Subprogram_Used (Res);
         Set_Resolved_Flag (Atype, True);
         Set_Resolution_Indication (Atype, Name1);
      end if;
   end Sem_Resolution_Function;

   --  Analyze the constraint DEF + RESOLUTION for type TYPE_MARK.  The
   --  result is always a subtype definition.
   function Sem_Subtype_Constraint
     (Def : Iir; Type_Mark : Iir; Resolution : Iir)
     return Iir;

   --  Create a copy of elements_declaration_list of SRC and set it to DST.
   procedure Copy_Record_Elements_Declaration_List (Dst : Iir; Src : Iir)
   is
      El_List, New_El_List : Iir_List;
      El, New_El : Iir;
   begin
      New_El_List := Create_Iir_List;
      Set_Elements_Declaration_List (Dst, New_El_List);
      El_List := Get_Elements_Declaration_List (Src);
      for I in Natural loop
         El := Get_Nth_Element (El_List, I);
         exit when El = Null_Iir;

         New_El := Create_Iir (Iir_Kind_Element_Declaration);
         Location_Copy (New_El, El);
         Set_Identifier (New_El, Get_Identifier (El));
         Set_Type (New_El, Get_Type (El));
         Set_Base_Element_Declaration (New_El, El);
         Append_Element (New_El_List, New_El);
      end loop;
   end Copy_Record_Elements_Declaration_List;

   function Copy_Resolution_Indication (Subdef : Iir) return Iir
   is
      Ind : constant Iir := Get_Resolution_Indication (Subdef);
   begin
      if Is_Null (Ind)
        or else Get_Kind (Ind) = Iir_Kind_Array_Element_Resolution
      then
         --  No need to copy array_element_resolution, it is part of the
         --  element_subtype.
         return Null_Iir;
      else
         return Build_Reference_Name (Ind);
      end if;
   end Copy_Resolution_Indication;

   function Copy_Subtype_Indication (Def : Iir) return Iir
   is
      Res : Iir;
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition =>
            Res := Create_Iir (Get_Kind (Def));
            Set_Range_Constraint (Res, Get_Range_Constraint (Def));
            Set_Is_Ref (Res, True);
            Set_Resolution_Indication
              (Res, Copy_Resolution_Indication (Def));
         when Iir_Kind_Enumeration_Type_Definition =>
            Res := Create_Iir (Iir_Kind_Enumeration_Subtype_Definition);
            Set_Range_Constraint (Res, Get_Range_Constraint (Def));
            Set_Is_Ref (Res, True);

         when Iir_Kind_Access_Subtype_Definition
           | Iir_Kind_Access_Type_Definition =>
            Res := Create_Iir (Iir_Kind_Access_Subtype_Definition);
            Set_Designated_Type (Res, Get_Designated_Type (Def));

         when Iir_Kind_Array_Type_Definition =>
            Res := Create_Iir (Iir_Kind_Array_Subtype_Definition);
            Set_Type_Staticness (Res, Get_Type_Staticness (Def));
            Set_Resolved_Flag (Res, Get_Resolved_Flag (Def));
            Set_Index_Constraint_List (Res, Null_Iir_List);
            Set_Index_Subtype_List
              (Res, Get_Index_Subtype_Definition_List (Def));
            Set_Element_Subtype (Res, Get_Element_Subtype (Def));
            Set_Index_Constraint_Flag (Res, False);
            Set_Constraint_State (Res, Get_Constraint_State (Def));
         when Iir_Kind_Array_Subtype_Definition =>
            Res := Create_Iir (Iir_Kind_Array_Subtype_Definition);
            Set_Resolution_Indication (Res, Copy_Resolution_Indication (Def));
            Set_Resolved_Flag (Res, Get_Resolved_Flag (Def));
            Set_Index_Subtype_List (Res, Get_Index_Subtype_List (Def));
            Set_Element_Subtype (Res, Get_Element_Subtype (Def));
            Set_Index_Constraint_Flag
              (Res, Get_Index_Constraint_Flag (Def));
            Set_Constraint_State (Res, Get_Constraint_State (Def));

         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            Res := Create_Iir (Iir_Kind_Record_Subtype_Definition);
            Set_Type_Staticness (Res, Get_Type_Staticness (Def));
            if Get_Kind (Def) = Iir_Kind_Record_Subtype_Definition then
               Set_Resolution_Indication
                 (Res, Copy_Resolution_Indication (Def));
            end if;
            Set_Resolved_Flag (Res, Get_Resolved_Flag (Def));
            Set_Constraint_State (Res, Get_Constraint_State (Def));
            Copy_Record_Elements_Declaration_List (Res, Def);
         when others =>
            --  FIXME: todo (protected type ?)
            Error_Kind ("copy_subtype_indication", Def);
      end case;
      Location_Copy (Res, Def);
      Set_Base_Type (Res, Get_Base_Type (Def));
      Set_Type_Staticness (Res, Get_Type_Staticness (Def));
      Set_Signal_Type_Flag (Res, Get_Signal_Type_Flag (Def));
      return Res;
   end Copy_Subtype_Indication;

   --  DEF is an incomplete subtype_indication or array_constraint,
   --  TYPE_MARK is the base type of the subtype_indication.
   function Sem_Array_Constraint
     (Def : Iir; Type_Mark : Iir; Resolution : Iir) return Iir
   is
      El_Type : constant Iir := Get_Element_Subtype (Type_Mark);
      Res : Iir;
      Type_Index, Subtype_Index: Iir;
      Base_Type : Iir;
      El_Def : Iir;
      Staticness : Iir_Staticness;
      Error_Seen : Boolean;
      Type_Index_List : Iir_List;
      Subtype_Index_List : Iir_List;
      Resolv_Func : Iir := Null_Iir;
      Resolv_El : Iir := Null_Iir;
      Resolv_Ind : Iir;
   begin
      if Resolution /= Null_Iir then
         --  A resolution indication is present.
         case Get_Kind (Resolution) is
            when Iir_Kinds_Denoting_Name =>
               Resolv_Func := Resolution;
            when Iir_Kind_Array_Element_Resolution =>
               Resolv_El := Get_Resolution_Indication (Resolution);
            when Iir_Kind_Record_Resolution =>
               Error_Msg_Sem
                 (+Resolution,
                  "record resolution not allowed for array subtype");
            when others =>
               Error_Kind ("sem_array_constraint(resolution)", Resolution);
         end case;
      end if;

      if Def = Null_Iir then
         --  There is no element_constraint.
         pragma Assert (Resolution /= Null_Iir);
         Res := Copy_Subtype_Indication (Type_Mark);
         El_Def := Null_Iir;
      else
         case Get_Kind (Def) is
            when Iir_Kind_Subtype_Definition =>
               -- This is the case of "subtype new_array is [func] old_array".
               -- def must be a constrained array.
               if Get_Range_Constraint (Def) /= Null_Iir then
                  Error_Msg_Sem
                    (+Def, "cannot use a range constraint for array types");
                  return Copy_Subtype_Indication (Type_Mark);
               end if;

               --  LRM08 6.3 Subtype declarations
               --
               --  If the subtype indication does not include a constraint, the
               --  subtype is the same as that denoted by the type mark.
               if Resolution = Null_Iir then
                  --  FIXME: is it reachable ?
                  Free_Name (Def);
                  return Type_Mark;
               end if;

               Res := Copy_Subtype_Indication (Type_Mark);
               Location_Copy (Res, Def);
               Free_Name (Def);

               --  No element constraint.
               El_Def := Null_Iir;

            when Iir_Kind_Array_Subtype_Definition =>
               -- Case of a constraint for an array.
               -- Check each index constraint against array type.

               Base_Type := Get_Base_Type (Type_Mark);
               Set_Base_Type (Def, Base_Type);
               El_Def := Get_Array_Element_Constraint (Def);

               Staticness := Get_Type_Staticness (El_Type);
               Error_Seen := False;
               Type_Index_List :=
                 Get_Index_Subtype_Definition_List (Base_Type);
               Subtype_Index_List := Get_Index_Constraint_List (Def);

               --  LRM08 5.3.2.2
               --  If an array constraint of the first form (including an index
               --  constraint) applies to a type or subtype, then the type or
               --  subtype shall be an unconstrained or partially constrained
               --  array type with no index constraint applying to the index
               --  subtypes, or an access type whose designated type is such
               --  a type.
               if Get_Kind (Type_Mark) = Iir_Kind_Array_Subtype_Definition
                 and then Get_Index_Constraint_Flag (Type_Mark)
               then
                  Error_Msg_Sem
                    (+Def, "constrained array cannot be re-constrained");
               end if;
               if Subtype_Index_List = Null_Iir_List then
                  --  Array is not constrained.
                  Set_Index_Constraint_Flag (Def, False);
                  Set_Index_Subtype_List (Def, Type_Index_List);
               else
                  for I in Natural loop
                     Type_Index := Get_Nth_Element (Type_Index_List, I);
                     Subtype_Index := Get_Nth_Element (Subtype_Index_List, I);
                     exit when Type_Index = Null_Iir
                       and Subtype_Index = Null_Iir;

                     if Type_Index = Null_Iir then
                        Error_Msg_Sem
                          (+Subtype_Index,
                           "subtype has more indexes than %n defined at %l",
                           (+Type_Mark, +Type_Mark));
                        --  Forget extra indexes.
                        Set_Nbr_Elements (Subtype_Index_List, I);
                        exit;
                     end if;
                     if Subtype_Index = Null_Iir then
                        if not Error_Seen then
                           Error_Msg_Sem
                             (+Def,
                              "subtype has less indexes than %n defined at %l",
                              (+Type_Mark, +Type_Mark));
                           Error_Seen := True;
                        end if;
                     else
                        Subtype_Index := Sem_Discrete_Range_Expression
                          (Subtype_Index, Get_Index_Type (Type_Index), True);
                        if Subtype_Index /= Null_Iir then
                           Subtype_Index :=
                             Range_To_Subtype_Indication (Subtype_Index);
                           Staticness := Min
                             (Staticness,
                              Get_Type_Staticness
                                (Get_Type_Of_Subtype_Indication
                                   (Subtype_Index)));
                        end if;
                     end if;
                     if Subtype_Index = Null_Iir then
                        --  Create a fake subtype from type_index.
                        --  FIXME: It is too fake.
                        Subtype_Index := Type_Index;
                        Staticness := None;
                     end if;
                     if Error_Seen then
                        Append_Element (Subtype_Index_List, Subtype_Index);
                     else
                        Replace_Nth_Element
                          (Subtype_Index_List, I, Subtype_Index);
                     end if;
                  end loop;
                  Set_Index_Subtype_List (Def, Subtype_Index_List);
                  Set_Index_Constraint_Flag (Def, True);
               end if;
               Set_Type_Staticness (Def, Staticness);
               Set_Signal_Type_Flag (Def, Get_Signal_Type_Flag (Type_Mark));
               Res := Def;

            when others =>
               --  LRM93 3.2.1.1 / LRM08 5.3.2.2
               --  Index Constraints and Discrete Ranges
               --
               --  If an index constraint appears after a type mark [...]
               --  The type mark must denote either an unconstrained array
               --  type, or an access type whose designated type is such
               --  an array type.
               Error_Msg_Sem
                 (+Def,
                  "only unconstrained array type may be contrained by index",
                  Cont => True);
               Error_Msg_Sem
                 (+Type_Mark, " (type mark is %n)", +Type_Mark);
               return Type_Mark;
         end case;
      end if;

      --  Element subtype.
      if Resolv_El /= Null_Iir or else El_Def /= Null_Iir then
         El_Def := Sem_Subtype_Constraint (El_Def, El_Type, Resolv_El);
         if Resolv_El /= Null_Iir then
            --  Save EL_DEF so that it is owned.
            Set_Element_Subtype_Indication (Resolution, El_Def);
            Set_Resolution_Indication (Resolution, Null_Iir);
         end if;
      end if;
      if El_Def = Null_Iir then
         El_Def := Get_Element_Subtype (Type_Mark);
      end if;
      Set_Element_Subtype (Res, El_Def);

      Set_Constraint_State (Res, Get_Array_Constraint (Res));

      if Resolv_Func /= Null_Iir then
         Sem_Resolution_Function (Resolv_Func, Res);
      elsif Resolv_El /= Null_Iir then
         Set_Resolution_Indication (Res, Resolution);
         --  FIXME: may a resolution indication for a record be incomplete ?
         Set_Resolved_Flag (Res, Get_Resolved_Flag (El_Def));
      elsif Get_Kind (Type_Mark) = Iir_Kind_Array_Subtype_Definition then
         Resolv_Ind := Get_Resolution_Indication (Type_Mark);
         if Resolv_Ind /= Null_Iir then
            case Get_Kind (Resolv_Ind) is
               when Iir_Kinds_Denoting_Name =>
                  Error_Kind ("sem_array_constraint(resolution)", Resolv_Ind);
               when Iir_Kind_Array_Element_Resolution =>
                  --  Already applied to the element.
                  Resolv_Ind := Null_Iir;
               when others =>
                  Error_Kind ("sem_array_constraint(resolution2)", Resolv_Ind);
            end case;
            Set_Resolution_Indication (Res, Resolv_Ind);
         end if;
         Set_Resolved_Flag (Res, Get_Resolved_Flag (Type_Mark));
      else
         pragma Assert (Get_Kind (Type_Mark) = Iir_Kind_Array_Type_Definition);
         Set_Resolved_Flag (Res, Get_Resolved_Flag (Type_Mark));
      end if;

      return Res;
   end Sem_Array_Constraint;

   function Reparse_As_Record_Element_Constraint (Name : Iir) return Iir
   is
      Prefix : Iir;
      Parent : Iir;
      El : Iir;
   begin
      if Get_Kind (Name) /= Iir_Kind_Parenthesis_Name then
         Error_Msg_Sem (+Name, "record element constraint expected");
         return Null_Iir;
      else
         Prefix := Get_Prefix (Name);
         Parent := Name;
         while Get_Kind (Prefix) = Iir_Kind_Parenthesis_Name loop
            Parent := Prefix;
            Prefix := Get_Prefix (Prefix);
         end loop;
         if Get_Kind (Prefix) /= Iir_Kind_Simple_Name then
            Error_Msg_Sem
              (+Prefix, "record element name must be a simple name");
            return Null_Iir;
         else
            El := Create_Iir (Iir_Kind_Record_Element_Constraint);
            Location_Copy (El, Prefix);
            Set_Identifier (El, Get_Identifier (Prefix));
            Set_Type (El, Name);
            Set_Prefix (Parent, Null_Iir);
            Free_Name (Prefix);
            return El;
         end if;
      end if;
   end Reparse_As_Record_Element_Constraint;

   function Reparse_As_Record_Constraint (Def : Iir) return Iir
   is
      Res : Iir;
      Chain : Iir;
      El_List : Iir_List;
      El : Iir;
   begin
      if Get_Prefix (Def) /= Null_Iir then
         raise Internal_Error;
      end if;
      Res := Create_Iir (Iir_Kind_Record_Subtype_Definition);
      Location_Copy (Res, Def);
      El_List := Create_Iir_List;
      Set_Elements_Declaration_List (Res, El_List);
      Chain := Get_Association_Chain (Def);
      while Chain /= Null_Iir loop
         if Get_Kind (Chain) /= Iir_Kind_Association_Element_By_Expression
           or else Get_Formal (Chain) /= Null_Iir
         then
            Error_Msg_Sem (+Chain, "badly formed record constraint");
         else
            El := Reparse_As_Record_Element_Constraint (Get_Actual (Chain));
            if El /= Null_Iir then
               Append_Element (El_List, El);
            end if;
         end if;
         Chain := Get_Chain (Chain);
      end loop;
      return Res;
   end Reparse_As_Record_Constraint;

   function Reparse_As_Array_Constraint (Def : Iir; Def_Type : Iir) return Iir
   is
      Parent : Iir;
      Name : Iir;
      Prefix : Iir;
      Res : Iir;
      Chain : Iir;
      El_List : Iir_List;
      Def_El_Type : Iir;
   begin
      Name := Def;
      Prefix := Get_Prefix (Name);
      Parent := Null_Iir;
      while Prefix /= Null_Iir
        and then Get_Kind (Prefix) = Iir_Kind_Parenthesis_Name
      loop
         Parent := Name;
         Name := Prefix;
         Prefix := Get_Prefix (Name);
      end loop;
      --  Detach prefix.
      if Parent /= Null_Iir then
         Set_Prefix (Parent, Null_Iir);
      end if;
      Res := Create_Iir (Iir_Kind_Array_Subtype_Definition);
      Location_Copy (Res, Name);
      Chain := Get_Association_Chain (Name);
      if Get_Kind (Chain) = Iir_Kind_Association_Element_Open then
         if Get_Chain (Chain) /= Null_Iir then
            Error_Msg_Sem (+Chain, "'open' must be alone");
         end if;
      else
         El_List := Create_Iir_List;
         Set_Index_Constraint_List (Res, El_List);
         while Chain /= Null_Iir loop
            if Get_Kind (Chain) /= Iir_Kind_Association_Element_By_Expression
              or else Get_Formal (Chain) /= Null_Iir
            then
               Error_Msg_Sem (+Chain, "bad form of array constraint");
            else
               Append_Element (El_List, Get_Actual (Chain));
            end if;
            Chain := Get_Chain (Chain);
         end loop;
      end if;

      Def_El_Type := Get_Element_Subtype (Def_Type);
      if Parent /= Null_Iir then
         case Get_Kind (Def_El_Type) is
            when Iir_Kinds_Array_Type_Definition =>
               Set_Element_Subtype_Indication
                 (Res, Reparse_As_Array_Constraint (Def, Def_El_Type));
            when others =>
               Error_Kind ("reparse_as_array_constraint", Def_El_Type);
         end case;
      end if;
      return Res;
   end Reparse_As_Array_Constraint;

   function Sem_Record_Constraint
     (Def : Iir; Type_Mark : Iir; Resolution : Iir)
     return Iir
   is
      Res : Iir;
      El_List, Tm_El_List : Iir_List;
      El : Iir;
      Tm_El : Iir;
      Tm_El_Type : Iir;
      El_Type : Iir;
      Res_List : Iir_List;

      Index_List : Iir_List;
      Index_El : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Record_Subtype_Definition);
      Location_Copy (Res, Def);
      Set_Base_Type (Res, Get_Base_Type (Type_Mark));
      Set_Type_Staticness (Res, Get_Type_Staticness (Type_Mark));
      if Get_Kind (Type_Mark) = Iir_Kind_Record_Subtype_Definition then
         Set_Resolution_Indication
           (Res, Get_Resolution_Indication (Type_Mark));
      end if;

      case Get_Kind (Def) is
         when Iir_Kind_Subtype_Definition =>
            Free_Name (Def);
            Set_Signal_Type_Flag (Res, Get_Signal_Type_Flag (Type_Mark));
            Set_Constraint_State (Res, Get_Constraint_State (Type_Mark));
            El_List := Null_Iir_List;

         when Iir_Kind_Array_Subtype_Definition =>
            --  Record constraints are parsed as array constraints.
            pragma Assert (Get_Kind (Def) = Iir_Kind_Array_Subtype_Definition);
            Index_List := Get_Index_Constraint_List (Def);
            El_List := Create_Iir_List;
            Set_Elements_Declaration_List (Res, El_List);
            for I in Natural loop
               Index_El := Get_Nth_Element (Index_List, I);
               exit when Index_El = Null_Iir;
               El := Reparse_As_Record_Element_Constraint (Index_El);
               if El /= Null_Iir then
                  Append_Element (El_List, El);
               end if;
            end loop;

         when Iir_Kind_Record_Subtype_Definition =>
            El_List := Get_Elements_Declaration_List (Def);
            Set_Elements_Declaration_List (Res, El_List);

         when others =>
            Error_Kind ("sem_record_constraint", Def);
      end case;

      Res_List := Null_Iir_List;
      if Resolution /= Null_Iir then
         case Get_Kind (Resolution) is
            when Iir_Kinds_Denoting_Name =>
               null;
            when Iir_Kind_Record_Subtype_Definition =>
               Res_List := Get_Elements_Declaration_List (Resolution);
            when Iir_Kind_Array_Subtype_Definition =>
               Error_Msg_Sem
                 (+Resolution,
                  "resolution indication must be an array element resolution");
            when others =>
               Error_Kind ("sem_record_constraint(resolution)", Resolution);
         end case;
      end if;

      Tm_El_List := Get_Elements_Declaration_List (Type_Mark);
      if El_List /= Null_Iir_List or Res_List /= Null_Iir_List then
         declare
            Nbr_Els : constant Natural := Get_Nbr_Elements (Tm_El_List);
            Els : Iir_Array (0 .. Nbr_Els - 1) := (others => Null_Iir);
            Res_Els : Iir_Array (0 .. Nbr_Els - 1) := (others => Null_Iir);
            Pos : Natural;
            Constraint : Iir_Constraint;
         begin
            --  Fill ELS.
            if El_List /= Null_Iir_List then
               for I in Natural loop
                  El := Get_Nth_Element (El_List, I);
                  exit when El = Null_Iir;
                  Tm_El := Find_Name_In_List (Tm_El_List, Get_Identifier (El));
                  if Tm_El = Null_Iir then
                     Error_Msg_Sem (+El, "%n has no %n", (+Type_Mark, +El));
                  else
                     Set_Element_Declaration (El, Tm_El);
                     Pos := Natural (Get_Element_Position (Tm_El));
                     if Els (Pos) /= Null_Iir then
                        Error_Msg_Sem
                          (+El, "%n was already constrained",
                           (1 => +El), Cont => True);
                        Error_Msg_Sem
                          (+Els (Pos), " (location of previous constrained)");
                     else
                        Els (Pos) := El;
                        Set_Parent (El, Res);
                     end if;
                     El_Type := Get_Type (El);
                     Tm_El_Type := Get_Type (Tm_El);
                     if Get_Kind (El_Type) = Iir_Kind_Parenthesis_Name then
                        case Get_Kind (Tm_El_Type) is
                           when Iir_Kinds_Array_Type_Definition =>
                              El_Type := Reparse_As_Array_Constraint
                                (El_Type, Tm_El_Type);
                           when Iir_Kind_Record_Type_Definition
                             | Iir_Kind_Record_Subtype_Definition =>
                              El_Type :=
                                Reparse_As_Record_Constraint (El_Type);
                           when others =>
                              Error_Msg_Sem
                                (+El_Type,
                                 "only composite types may be constrained");
                        end case;
                     end if;
                     Set_Type (El, El_Type);
                  end if;
               end loop;
               Destroy_Iir_List (El_List);
            end if;

            --  Fill Res_Els.
            if Res_List /= Null_Iir_List then
               for I in Natural loop
                  El := Get_Nth_Element (Res_List, I);
                  exit when El = Null_Iir;
                  Tm_El := Find_Name_In_List (Tm_El_List, Get_Identifier (El));
                  if Tm_El = Null_Iir then
                     Error_Msg_Sem (+El, "%n has no %n", (+Type_Mark, +El));
                  else
                     Pos := Natural (Get_Element_Position (Tm_El));
                     if Res_Els (Pos) /= Null_Iir then
                        Error_Msg_Sem (+El, "%n was already resolved",
                                       (1 => +El), Cont => True);
                        Error_Msg_Sem
                          (+Els (Pos), " (location of previous constrained)");
                     else
                        Res_Els (Pos) := Get_Element_Declaration (El);
                     end if;
                  end if;
                  --Free_Iir (El);
               end loop;
               Destroy_Iir_List (Res_List);
            end if;

            --  Build elements list.
            El_List := Create_Iir_List;
            Set_Elements_Declaration_List (Res, El_List);
            Constraint := Fully_Constrained;
            for I in Els'Range loop
               Tm_El := Get_Nth_Element (Tm_El_List, I);
               if Els (I) = Null_Iir and Res_Els (I) = Null_Iir then
                  El := Tm_El;
               else
                  if Els (I) = Null_Iir then
                     El := Create_Iir (Iir_Kind_Record_Element_Constraint);
                     Location_Copy (El, Tm_El);
                     Set_Element_Declaration (El, Tm_El);
                     Set_Element_Position (El, Get_Element_Position (Tm_El));
                     El_Type := Null_Iir;
                  else
                     El := Els (I);
                     El_Type := Get_Type (El);
                  end if;
                  El_Type := Sem_Subtype_Constraint (El_Type,
                                                     Get_Type (Tm_El),
                                                     Res_Els (I));
                  Set_Type (El, El_Type);
               end if;
               Append_Element (El_List, El);
               Constraint := Update_Record_Constraint
                 (Constraint, Get_Type (El));
            end loop;
            Set_Constraint_State (Res, Constraint);
         end;
      else
         Copy_Record_Elements_Declaration_List (Res, Type_Mark);
         Set_Constraint_State (Res, Get_Constraint_State (Type_Mark));
      end if;

      Set_Signal_Type_Flag (Res, Get_Signal_Type_Flag (Type_Mark));

      if Resolution /= Null_Iir
        and then Get_Kind (Resolution) in Iir_Kinds_Denoting_Name
      then
         Sem_Resolution_Function (Resolution, Res);
      end if;

      return Res;
   end Sem_Record_Constraint;

   --  Return a scalar subtype definition (even in case of error).
   function Sem_Range_Constraint
     (Def : Iir; Type_Mark : Iir; Resolution : Iir)
     return Iir
   is
      Res : Iir;
      A_Range : Iir;
      Tolerance : Iir;
   begin
      if Def = Null_Iir then
         Res := Copy_Subtype_Indication (Type_Mark);
      elsif Get_Kind (Def) /= Iir_Kind_Subtype_Definition then
         --  FIXME: find the correct sentence from LRM
         --  GHDL: subtype_definition may also be used just to add
         --    a resolution function.
         Error_Msg_Sem
           (+Def, "only scalar types may be constrained by range",
            Cont => True);
         Error_Msg_Sem
           (+Type_Mark, " (type mark is %n)", +Type_Mark);
         Res := Copy_Subtype_Indication (Type_Mark);
      else
         Tolerance := Get_Tolerance (Def);

         if Get_Range_Constraint (Def) = Null_Iir
           and then Resolution = Null_Iir
           and then Tolerance = Null_Iir
         then
            --  This defines an alias, and must have been handled just
            --  before the case statment.
            raise Internal_Error;
         end if;

         -- There are limits.  Create a new subtype.
         if Get_Kind (Type_Mark) = Iir_Kind_Enumeration_Type_Definition then
            Res := Create_Iir (Iir_Kind_Enumeration_Subtype_Definition);
         else
            Res := Create_Iir (Get_Kind (Type_Mark));
         end if;
         Location_Copy (Res, Def);
         Set_Base_Type (Res, Get_Base_Type (Type_Mark));
         Set_Resolution_Indication (Res, Get_Resolution_Indication (Def));
         A_Range := Get_Range_Constraint (Def);
         if A_Range = Null_Iir then
            A_Range := Get_Range_Constraint (Type_Mark);
            Set_Is_Ref (Res, True);
         else
            A_Range := Sem_Range_Expression (A_Range, Type_Mark, True);
            if A_Range = Null_Iir then
               --  Avoid error propagation.
               A_Range := Get_Range_Constraint (Type_Mark);
               Set_Is_Ref (Res, True);
            end if;
         end if;
         Set_Range_Constraint (Res, A_Range);
         Set_Type_Staticness (Res, Get_Expr_Staticness (A_Range));
         Free_Name (Def);
         Set_Signal_Type_Flag (Res, Get_Signal_Type_Flag (Type_Mark));
         if Tolerance /= Null_Iir then
            --  LRM93 4.2 Subtype declarations
            --  It is an error in this case the subtype is not a nature
            --  type
            --
            --  FIXME: should be moved into sem_subtype_indication
            if Get_Kind (Res) /= Iir_Kind_Floating_Subtype_Definition then
               Error_Msg_Sem
                 (+Tolerance, "tolerance allowed only for floating subtype");
            else
               --  LRM93 4.2 Subtype declarations
               --  If the subtype indication includes a tolerance aspect, then
               --  the string expression must be a static expression
               Tolerance := Sem_Expression (Tolerance, String_Type_Definition);
               if Tolerance /= Null_Iir
                 and then Get_Expr_Staticness (Tolerance) /= Locally
               then
                  Error_Msg_Sem
                    (+Tolerance, "tolerance must be a static string");
               end if;
               Set_Tolerance (Res, Tolerance);
            end if;
         end if;
      end if;

      if Resolution /= Null_Iir then
         --  LRM08 6.3  Subtype declarations.
         if Get_Kind (Resolution) not in Iir_Kinds_Denoting_Name then
            Error_Msg_Sem
              (+Resolution, "resolution indication must be a function name");
         else
            Sem_Resolution_Function (Resolution, Res);
         end if;
      end if;
      return Res;
   end Sem_Range_Constraint;

   function Sem_Subtype_Constraint
     (Def : Iir; Type_Mark : Iir; Resolution : Iir)
     return Iir is
   begin
      case Get_Kind (Type_Mark) is
         when Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Array_Type_Definition =>
            return Sem_Array_Constraint (Def, Type_Mark, Resolution);
         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition=>
            return Sem_Range_Constraint (Def, Type_Mark, Resolution);
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            return Sem_Record_Constraint (Def, Type_Mark, Resolution);
         when Iir_Kind_Access_Type_Definition
           | Iir_Kind_Access_Subtype_Definition =>
            --  LRM93 4.2
            --  A subtype indication denoting an access type [or a file type]
            --  may not contain a resolution function.
            if Resolution /= Null_Iir then
               Error_Msg_Sem
                 (+Def, "resolution function not allowed for an access type");
            end if;

            case Get_Kind (Def) is
               when Iir_Kind_Subtype_Definition =>
                  Free_Name (Def);
                  return Copy_Subtype_Indication (Type_Mark);
               when Iir_Kind_Array_Subtype_Definition =>
                  --  LRM93 3.3
                  --  The only form of constraint that is allowed after a name
                  --  of an access type in a subtype indication is an index
                  --  constraint.
                  declare
                     Base_Type : constant Iir :=
                       Get_Designated_Type (Type_Mark);
                     Sub_Type : Iir;
                     Res : Iir;
                  begin
                     Sub_Type := Sem_Array_Constraint
                       (Def, Base_Type, Null_Iir);
                     Res := Create_Iir (Iir_Kind_Access_Subtype_Definition);
                     Location_Copy (Res, Def);
                     Set_Base_Type (Res, Type_Mark);
                     Set_Designated_Subtype_Indication (Res, Sub_Type);
                     Set_Signal_Type_Flag (Res, False);

                     --  The type_mark is a type_mark of the access subtype,
                     --  not of the array subtype.
                     Set_Subtype_Type_Mark
                       (Res, Get_Subtype_Type_Mark (Sub_Type));
                     Set_Subtype_Type_Mark (Sub_Type, Null_Iir);
                     return Res;
                  end;
               when others =>
                  raise Internal_Error;
            end case;

         when Iir_Kind_File_Type_Definition =>
            --  LRM08 6.3 Subtype declarations
            --  A subtype indication denoting a subtype of [...] a file
            --  type [...] shall not contain a constraint.
            if Get_Kind (Def) /= Iir_Kind_Subtype_Definition
              or else Get_Range_Constraint (Def) /= Null_Iir
            then
               Error_Msg_Sem (+Def, "file types can't be constrained");
               return Type_Mark;
            end if;

            --  LRM93 4.2
            --  A subtype indication denoting [an access type or] a file type
            --  may not contain a resolution function.
            if Resolution /= Null_Iir then
               Error_Msg_Sem
                 (+Def, "resolution function not allowed for file types");
               return Type_Mark;
            end if;
            Free_Name (Def);
            return Type_Mark;

         when Iir_Kind_Protected_Type_Declaration =>
            --  LRM08 6.3 Subtype declarations
            --  A subtype indication denoting a subtype of [...] a protected
            --  type [...] shall not contain a constraint.
            if Get_Kind (Def) /= Iir_Kind_Subtype_Definition
              or else Get_Range_Constraint (Def) /= Null_Iir
            then
               Error_Msg_Sem (+Def, "protected types can't be constrained");
               return Type_Mark;
            end if;

            --  LRM08 6.3 Subtype declarations
            --  A subtype indication denoting [...] a protected type shall
            --  not contain a resolution function.
            if Resolution /= Null_Iir then
               Error_Msg_Sem
                 (+Def, "resolution function not allowed for file types");
               return Type_Mark;
            end if;
            Free_Name (Def);
            return Type_Mark;

         when others =>
            Error_Kind ("sem_subtype_constraint", Type_Mark);
            return Type_Mark;
      end case;
   end Sem_Subtype_Constraint;

   function Sem_Subtype_Indication (Def: Iir; Incomplete : Boolean := False)
                                   return Iir
   is
      Type_Mark_Name : Iir;
      Type_Mark: Iir;
      Res : Iir;
   begin
      --  LRM08 6.3 Subtype declarations
      --
      --  If the subtype indication does not include a constraint, the subtype
      --  is the same as that denoted by the type mark.
      if Get_Kind (Def) in Iir_Kinds_Denoting_Name then
         Type_Mark := Sem_Type_Mark (Def, Incomplete);
         return Type_Mark;
      end if;

      --  Analyze the type mark.
      Type_Mark_Name := Get_Subtype_Type_Mark (Def);
      Type_Mark_Name := Sem_Type_Mark (Type_Mark_Name);
      Set_Subtype_Type_Mark (Def, Type_Mark_Name);
      Type_Mark := Get_Type (Type_Mark_Name);
      --  FIXME: incomplete type ?
      if Get_Kind (Type_Mark) = Iir_Kind_Error then
         --  FIXME: handle inversion such as "subtype BASETYPE RESOLV", which
         --  should emit "resolution function must precede type name".

         --  Discard the subtype definition and only keep the type mark.
         return Type_Mark_Name;
      end if;

      Res := Sem_Subtype_Constraint
        (Def, Type_Mark, Get_Resolution_Indication (Def));
      Set_Subtype_Type_Mark (Res, Type_Mark_Name);
      return Res;
   end Sem_Subtype_Indication;

   function Sem_Subnature_Indication (Def: Iir) return Iir
   is
      Nature_Mark: Iir;
      Res : Iir;
   begin
      -- LRM 4.8 Nature declatation
      --
      -- If the subnature indication does not include a constraint, the
      -- subnature is the same as that denoted by the type mark.
      case Get_Kind (Def) is
         when Iir_Kind_Scalar_Nature_Definition =>
            --  Used for reference declared by a nature
            return Def;
         when Iir_Kinds_Denoting_Name =>
            Nature_Mark := Sem_Denoting_Name (Def);
            Res := Get_Named_Entity (Nature_Mark);
            if Get_Kind (Res) /= Iir_Kind_Scalar_Nature_Definition then
               Error_Class_Match (Nature_Mark, "nature");
               raise Program_Error; --  TODO
            else
               return Nature_Mark;
            end if;
         when others =>
            raise Program_Error; --  TODO
      end case;
   end Sem_Subnature_Indication;

end Sem_Types;
