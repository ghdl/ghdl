--  Evaluation of static expressions.
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
with Ada.Unchecked_Deallocation;
with Errorout; use Errorout;
with Name_Table; use Name_Table;
with Str_Table;
with Iirs_Utils; use Iirs_Utils;
with Std_Package; use Std_Package;
with Flags; use Flags;
with Std_Names;
with Ada.Characters.Handling;

package body Evaluation is
   function Eval_Enum_To_String (Lit : Iir; Orig : Iir) return Iir;
   function Eval_Integer_Image (Val : Iir_Int64; Orig : Iir) return Iir;

   function Get_Physical_Value (Expr : Iir) return Iir_Int64
   is
      pragma Unsuppress (Overflow_Check);
      Kind : constant Iir_Kind := Get_Kind (Expr);
      Unit : Iir;
   begin
      case Kind is
         when Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal =>
            --  Extract Unit.
            Unit := Get_Physical_Unit_Value
              (Get_Named_Entity (Get_Unit_Name (Expr)));
            case Kind is
               when Iir_Kind_Physical_Int_Literal =>
                  return Get_Value (Expr) * Get_Value (Unit);
               when Iir_Kind_Physical_Fp_Literal =>
                  return Iir_Int64
                    (Get_Fp_Value (Expr) * Iir_Fp64 (Get_Value (Unit)));
               when others =>
                  raise Program_Error;
            end case;
         when Iir_Kind_Unit_Declaration =>
            return Get_Value (Get_Physical_Unit_Value (Expr));
         when others =>
            Error_Kind ("get_physical_value", Expr);
      end case;
   end Get_Physical_Value;

   function Build_Integer (Val : Iir_Int64; Origin : Iir)
     return Iir_Integer_Literal
   is
      Res : Iir_Integer_Literal;
   begin
      Res := Create_Iir (Iir_Kind_Integer_Literal);
      Location_Copy (Res, Origin);
      Set_Value (Res, Val);
      Set_Type (Res, Get_Type (Origin));
      Set_Literal_Origin (Res, Origin);
      Set_Expr_Staticness (Res, Locally);
      return Res;
   end Build_Integer;

   function Build_Floating (Val : Iir_Fp64; Origin : Iir)
     return Iir_Floating_Point_Literal
   is
      Res : Iir_Floating_Point_Literal;
   begin
      Res := Create_Iir (Iir_Kind_Floating_Point_Literal);
      Location_Copy (Res, Origin);
      Set_Fp_Value (Res, Val);
      Set_Type (Res, Get_Type (Origin));
      Set_Literal_Origin (Res, Origin);
      Set_Expr_Staticness (Res, Locally);
      return Res;
   end Build_Floating;

   function Build_Enumeration_Constant (Val : Iir_Index32; Origin : Iir)
     return Iir_Enumeration_Literal
   is
      Enum_Type : constant Iir := Get_Base_Type (Get_Type (Origin));
      Enum_List : constant Iir_List :=
        Get_Enumeration_Literal_List (Enum_Type);
      Lit : constant Iir_Enumeration_Literal :=
        Get_Nth_Element (Enum_List, Integer (Val));
      Res : Iir_Enumeration_Literal;
   begin
      Res := Copy_Enumeration_Literal (Lit);
      Location_Copy (Res, Origin);
      Set_Literal_Origin (Res, Origin);
      return Res;
   end Build_Enumeration_Constant;

   function Build_Physical (Val : Iir_Int64; Origin : Iir)
     return Iir_Physical_Int_Literal
   is
      Res : Iir_Physical_Int_Literal;
      Unit_Name : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Physical_Int_Literal);
      Location_Copy (Res, Origin);
      Unit_Name := Get_Primary_Unit_Name (Get_Base_Type (Get_Type (Origin)));
      Set_Unit_Name (Res, Unit_Name);
      Set_Value (Res, Val);
      Set_Type (Res, Get_Type (Origin));
      Set_Literal_Origin (Res, Origin);
      Set_Expr_Staticness (Res, Locally);
      return Res;
   end Build_Physical;

   function Build_Discrete (Val : Iir_Int64; Origin : Iir) return Iir is
   begin
      case Get_Kind (Get_Type (Origin)) is
         when Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            return Build_Enumeration_Constant (Iir_Index32 (Val), Origin);
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Integer_Subtype_Definition =>
            return Build_Integer (Val, Origin);
         when others =>
            Error_Kind ("build_discrete", Get_Type (Origin));
      end case;
   end Build_Discrete;

   function Build_String (Val : String8_Id; Len : Nat32; Origin : Iir)
                         return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_String_Literal8);
      Location_Copy (Res, Origin);
      Set_String8_Id (Res, Val);
      Set_String_Length (Res, Len);
      Set_Type (Res, Get_Type (Origin));
      Set_Literal_Origin (Res, Origin);
      Set_Expr_Staticness (Res, Locally);
      return Res;
   end Build_String;

   function Build_Simple_Aggregate
     (El_List : Iir_List; Origin : Iir; Stype : Iir)
     return Iir_Simple_Aggregate
   is
      Res : Iir_Simple_Aggregate;
   begin
      Res := Create_Iir (Iir_Kind_Simple_Aggregate);
      Location_Copy (Res, Origin);
      Set_Simple_Aggregate_List (Res, El_List);
      Set_Type (Res, Stype);
      Set_Literal_Origin (Res, Origin);
      Set_Expr_Staticness (Res, Locally);
      Set_Literal_Subtype (Res, Stype);
      return Res;
   end Build_Simple_Aggregate;

   function Build_Overflow (Origin : Iir; Expr_Type : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Overflow_Literal);
      Location_Copy (Res, Origin);
      Set_Type (Res, Expr_Type);
      Set_Literal_Origin (Res, Origin);
      Set_Expr_Staticness (Res, Locally);
      return Res;
   end Build_Overflow;

   function Build_Overflow (Origin : Iir) return Iir is
   begin
      return Build_Overflow (Origin, Get_Type (Origin));
   end Build_Overflow;

   function Build_Constant (Val : Iir; Origin : Iir) return Iir
   is
      Res : Iir;
   begin
      --  Note: this must work for any literals, because it may be used to
      --  replace a locally static constant by its initial value.
      case Get_Kind (Val) is
         when Iir_Kind_Integer_Literal =>
            Res := Create_Iir (Iir_Kind_Integer_Literal);
            Set_Value (Res, Get_Value (Val));

         when Iir_Kind_Floating_Point_Literal =>
            Res := Create_Iir (Iir_Kind_Floating_Point_Literal);
            Set_Fp_Value (Res, Get_Fp_Value (Val));

         when Iir_Kind_Enumeration_Literal =>
            return Build_Enumeration_Constant
              (Iir_Index32 (Get_Enum_Pos (Val)), Origin);

         when Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal =>
            Res := Create_Iir (Iir_Kind_Physical_Int_Literal);
            Set_Unit_Name (Res, Get_Primary_Unit_Name
                             (Get_Base_Type (Get_Type (Origin))));
            Set_Value (Res, Get_Physical_Value (Val));

         when Iir_Kind_Unit_Declaration =>
            Res := Create_Iir (Iir_Kind_Physical_Int_Literal);
            Set_Value (Res, Get_Physical_Value (Val));
            Set_Unit_Name (Res, Get_Primary_Unit_Name (Get_Type (Val)));

         when Iir_Kind_String_Literal8 =>
            Res := Create_Iir (Iir_Kind_String_Literal8);
            Set_String8_Id (Res, Get_String8_Id (Val));
            Set_String_Length (Res, Get_String_Length (Val));

         when Iir_Kind_Simple_Aggregate =>
            Res := Create_Iir (Iir_Kind_Simple_Aggregate);
            Set_Simple_Aggregate_List (Res, Get_Simple_Aggregate_List (Val));
            Set_Literal_Subtype (Res, Get_Type (Origin));

         when Iir_Kind_Overflow_Literal =>
            Res := Create_Iir (Iir_Kind_Overflow_Literal);

         when others =>
            Error_Kind ("build_constant", Val);
      end case;
      Location_Copy (Res, Origin);
      Set_Type (Res, Get_Type (Origin));
      Set_Literal_Origin (Res, Origin);
      Set_Expr_Staticness (Res, Locally);
      return Res;
   end Build_Constant;

   --  FIXME: origin ?
   function Build_Boolean (Cond : Boolean) return Iir is
   begin
      if Cond then
         return Boolean_True;
      else
         return Boolean_False;
      end if;
   end Build_Boolean;

   function Build_Enumeration (Val : Iir_Index32; Origin : Iir)
                              return Iir_Enumeration_Literal
   is
      Enum_Type : constant Iir := Get_Base_Type (Get_Type (Origin));
      Enum_List : constant Iir_List :=
        Get_Enumeration_Literal_List (Enum_Type);
   begin
      return Get_Nth_Element (Enum_List, Integer (Val));
   end Build_Enumeration;

   function Build_Enumeration (Val : Boolean; Origin : Iir)
                              return Iir_Enumeration_Literal
   is
      Enum_Type : constant Iir := Get_Base_Type (Get_Type (Origin));
      Enum_List : constant Iir_List :=
        Get_Enumeration_Literal_List (Enum_Type);
   begin
      return Get_Nth_Element (Enum_List, Boolean'Pos (Val));
   end Build_Enumeration;

   function Build_Constant_Range (Range_Expr : Iir; Origin : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Iir_Kind_Range_Expression);
      Location_Copy (Res, Origin);
      Set_Type (Res, Get_Type (Range_Expr));
      Set_Left_Limit (Res, Get_Left_Limit (Range_Expr));
      Set_Right_Limit (Res, Get_Right_Limit (Range_Expr));
      Set_Direction (Res, Get_Direction (Range_Expr));
      Set_Range_Origin (Res, Origin);
      Set_Expr_Staticness (Res, Locally);
      return Res;
   end Build_Constant_Range;

   function Build_Extreme_Value (Is_Pos : Boolean; Origin : Iir) return Iir
   is
      Orig_Type : constant Iir := Get_Base_Type (Get_Type (Origin));
   begin
      case Get_Kind (Orig_Type) is
         when Iir_Kind_Integer_Type_Definition =>
            if Is_Pos then
               return Build_Integer (Iir_Int64'Last, Origin);
            else
               return Build_Integer (Iir_Int64'First, Origin);
            end if;
         when others =>
            Error_Kind ("build_extreme_value", Orig_Type);
      end case;
   end Build_Extreme_Value;

   --  A_RANGE is a range expression, whose type, location, expr_staticness,
   --  left_limit and direction are set.
   --  Type of A_RANGE must have a range_constraint.
   --  Set the right limit of A_RANGE from LEN.
   procedure Set_Right_Limit_By_Length (A_Range : Iir; Len : Iir_Int64)
   is
      Left, Right : Iir;
      Pos : Iir_Int64;
      A_Type : Iir;
   begin
      if Get_Expr_Staticness (A_Range) /= Locally then
         raise Internal_Error;
      end if;
      A_Type := Get_Type (A_Range);

      Left := Get_Left_Limit (A_Range);

      Pos := Eval_Pos (Left);
      case Get_Direction (A_Range) is
         when Iir_To =>
            Pos := Pos + Len -1;
         when Iir_Downto =>
            Pos := Pos - Len + 1;
      end case;
      if Len > 0
        and then not Eval_Int_In_Range (Pos, Get_Range_Constraint (A_Type))
      then
         Error_Msg_Sem ("range length is beyond subtype length", A_Range);
         Right := Left;
      else
         -- FIXME: what about nul range?
         Right := Build_Discrete (Pos, A_Range);
         Set_Literal_Origin (Right, Null_Iir);
      end if;
      Set_Right_Limit (A_Range, Right);
   end Set_Right_Limit_By_Length;

   --  Create a range of type A_TYPE whose length is LEN.
   --  Note: only two nodes are created:
   --  * the range_expression (node returned)
   --  * the right bound
   --  The left bound *IS NOT* created, but points to the left bound of A_TYPE.
   function Create_Range_By_Length
     (A_Type : Iir; Len : Iir_Int64; Loc : Location_Type)
     return Iir
   is
      Index_Constraint : Iir;
      Constraint : Iir;
   begin
      --  The left limit must be locally static in order to compute the right
      --  limit.
      pragma Assert (Get_Type_Staticness (A_Type) = Locally);

      Index_Constraint := Get_Range_Constraint (A_Type);
      Constraint := Create_Iir (Iir_Kind_Range_Expression);
      Set_Location (Constraint, Loc);
      Set_Expr_Staticness (Constraint, Locally);
      Set_Type (Constraint, A_Type);
      Set_Left_Limit (Constraint, Get_Left_Limit (Index_Constraint));
      Set_Direction (Constraint, Get_Direction (Index_Constraint));
      Set_Right_Limit_By_Length (Constraint, Len);
      return Constraint;
   end Create_Range_By_Length;

   function Create_Range_Subtype_From_Type (A_Type : Iir; Loc : Location_Type)
                                          return Iir
   is
      Res : Iir;
   begin
      pragma Assert (Get_Type_Staticness (A_Type) = Locally);

      case Get_Kind (A_Type) is
         when Iir_Kind_Enumeration_Type_Definition =>
            Res := Create_Iir (Iir_Kind_Enumeration_Subtype_Definition);
         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            Res := Create_Iir (Get_Kind (A_Type));
         when others =>
            Error_Kind ("create_range_subtype_by_length", A_Type);
      end case;
      Set_Location (Res, Loc);
      Set_Base_Type (Res, Get_Base_Type (A_Type));
      Set_Type_Staticness (Res, Locally);

      return Res;
   end Create_Range_Subtype_From_Type;

   --  Create a subtype of A_TYPE whose length is LEN.
   --  This is used to create subtypes for strings or aggregates.
   function Create_Range_Subtype_By_Length
     (A_Type : Iir; Len : Iir_Int64; Loc : Location_Type)
     return Iir
   is
      Res : Iir;
   begin
      Res := Create_Range_Subtype_From_Type (A_Type, Loc);

      Set_Range_Constraint (Res, Create_Range_By_Length (A_Type, Len, Loc));
      return Res;
   end Create_Range_Subtype_By_Length;

   function Create_Unidim_Array_From_Index
     (Base_Type : Iir; Index_Type : Iir; Loc : Iir)
     return Iir_Array_Subtype_Definition
   is
      Res : Iir_Array_Subtype_Definition;
   begin
      Res := Create_Array_Subtype (Base_Type, Get_Location (Loc));
      Append_Element (Get_Index_Subtype_List (Res), Index_Type);
      Set_Type_Staticness (Res, Min (Get_Type_Staticness (Res),
                                     Get_Type_Staticness (Index_Type)));
      Set_Constraint_State (Res, Fully_Constrained);
      Set_Index_Constraint_Flag (Res, True);
      return Res;
   end Create_Unidim_Array_From_Index;

   function Create_Unidim_Array_By_Length
     (Base_Type : Iir; Len : Iir_Int64; Loc : Iir)
     return Iir_Array_Subtype_Definition
   is
      Index_Type : constant Iir := Get_Index_Type (Base_Type, 0);
      N_Index_Type : Iir;
   begin
      N_Index_Type := Create_Range_Subtype_By_Length
        (Index_Type, Len, Get_Location (Loc));
      return Create_Unidim_Array_From_Index (Base_Type, N_Index_Type, Loc);
   end Create_Unidim_Array_By_Length;

   procedure Free_Eval_Static_Expr (Res : Iir; Orig : Iir) is
   begin
      if Res /= Orig and then Get_Literal_Origin (Res) = Orig then
         Free_Iir (Res);
      end if;
   end Free_Eval_Static_Expr;

   --  Free the result RES of Eval_String_Literal called with ORIG, if created.
   procedure Free_Eval_String_Literal (Res : Iir; Orig : Iir)
   is
      L : Iir_List;
   begin
      if Res /= Orig then
         L := Get_Simple_Aggregate_List (Res);
         Destroy_Iir_List (L);
         Free_Iir (Res);
      end if;
   end Free_Eval_String_Literal;

   function Eval_String_Literal (Str : Iir) return Iir
   is
      Len : Nat32;
   begin
      case Get_Kind (Str) is
         when Iir_Kind_String_Literal8 =>
            declare
               Element_Type : Iir;
               Literal_List : Iir_List;
               Lit : Iir;

               List : Iir_List;
               Id : String8_Id;
            begin
               Element_Type := Get_Base_Type
                 (Get_Element_Subtype (Get_Base_Type (Get_Type (Str))));
               Literal_List := Get_Enumeration_Literal_List (Element_Type);
               List := Create_Iir_List;

               Id := Get_String8_Id (Str);
               Len := Get_String_Length (Str);

               for I in 1 .. Len loop
                  Lit := Get_Nth_Element
                    (Literal_List,
                     Natural (Str_Table.Element_String8 (Id, I)));
                  Append_Element (List, Lit);
               end loop;
               return Build_Simple_Aggregate (List, Str, Get_Type (Str));
            end;

         when Iir_Kind_Simple_Aggregate =>
            return Str;

         when others =>
            Error_Kind ("eval_string_literal", Str);
      end case;
   end Eval_String_Literal;

   function Eval_Monadic_Operator (Orig : Iir; Operand : Iir) return Iir
   is
      pragma Unsuppress (Overflow_Check);

      Func : Iir_Predefined_Functions;
   begin
      if Get_Kind (Operand) = Iir_Kind_Overflow_Literal then
         --  Propagate overflow.
         return Build_Overflow (Orig);
      end if;

      Func := Get_Implicit_Definition (Get_Implementation (Orig));
      case Func is
         when Iir_Predefined_Integer_Negation =>
            return Build_Integer (-Get_Value (Operand), Orig);
         when Iir_Predefined_Integer_Identity =>
            return Build_Integer (Get_Value (Operand), Orig);
         when Iir_Predefined_Integer_Absolute =>
            return Build_Integer (abs Get_Value (Operand), Orig);

         when Iir_Predefined_Floating_Negation =>
            return Build_Floating (-Get_Fp_Value (Operand), Orig);
         when Iir_Predefined_Floating_Identity =>
            return Build_Floating (Get_Fp_Value (Operand), Orig);
         when Iir_Predefined_Floating_Absolute =>
            return Build_Floating (abs Get_Fp_Value (Operand), Orig);

         when Iir_Predefined_Physical_Negation =>
            return Build_Physical (-Get_Physical_Value (Operand), Orig);
         when Iir_Predefined_Physical_Identity =>
            return Build_Physical (Get_Physical_Value (Operand), Orig);
         when Iir_Predefined_Physical_Absolute =>
            return Build_Physical (abs Get_Physical_Value (Operand), Orig);

         when Iir_Predefined_Boolean_Not
           | Iir_Predefined_Bit_Not =>
            return Build_Enumeration (Get_Enum_Pos (Operand) = 0, Orig);

         when Iir_Predefined_TF_Array_Not =>
            declare
               O_List : Iir_List;
               R_List : Iir_List;
               El : Iir;
               Lit : Iir;
            begin
               O_List := Get_Simple_Aggregate_List
                 (Eval_String_Literal (Operand));
               R_List := Create_Iir_List;

               for I in Natural loop
                  El := Get_Nth_Element (O_List, I);
                  exit when El = Null_Iir;
                  case Get_Enum_Pos (El) is
                     when 0 =>
                        Lit := Bit_1;
                     when 1 =>
                        Lit := Bit_0;
                     when others =>
                        raise Internal_Error;
                  end case;
                  Append_Element (R_List, Lit);
               end loop;
               return Build_Simple_Aggregate
                 (R_List, Orig, Get_Type (Operand));
            end;

         when Iir_Predefined_Enum_To_String =>
            return Eval_Enum_To_String (Operand, Orig);
         when Iir_Predefined_Integer_To_String =>
            return Eval_Integer_Image (Get_Value (Operand), Orig);

         when others =>
            Error_Internal (Orig, "eval_monadic_operator: " &
                            Iir_Predefined_Functions'Image (Func));
      end case;
   exception
      when Constraint_Error =>
         --  Can happen for absolute.
         Warning_Msg_Sem ("arithmetic overflow in static expression", Orig);
         return Build_Overflow (Orig);
   end Eval_Monadic_Operator;

   function Eval_Dyadic_Bit_Array_Operator
     (Expr : Iir;
      Left, Right : Iir;
      Func : Iir_Predefined_Dyadic_TF_Array_Functions)
     return Iir
   is
      use Str_Table;
      L_Str : constant String8_Id := Get_String8_Id (Left);
      R_Str : constant String8_Id := Get_String8_Id (Right);
      Len : Nat32;
      Id : String8_Id;
      Res : Iir;
   begin
      Len := Get_String_Length (Left);
      if Len /= Get_String_Length (Right) then
         Warning_Msg_Sem ("length of left and right operands mismatch", Expr);
         return Build_Overflow (Expr);
      else
         Id := Create_String8;
         case Func is
            when Iir_Predefined_TF_Array_And =>
               for I in 1 .. Len loop
                  case Element_String8 (L_Str, I) is
                     when 0 =>
                        Append_String8 (0);
                     when 1 =>
                        Append_String8 (Element_String8 (R_Str, I));
                     when others =>
                        raise Internal_Error;
                  end case;
               end loop;
            when Iir_Predefined_TF_Array_Nand =>
               for I in 1 .. Len loop
                  case Element_String8 (L_Str, I) is
                     when 0 =>
                        Append_String8 (1);
                     when 1 =>
                        case Element_String8 (R_Str, I) is
                           when 0 =>
                              Append_String8 (1);
                           when 1 =>
                              Append_String8 (0);
                           when others =>
                              raise Internal_Error;
                        end case;
                     when others =>
                        raise Internal_Error;
                  end case;
               end loop;
            when Iir_Predefined_TF_Array_Or =>
               for I in 1 .. Len loop
                  case Element_String8 (L_Str, I) is
                     when 1 =>
                        Append_String8 (1);
                     when 0 =>
                        Append_String8 (Element_String8 (R_Str, I));
                     when others =>
                        raise Internal_Error;
                  end case;
               end loop;
            when Iir_Predefined_TF_Array_Nor =>
               for I in 1 .. Len loop
                  case Element_String8 (L_Str, I) is
                     when 1 =>
                        Append_String8 (0);
                     when 0 =>
                        case Element_String8 (R_Str, I) is
                           when 0 =>
                              Append_String8 (1);
                           when 1 =>
                              Append_String8 (0);
                           when others =>
                              raise Internal_Error;
                        end case;
                     when others =>
                        raise Internal_Error;
                  end case;
               end loop;
            when Iir_Predefined_TF_Array_Xor =>
               for I in 1 .. Len loop
                  case Element_String8 (L_Str, I) is
                     when 1 =>
                        case Element_String8 (R_Str, I) is
                           when 0 =>
                              Append_String8 (1);
                           when 1 =>
                              Append_String8 (0);
                           when others =>
                              raise Internal_Error;
                        end case;
                     when 0 =>
                        Append_String8 (Element_String8 (R_Str, I));
                     when others =>
                        raise Internal_Error;
                  end case;
               end loop;
            when others =>
               Error_Internal (Expr, "eval_dyadic_bit_array_functions: " &
                               Iir_Predefined_Functions'Image (Func));
         end case;
         Res := Build_String (Id, Len, Expr);

         --  The unconstrained type is replaced by the constrained one.
         Set_Type (Res, Get_Type (Left));
         return Res;
      end if;
   end Eval_Dyadic_Bit_Array_Operator;

   --  Return TRUE if VAL /= 0.
   function Check_Integer_Division_By_Zero (Expr : Iir; Val : Iir)
                                           return Boolean
   is
   begin
      if Get_Value (Val) = 0 then
         Warning_Msg_Sem ("division by 0", Expr);
         return False;
      else
         return True;
      end if;
   end Check_Integer_Division_By_Zero;

   function Eval_Shift_Operator
     (Left, Right : Iir; Origin : Iir; Func : Iir_Predefined_Shift_Functions)
     return Iir
   is
      Count : Iir_Int64;
      Cnt : Natural;
      Len : Natural;
      Arr_List : Iir_List;
      Res_List : Iir_List;
      Dir_Left : Boolean;
      E : Iir;
   begin
      Count := Get_Value (Right);
      Arr_List := Get_Simple_Aggregate_List (Left);
      Len := Get_Nbr_Elements (Arr_List);
      --  LRM93 7.2.3
      --  That is, if R is 0 or if L is a null array, the return value is L.
      if Count = 0 or Len = 0 then
         return Build_Simple_Aggregate (Arr_List, Origin, Get_Type (Left));
      end if;
      case Func is
         when Iir_Predefined_Array_Sll
           | Iir_Predefined_Array_Sla
           | Iir_Predefined_Array_Rol =>
            Dir_Left := True;
         when Iir_Predefined_Array_Srl
           | Iir_Predefined_Array_Sra
           | Iir_Predefined_Array_Ror =>
            Dir_Left := False;
      end case;
      if Count < 0 then
         Cnt := Natural (-Count);
         Dir_Left := not Dir_Left;
      else
         Cnt := Natural (Count);
      end if;

      case Func is
         when Iir_Predefined_Array_Sll
           | Iir_Predefined_Array_Srl =>
            declare
               Enum_List : Iir_List;
            begin
               Enum_List := Get_Enumeration_Literal_List
                 (Get_Base_Type (Get_Element_Subtype (Get_Type (Left))));
               E := Get_Nth_Element (Enum_List, 0);
            end;
         when Iir_Predefined_Array_Sla
           | Iir_Predefined_Array_Sra =>
            if Dir_Left then
               E := Get_Nth_Element (Arr_List, Len - 1);
            else
               E := Get_Nth_Element (Arr_List, 0);
            end if;
         when Iir_Predefined_Array_Rol
           | Iir_Predefined_Array_Ror =>
            Cnt := Cnt mod Len;
            if not Dir_Left then
               Cnt := (Len - Cnt) mod Len;
            end if;
      end case;

      Res_List := Create_Iir_List;

      case Func is
         when Iir_Predefined_Array_Sll
           | Iir_Predefined_Array_Srl
           | Iir_Predefined_Array_Sla
           | Iir_Predefined_Array_Sra =>
            if Dir_Left then
               if Cnt < Len then
                  for I in Cnt .. Len - 1 loop
                     Append_Element
                       (Res_List, Get_Nth_Element (Arr_List, I));
                  end loop;
               else
                  Cnt := Len;
               end if;
               for I in 0 .. Cnt - 1 loop
                  Append_Element (Res_List, E);
               end loop;
            else
               if Cnt > Len then
                  Cnt := Len;
               end if;
               for I in 0 .. Cnt - 1 loop
                  Append_Element (Res_List, E);
               end loop;
               for I in Cnt .. Len - 1 loop
                  Append_Element
                    (Res_List, Get_Nth_Element (Arr_List, I - Cnt));
               end loop;
            end if;
         when Iir_Predefined_Array_Rol
           | Iir_Predefined_Array_Ror =>
            for I in 1 .. Len loop
               Append_Element
                 (Res_List, Get_Nth_Element (Arr_List, Cnt));
               Cnt := Cnt + 1;
               if Cnt = Len then
                  Cnt := 0;
               end if;
            end loop;
      end case;
      return Build_Simple_Aggregate (Res_List, Origin, Get_Type (Left));
   end Eval_Shift_Operator;

   --  Note: operands must be locally static.
   function Eval_Concatenation
     (Left, Right : Iir; Orig : Iir; Func : Iir_Predefined_Concat_Functions)
     return Iir
   is
      Res_List : Iir_List;
      L : Natural;
      Res_Type : Iir;
      Origin_Type : Iir;
      Left_Aggr, Right_Aggr : Iir;
      Left_List, Right_List : Iir_List;
      Left_Len : Natural;
   begin
      Res_List := Create_Iir_List;
      --  Do the concatenation.
      --  Left:
      case Func is
         when Iir_Predefined_Element_Array_Concat
           | Iir_Predefined_Element_Element_Concat =>
            Append_Element (Res_List, Left);
            Left_Len := 1;
         when Iir_Predefined_Array_Element_Concat
           | Iir_Predefined_Array_Array_Concat =>
            Left_Aggr := Eval_String_Literal (Left);
            Left_List := Get_Simple_Aggregate_List (Left_Aggr);
            Left_Len := Get_Nbr_Elements (Left_List);
            for I in 0 .. Left_Len - 1 loop
               Append_Element (Res_List, Get_Nth_Element (Left_List, I));
            end loop;
            Free_Eval_String_Literal (Left_Aggr, Left);
      end case;
      --  Right:
      case Func is
         when Iir_Predefined_Array_Element_Concat
           | Iir_Predefined_Element_Element_Concat =>
            Append_Element (Res_List, Right);
         when Iir_Predefined_Element_Array_Concat
           | Iir_Predefined_Array_Array_Concat =>
            Right_Aggr := Eval_String_Literal (Right);
            Right_List := Get_Simple_Aggregate_List (Right_Aggr);
            L := Get_Nbr_Elements (Right_List);
            for I in 0 .. L - 1 loop
               Append_Element (Res_List, Get_Nth_Element (Right_List, I));
            end loop;
            Free_Eval_String_Literal (Right_Aggr, Right);
      end case;
      L := Get_Nbr_Elements (Res_List);

      --  Compute subtype...
      Origin_Type := Get_Type (Orig);
      Res_Type := Null_Iir;
      if Func = Iir_Predefined_Array_Array_Concat
        and then Left_Len = 0
      then
         if Flags.Vhdl_Std = Vhdl_87 then
            --  LRM87 7.2.3
            --  [...], unless the left operand is a null array, in which case
            --  the result of the concatenation is the right operand.
            Res_Type := Get_Type (Right);
         else
            --  LRM93 7.2.4
            --  If both operands are null arrays, then the result of the
            --  concatenation is the right operand.
            if Get_Nbr_Elements (Right_List) = 0 then
               Res_Type := Get_Type (Right);
            end if;
         end if;
      end if;
      if Res_Type = Null_Iir then
         if Flags.Vhdl_Std = Vhdl_87
           and then (Func = Iir_Predefined_Array_Array_Concat
                     or Func = Iir_Predefined_Array_Element_Concat)
         then
            --  LRM87 7.2.3
            --  The left bound of the result is the left operand, [...]
            --
            --  LRM87 7.2.3
            --  The direction of the result is the direction of the left
            --  operand, [...]
            declare
               Left_Index : constant Iir :=
                 Get_Index_Type (Get_Type (Left), 0);
               Left_Range : constant Iir :=
                 Get_Range_Constraint (Left_Index);
               Ret_Type : constant Iir :=
                 Get_Return_Type (Get_Implementation (Orig));
               A_Range : Iir;
               Index_Type : Iir;
            begin
               A_Range := Create_Iir (Iir_Kind_Range_Expression);
               Set_Type (A_Range, Get_Index_Type (Ret_Type, 0));
               Set_Expr_Staticness (A_Range, Locally);
               Set_Left_Limit (A_Range, Get_Left_Limit (Left_Range));
               Set_Direction (A_Range, Get_Direction (Left_Range));
               Location_Copy (A_Range, Orig);
               Set_Right_Limit_By_Length (A_Range, Iir_Int64 (L));
               Index_Type := Create_Range_Subtype_From_Type
                 (Left_Index, Get_Location (Orig));
               Set_Range_Constraint (Index_Type, A_Range);
               Res_Type := Create_Unidim_Array_From_Index
                 (Origin_Type, Index_Type, Orig);
            end;
         else
            --  LRM93 7.2.4
            --  Otherwise, the direction and bounds of the result are
            --  determined as follows: let S be the index subtype of the base
            --  type of the result.  The direction of the result of the
            --  concatenation is the direction of S, and the left bound of the
            --  result is S'LEFT.
            Res_Type := Create_Unidim_Array_By_Length
              (Origin_Type, Iir_Int64 (L), Orig);
         end if;
      end if;
      --  FIXME: this is not necessarily a string, it may be an aggregate if
      --  element type is not a character type.
      return Build_Simple_Aggregate (Res_List, Orig, Res_Type);
   end Eval_Concatenation;

   function Eval_Discrete_Compare (Left, Right : Iir) return Compare_Type
   is
      Ltype : constant Iir := Get_Base_Type (Get_Type (Left));
   begin
      pragma Assert
        (Get_Kind (Ltype) = Get_Kind (Get_Base_Type (Get_Type (Right))));

      case Get_Kind (Ltype) is
         when Iir_Kind_Enumeration_Type_Definition =>
            declare
               L_Pos : constant Iir_Int32 := Get_Enum_Pos (Left);
               R_Pos : constant Iir_Int32 := Get_Enum_Pos (Right);
            begin
               if L_Pos = R_Pos then
                  return Compare_Eq;
               else
                  if L_Pos < R_Pos then
                     return Compare_Lt;
                  else
                     return Compare_Gt;
                  end if;
               end if;
            end;
         when Iir_Kind_Integer_Type_Definition =>
            declare
               L_Val : constant Iir_Int64 := Get_Value (Left);
               R_Val : constant Iir_Int64 := Get_Value (Right);
            begin
               if L_Val = R_Val then
                  return Compare_Eq;
               else
                  if L_Val < R_Val then
                     return Compare_Lt;
                  else
                     return Compare_Gt;
                  end if;
               end if;
            end;
         when others =>
            Error_Kind ("eval_discrete_compare", Ltype);
      end case;
   end Eval_Discrete_Compare;

   function Eval_Array_Compare (Left, Right : Iir) return Compare_Type is
   begin
      if Get_Kind (Left) = Iir_Kind_String_Literal8
        and then Get_Kind (Right) = Iir_Kind_String_Literal8
      then
         --  Common case: both parameters are strings.
         declare
            L_Id : constant String8_Id := Get_String8_Id (Left);
            R_Id : constant String8_Id := Get_String8_Id (Right);
            L_Len : constant Int32 := Get_String_Length (Left);
            R_Len : constant Int32 := Get_String_Length (Right);
            L_El, R_El : Nat8;
            P : Nat32;
         begin
            P := 1;
            while P <= L_Len and P <= R_Len loop
               L_El := Str_Table.Element_String8 (L_Id, P);
               R_El := Str_Table.Element_String8 (R_Id, P);
               if L_El /= R_El then
                  if L_El < R_El then
                     return Compare_Lt;
                  else
                     return Compare_Gt;
                  end if;
               end if;
               P := P + 1;
            end loop;
            if L_Len = R_Len then
               return Compare_Eq;
            elsif L_Len < R_Len then
               return Compare_Lt;
            else
               return Compare_Gt;
            end if;
         end;
      else
         --  General case.
         declare
            Left_Val, Right_Val : Iir;
            R_List, L_List : Iir_List;
            R_Len, L_Len : Natural;
            P : Natural;
            Res : Compare_Type;
         begin
            Left_Val := Eval_String_Literal (Left);
            Right_Val := Eval_String_Literal (Right);

            L_List := Get_Simple_Aggregate_List (Left_Val);
            R_List := Get_Simple_Aggregate_List (Right_Val);
            L_Len := Get_Nbr_Elements (L_List);
            R_Len := Get_Nbr_Elements (R_List);

            Res := Compare_Eq;
            P := 0;
            while P < L_Len and P < R_Len loop
               Res := Eval_Discrete_Compare (Get_Nth_Element (L_List, P),
                                             Get_Nth_Element (R_List, P));
               exit when Res /= Compare_Eq;
               P := P + 1;
            end loop;
            if Res = Compare_Eq then
               if L_Len < R_Len then
                  Res := Compare_Lt;
               elsif L_Len > R_Len then
                  Res := Compare_Gt;
               end if;
            end if;

            Free_Eval_Static_Expr (Left_Val, Left);
            Free_Eval_Static_Expr (Right_Val, Right);

            return Res;
         end;
      end if;
   end Eval_Array_Compare;

   --  ORIG is either a dyadic operator or a function call.
   function Eval_Dyadic_Operator (Orig : Iir; Imp : Iir; Left, Right : Iir)
     return Iir
   is
      pragma Unsuppress (Overflow_Check);
      Func : constant Iir_Predefined_Functions :=
        Get_Implicit_Definition (Imp);
   begin
      if Get_Kind (Left) = Iir_Kind_Overflow_Literal
        or else Get_Kind (Right) = Iir_Kind_Overflow_Literal
      then
         return Build_Overflow (Orig);
      end if;

      case Func is
         when Iir_Predefined_Integer_Plus =>
            return Build_Integer (Get_Value (Left) + Get_Value (Right), Orig);
         when Iir_Predefined_Integer_Minus =>
            return Build_Integer (Get_Value (Left) - Get_Value (Right), Orig);
         when Iir_Predefined_Integer_Mul =>
            return Build_Integer (Get_Value (Left) * Get_Value (Right), Orig);
         when Iir_Predefined_Integer_Div =>
            if Check_Integer_Division_By_Zero (Orig, Right) then
               return Build_Integer
                 (Get_Value (Left) / Get_Value (Right), Orig);
            else
               return Build_Overflow (Orig);
            end if;
         when Iir_Predefined_Integer_Mod =>
            if Check_Integer_Division_By_Zero (Orig, Right) then
               return Build_Integer
                 (Get_Value (Left) mod Get_Value (Right), Orig);
            else
               return Build_Overflow (Orig);
            end if;
         when Iir_Predefined_Integer_Rem =>
            if Check_Integer_Division_By_Zero (Orig, Right) then
               return Build_Integer
                 (Get_Value (Left) rem Get_Value (Right), Orig);
            else
               return Build_Overflow (Orig);
            end if;
         when Iir_Predefined_Integer_Exp =>
            return Build_Integer
              (Get_Value (Left) ** Integer (Get_Value (Right)), Orig);

         when Iir_Predefined_Integer_Equality =>
            return Build_Boolean (Get_Value (Left) = Get_Value (Right));
         when Iir_Predefined_Integer_Inequality =>
            return Build_Boolean (Get_Value (Left) /= Get_Value (Right));
         when Iir_Predefined_Integer_Greater_Equal =>
            return Build_Boolean (Get_Value (Left) >= Get_Value (Right));
         when Iir_Predefined_Integer_Greater =>
            return Build_Boolean (Get_Value (Left) > Get_Value (Right));
         when Iir_Predefined_Integer_Less_Equal =>
            return Build_Boolean (Get_Value (Left) <= Get_Value (Right));
         when Iir_Predefined_Integer_Less =>
            return Build_Boolean (Get_Value (Left) < Get_Value (Right));

         when Iir_Predefined_Integer_Minimum =>
            if Get_Value (Left) < Get_Value (Right) then
               return Left;
            else
               return Right;
            end if;
         when Iir_Predefined_Integer_Maximum =>
            if Get_Value (Left) > Get_Value (Right) then
               return Left;
            else
               return Right;
            end if;

         when Iir_Predefined_Floating_Equality =>
            return Build_Boolean (Get_Fp_Value (Left) = Get_Fp_Value (Right));
         when Iir_Predefined_Floating_Inequality =>
            return Build_Boolean (Get_Fp_Value (Left) /= Get_Fp_Value (Right));
         when Iir_Predefined_Floating_Greater =>
            return Build_Boolean (Get_Fp_Value (Left) > Get_Fp_Value (Right));
         when Iir_Predefined_Floating_Greater_Equal =>
            return Build_Boolean (Get_Fp_Value (Left) >= Get_Fp_Value (Right));
         when Iir_Predefined_Floating_Less =>
            return Build_Boolean (Get_Fp_Value (Left) < Get_Fp_Value (Right));
         when Iir_Predefined_Floating_Less_Equal =>
            return Build_Boolean (Get_Fp_Value (Left) <= Get_Fp_Value (Right));

         when Iir_Predefined_Floating_Minus =>
            return Build_Floating
              (Get_Fp_Value (Left) - Get_Fp_Value (Right), Orig);
         when Iir_Predefined_Floating_Plus =>
            return Build_Floating
              (Get_Fp_Value (Left) + Get_Fp_Value (Right), Orig);
         when Iir_Predefined_Floating_Mul =>
            return Build_Floating
              (Get_Fp_Value (Left) * Get_Fp_Value (Right), Orig);
         when Iir_Predefined_Floating_Div =>
            if Get_Fp_Value (Right) = 0.0 then
               Warning_Msg_Sem ("right operand of division is 0", Orig);
               return Build_Overflow (Orig);
            else
               return Build_Floating
                 (Get_Fp_Value (Left) / Get_Fp_Value (Right), Orig);
            end if;
         when Iir_Predefined_Floating_Exp =>
            declare
               Exp : Iir_Int64;
               Res : Iir_Fp64;
               Val : Iir_Fp64;
            begin
               Res := 1.0;
               Val := Get_Fp_Value (Left);
               Exp := abs Get_Value (Right);
               while Exp /= 0 loop
                  if Exp mod 2 = 1 then
                     Res := Res * Val;
                  end if;
                  Exp := Exp / 2;
                  Val := Val * Val;
               end loop;
               if Get_Value (Right) < 0 then
                  Res := 1.0 / Res;
               end if;
               return Build_Floating (Res, Orig);
            end;

         when Iir_Predefined_Floating_Minimum =>
            if Get_Fp_Value (Left) < Get_Fp_Value (Right) then
               return Left;
            else
               return Right;
            end if;
         when Iir_Predefined_Floating_Maximum =>
            if Get_Fp_Value (Left) > Get_Fp_Value (Right) then
               return Left;
            else
               return Right;
            end if;

         when Iir_Predefined_Physical_Equality =>
            return Build_Boolean
              (Get_Physical_Value (Left) = Get_Physical_Value (Right));
         when Iir_Predefined_Physical_Inequality =>
            return Build_Boolean
              (Get_Physical_Value (Left) /= Get_Physical_Value (Right));
         when Iir_Predefined_Physical_Greater_Equal =>
            return Build_Boolean
              (Get_Physical_Value (Left) >= Get_Physical_Value (Right));
         when Iir_Predefined_Physical_Greater =>
            return Build_Boolean
              (Get_Physical_Value (Left) > Get_Physical_Value (Right));
         when Iir_Predefined_Physical_Less_Equal =>
            return Build_Boolean
              (Get_Physical_Value (Left) <= Get_Physical_Value (Right));
         when Iir_Predefined_Physical_Less =>
            return Build_Boolean
              (Get_Physical_Value (Left) < Get_Physical_Value (Right));

         when Iir_Predefined_Physical_Physical_Div =>
            return Build_Integer
              (Get_Physical_Value (Left) / Get_Physical_Value (Right), Orig);
         when Iir_Predefined_Physical_Integer_Div =>
            return Build_Physical
              (Get_Physical_Value (Left) / Get_Value (Right), Orig);
         when Iir_Predefined_Physical_Minus =>
            return Build_Physical
              (Get_Physical_Value (Left) - Get_Physical_Value (Right), Orig);
         when Iir_Predefined_Physical_Plus =>
            return Build_Physical
              (Get_Physical_Value (Left) + Get_Physical_Value (Right), Orig);
         when Iir_Predefined_Integer_Physical_Mul =>
            return Build_Physical
              (Get_Value (Left) * Get_Physical_Value (Right), Orig);
         when Iir_Predefined_Physical_Integer_Mul =>
            return Build_Physical
              (Get_Physical_Value (Left) * Get_Value (Right), Orig);
         when Iir_Predefined_Real_Physical_Mul =>
            --  FIXME: overflow??
            return Build_Physical
              (Iir_Int64 (Get_Fp_Value (Left)
                          * Iir_Fp64 (Get_Physical_Value (Right))), Orig);
         when Iir_Predefined_Physical_Real_Mul =>
            --  FIXME: overflow??
            return Build_Physical
              (Iir_Int64 (Iir_Fp64 (Get_Physical_Value (Left))
                          * Get_Fp_Value (Right)), Orig);
         when Iir_Predefined_Physical_Real_Div =>
            --  FIXME: overflow??
            return Build_Physical
              (Iir_Int64 (Iir_Fp64 (Get_Physical_Value (Left))
                          / Get_Fp_Value (Right)), Orig);

         when Iir_Predefined_Physical_Minimum =>
            return Build_Physical (Iir_Int64'Min (Get_Physical_Value (Left),
                                                  Get_Physical_Value (Right)),
                                   Orig);
         when Iir_Predefined_Physical_Maximum =>
            return Build_Physical (Iir_Int64'Max (Get_Physical_Value (Left),
                                                  Get_Physical_Value (Right)),
                                   Orig);

         when Iir_Predefined_Element_Array_Concat
           | Iir_Predefined_Array_Element_Concat
           | Iir_Predefined_Array_Array_Concat
           | Iir_Predefined_Element_Element_Concat =>
            return Eval_Concatenation (Left, Right, Orig, Func);

         when Iir_Predefined_Enum_Equality
           | Iir_Predefined_Bit_Match_Equality =>
            return Build_Enumeration
              (Get_Enum_Pos (Left) = Get_Enum_Pos (Right), Orig);
         when Iir_Predefined_Enum_Inequality
           | Iir_Predefined_Bit_Match_Inequality =>
            return Build_Enumeration
              (Get_Enum_Pos (Left) /= Get_Enum_Pos (Right), Orig);
         when Iir_Predefined_Enum_Greater_Equal
           | Iir_Predefined_Bit_Match_Greater_Equal =>
            return Build_Enumeration
              (Get_Enum_Pos (Left) >= Get_Enum_Pos (Right), Orig);
         when Iir_Predefined_Enum_Greater
           | Iir_Predefined_Bit_Match_Greater =>
            return Build_Enumeration
              (Get_Enum_Pos (Left) > Get_Enum_Pos (Right), Orig);
         when Iir_Predefined_Enum_Less_Equal
           | Iir_Predefined_Bit_Match_Less_Equal =>
            return Build_Enumeration
              (Get_Enum_Pos (Left) <= Get_Enum_Pos (Right), Orig);
         when Iir_Predefined_Enum_Less
           | Iir_Predefined_Bit_Match_Less =>
            return Build_Enumeration
              (Get_Enum_Pos (Left) < Get_Enum_Pos (Right), Orig);

         when Iir_Predefined_Enum_Minimum =>
            if Get_Enum_Pos (Left) < Get_Enum_Pos (Right) then
               return Left;
            else
               return Right;
            end if;
         when Iir_Predefined_Enum_Maximum =>
            if Get_Enum_Pos (Left) > Get_Enum_Pos (Right) then
               return Left;
            else
               return Right;
            end if;

         when Iir_Predefined_Boolean_And
           | Iir_Predefined_Bit_And =>
            return Build_Enumeration
              (Get_Enum_Pos (Left) = 1 and Get_Enum_Pos (Right) = 1, Orig);
         when Iir_Predefined_Boolean_Nand
           | Iir_Predefined_Bit_Nand =>
            return Build_Enumeration
              (not (Get_Enum_Pos (Left) = 1 and Get_Enum_Pos (Right) = 1),
               Orig);
         when Iir_Predefined_Boolean_Or
           | Iir_Predefined_Bit_Or =>
            return Build_Enumeration
              (Get_Enum_Pos (Left) = 1 or Get_Enum_Pos (Right) = 1, Orig);
         when Iir_Predefined_Boolean_Nor
           | Iir_Predefined_Bit_Nor =>
            return Build_Enumeration
              (not (Get_Enum_Pos (Left) = 1 or Get_Enum_Pos (Right) = 1),
               Orig);
         when Iir_Predefined_Boolean_Xor
           | Iir_Predefined_Bit_Xor =>
            return Build_Enumeration
              (Get_Enum_Pos (Left) = 1 xor Get_Enum_Pos (Right) = 1, Orig);
         when Iir_Predefined_Boolean_Xnor
           | Iir_Predefined_Bit_Xnor =>
            return Build_Enumeration
              (not (Get_Enum_Pos (Left) = 1 xor Get_Enum_Pos (Right) = 1),
               Orig);

         when Iir_Predefined_Dyadic_TF_Array_Functions =>
            --  FIXME: only for bit ?
            return Eval_Dyadic_Bit_Array_Operator (Orig, Left, Right, Func);

         when Iir_Predefined_Universal_R_I_Mul =>
            return Build_Floating
              (Get_Fp_Value (Left) * Iir_Fp64 (Get_Value (Right)), Orig);
         when Iir_Predefined_Universal_I_R_Mul =>
            return Build_Floating
              (Iir_Fp64 (Get_Value (Left)) * Get_Fp_Value (Right), Orig);
         when Iir_Predefined_Universal_R_I_Div =>
            return Build_Floating
              (Get_Fp_Value (Left) / Iir_Fp64 (Get_Value (Right)), Orig);

         when Iir_Predefined_Array_Sll
           | Iir_Predefined_Array_Srl
           | Iir_Predefined_Array_Sla
           | Iir_Predefined_Array_Sra
           | Iir_Predefined_Array_Rol
           | Iir_Predefined_Array_Ror =>
            declare
               Left_Aggr : Iir;
               Res : Iir;
            begin
               Left_Aggr := Eval_String_Literal (Left);
               Res := Eval_Shift_Operator (Left_Aggr, Right, Orig, Func);
               Free_Eval_String_Literal (Left_Aggr, Left);
               return Res;
            end;

         when Iir_Predefined_Array_Equality =>
            return Build_Boolean
              (Eval_Array_Compare (Left, Right) = Compare_Eq);
         when Iir_Predefined_Array_Inequality =>
            return Build_Boolean
              (Eval_Array_Compare (Left, Right) /= Compare_Eq);
         when Iir_Predefined_Array_Less =>
            return Build_Boolean
              (Eval_Array_Compare (Left, Right) = Compare_Lt);
         when Iir_Predefined_Array_Less_Equal =>
            return Build_Boolean
              (Eval_Array_Compare (Left, Right) <= Compare_Eq);
         when Iir_Predefined_Array_Greater =>
            return Build_Boolean
              (Eval_Array_Compare (Left, Right) = Compare_Gt);
         when Iir_Predefined_Array_Greater_Equal =>
            return Build_Boolean
              (Eval_Array_Compare (Left, Right) >= Compare_Eq);

         when Iir_Predefined_Boolean_Not
           | Iir_Predefined_Boolean_Rising_Edge
           | Iir_Predefined_Boolean_Falling_Edge
           | Iir_Predefined_Bit_Not
           | Iir_Predefined_Bit_Rising_Edge
           | Iir_Predefined_Bit_Falling_Edge
           | Iir_Predefined_Integer_Absolute
           | Iir_Predefined_Integer_Identity
           | Iir_Predefined_Integer_Negation
           | Iir_Predefined_Floating_Absolute
           | Iir_Predefined_Floating_Negation
           | Iir_Predefined_Floating_Identity
           | Iir_Predefined_Physical_Absolute
           | Iir_Predefined_Physical_Identity
           | Iir_Predefined_Physical_Negation
           | Iir_Predefined_Error
           | Iir_Predefined_Record_Equality
           | Iir_Predefined_Record_Inequality
           | Iir_Predefined_Access_Equality
           | Iir_Predefined_Access_Inequality
           | Iir_Predefined_TF_Array_Not
           | Iir_Predefined_Now_Function
           | Iir_Predefined_Deallocate
           | Iir_Predefined_Write
           | Iir_Predefined_Read
           | Iir_Predefined_Read_Length
           | Iir_Predefined_Flush
           | Iir_Predefined_File_Open
           | Iir_Predefined_File_Open_Status
           | Iir_Predefined_File_Close
           | Iir_Predefined_Endfile
           | Iir_Predefined_Attribute_Image
           | Iir_Predefined_Attribute_Value
           | Iir_Predefined_Attribute_Pos
           | Iir_Predefined_Attribute_Val
           | Iir_Predefined_Attribute_Succ
           | Iir_Predefined_Attribute_Pred
           | Iir_Predefined_Attribute_Rightof
           | Iir_Predefined_Attribute_Leftof
           | Iir_Predefined_Attribute_Left
           | Iir_Predefined_Attribute_Right
           | Iir_Predefined_Attribute_Event
           | Iir_Predefined_Attribute_Active
           | Iir_Predefined_Attribute_Last_Value
           | Iir_Predefined_Attribute_Last_Event
           | Iir_Predefined_Attribute_Last_Active
           | Iir_Predefined_Attribute_Driving
           | Iir_Predefined_Attribute_Driving_Value
           | Iir_Predefined_Array_Char_To_String
           | Iir_Predefined_Bit_Vector_To_Ostring
           | Iir_Predefined_Bit_Vector_To_Hstring =>
            --  Not binary or never locally static.
            Error_Internal (Orig, "eval_dyadic_operator: " &
                              Iir_Predefined_Functions'Image (Func));

         when Iir_Predefined_Bit_Condition =>
            raise Internal_Error;

         when Iir_Predefined_Array_Minimum
           | Iir_Predefined_Array_Maximum
           | Iir_Predefined_Vector_Minimum
           | Iir_Predefined_Vector_Maximum =>
            raise Internal_Error;

         when Iir_Predefined_Std_Ulogic_Match_Equality
           | Iir_Predefined_Std_Ulogic_Match_Inequality
           | Iir_Predefined_Std_Ulogic_Match_Less
           | Iir_Predefined_Std_Ulogic_Match_Less_Equal
           | Iir_Predefined_Std_Ulogic_Match_Greater
           | Iir_Predefined_Std_Ulogic_Match_Greater_Equal =>
            -- TODO
            raise Internal_Error;

         when Iir_Predefined_Enum_To_String
           | Iir_Predefined_Integer_To_String
           | Iir_Predefined_Floating_To_String
           | Iir_Predefined_Real_To_String_Digits
           | Iir_Predefined_Real_To_String_Format
           | Iir_Predefined_Physical_To_String
           | Iir_Predefined_Time_To_String_Unit =>
            --  TODO
            raise Internal_Error;

         when Iir_Predefined_TF_Array_Element_And
           | Iir_Predefined_TF_Element_Array_And
           | Iir_Predefined_TF_Array_Element_Or
           | Iir_Predefined_TF_Element_Array_Or
           | Iir_Predefined_TF_Array_Element_Nand
           | Iir_Predefined_TF_Element_Array_Nand
           | Iir_Predefined_TF_Array_Element_Nor
           | Iir_Predefined_TF_Element_Array_Nor
           | Iir_Predefined_TF_Array_Element_Xor
           | Iir_Predefined_TF_Element_Array_Xor
           | Iir_Predefined_TF_Array_Element_Xnor
           | Iir_Predefined_TF_Element_Array_Xnor =>
            --  TODO
            raise Internal_Error;

         when Iir_Predefined_TF_Reduction_And
           | Iir_Predefined_TF_Reduction_Or
           | Iir_Predefined_TF_Reduction_Nand
           | Iir_Predefined_TF_Reduction_Nor
           | Iir_Predefined_TF_Reduction_Xor
           | Iir_Predefined_TF_Reduction_Xnor
           | Iir_Predefined_TF_Reduction_Not =>
            --  TODO
            raise Internal_Error;

         when Iir_Predefined_Bit_Array_Match_Equality
           | Iir_Predefined_Bit_Array_Match_Inequality
           | Iir_Predefined_Std_Ulogic_Array_Match_Equality
           | Iir_Predefined_Std_Ulogic_Array_Match_Inequality =>
            --  TODO
            raise Internal_Error;

         when Iir_Predefined_Explicit =>
            raise Internal_Error;
      end case;
   exception
      when Constraint_Error =>
         Warning_Msg_Sem ("arithmetic overflow in static expression", Orig);
         return Build_Overflow (Orig);
   end Eval_Dyadic_Operator;

   --  Evaluate any array attribute, return the type for the prefix.
   function Eval_Array_Attribute (Attr : Iir) return Iir
   is
      Prefix : Iir;
      Prefix_Type : Iir;
   begin
      Prefix := Get_Prefix (Attr);
      case Get_Kind (Prefix) is
         when Iir_Kinds_Object_Declaration --  FIXME: remove
           | Iir_Kind_Selected_Element
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Slice_Name
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Function_Call
           | Iir_Kind_Attribute_Value =>
            Prefix_Type := Get_Type (Prefix);
         when Iir_Kinds_Subtype_Definition =>
            Prefix_Type := Prefix;
         when Iir_Kinds_Denoting_Name =>
            Prefix_Type := Get_Type (Prefix);
         when others =>
            Error_Kind ("eval_array_attribute", Prefix);
      end case;
      if Get_Kind (Prefix_Type) /= Iir_Kind_Array_Subtype_Definition then
         Error_Kind ("eval_array_attribute(2)", Prefix_Type);
      end if;
      return Get_Nth_Element (Get_Index_Subtype_List (Prefix_Type),
                              Natural (Get_Value (Get_Parameter (Attr)) - 1));
   end Eval_Array_Attribute;

   function Eval_Integer_Image (Val : Iir_Int64; Orig : Iir) return Iir
   is
      use Str_Table;
      Img : String (1 .. 24); --  23 is enough, 24 is rounded.
      L : Natural;
      V : Iir_Int64;
      Id : String8_Id;
   begin
      V := Val;
      L := Img'Last;
      loop
         Img (L) := Character'Val (Character'Pos ('0') + abs (V rem 10));
         V := V / 10;
         L := L - 1;
         exit when V = 0;
      end loop;
      if Val < 0 then
         Img (L) := '-';
         L := L - 1;
      end if;
      Id := Create_String8;
      for I in L + 1 .. Img'Last loop
         Append_String8_Char (Img (I));
      end loop;
      return Build_String (Id, Nat32 (Img'Last - L), Orig);
   end Eval_Integer_Image;

   function Eval_Floating_Image (Val : Iir_Fp64; Orig : Iir) return Iir
   is
      use Str_Table;
      Id : String8_Id;

      --  Sign (1) + digit (1) + dot (1) + digits (15) + exp (1) + sign (1)
      --  + exp_digits (4) -> 24.
      Str : String (1 .. 25);
      P : Natural;
      V : Iir_Fp64;
      Vd : Iir_Fp64;
      Exp : Integer;
      D : Integer;
      B : Boolean;

      Res : Iir;
   begin
      --  Handle sign.
      if Val < 0.0 then
         Str (1) := '-';
         P := 1;
         V := -Val;
      else
         P := 0;
         V := Val;
      end if;

      --  Compute the mantissa.
      --  FIXME: should do a dichotomy.
      if V  = 0.0 then
         Exp := 0;
      elsif V < 1.0 then
         Exp := -1;
         while V * (10.0 ** (-Exp)) < 1.0 loop
            Exp := Exp - 1;
         end loop;
      else
         Exp := 0;
         while V / (10.0 ** Exp) >= 10.0 loop
            Exp := Exp + 1;
         end loop;
      end if;

      --  Normalize VAL: in [0; 10[
      if Exp >= 0 then
         V := V / (10.0 ** Exp);
      else
         V := V * 10.0 ** (-Exp);
      end if;

      for I in 0 .. 15 loop
         Vd := Iir_Fp64'Truncation (V);
         P := P + 1;
         Str (P) := Character'Val (48 + Integer (Vd));
         V := (V - Vd) * 10.0;

         if I = 0 then
            P := P + 1;
            Str (P) := '.';
         end if;
         exit when I > 0 and V < 10.0 ** (I + 1 - 15);
      end loop;

      if Exp /= 0 then
         --  LRM93 14.3
         --  if the exponent is present, the `e' is written as a lower case
         --  character.
         P := P + 1;
         Str (P) := 'e';

         if Exp < 0 then
            P := P + 1;
            Str (P) := '-';
            Exp := -Exp;
         end if;
         B := False;
         for I in 0 .. 4 loop
            D := (Exp / 10000) mod 10;
            if D /= 0 or B or I = 4 then
               P := P + 1;
               Str (P) := Character'Val (48 + D);
               B := True;
            end if;
            Exp := (Exp - D * 10000) * 10;
         end loop;
      end if;

      Id := Create_String8;
      for I in 1 .. P loop
         Append_String8_Char (Str (I));
      end loop;
      Res := Build_String (Id, Int32 (P), Orig);
      --  FIXME: this is not correct since the type is *not* constrained.
      Set_Type (Res, Create_Unidim_Array_By_Length
                (Get_Type (Orig), Iir_Int64 (P), Orig));
      return Res;
   end Eval_Floating_Image;

   function Eval_Enumeration_Image (Lit : Iir; Orig : Iir) return Iir
   is
      use Str_Table;
      Name : constant String := Image_Identifier (Lit);
      Image_Id : constant String8_Id := Str_Table.Create_String8;
   begin
      Append_String8_String (Name);
      return Build_String (Image_Id, Name'Length, Orig);
   end Eval_Enumeration_Image;

   function Build_Enumeration_Value (Val : String; Enum, Expr : Iir) return Iir
   is
      Value : String (Val'range);
      List  : constant Iir_List := Get_Enumeration_Literal_List (Enum);
   begin
      for I in Val'range loop
         Value (I) := Ada.Characters.Handling.To_Lower (Val (I));
      end loop;
      for I in 0 .. Get_Nbr_Elements (List) - 1 loop
         if Value = Image_Identifier (Get_Nth_Element (List, I)) then
            return Build_Enumeration (Iir_Index32 (I), Expr);
         end if;
      end loop;
      Warning_Msg_Sem ("value """ & Value & """ not in enumeration "
                         & Disp_Node (Enum), Expr);
      return Build_Overflow (Expr);
   end Build_Enumeration_Value;

   function Eval_Physical_Image (Phys, Expr: Iir) return Iir
   is
      --  Reduces to the base unit (e.g. femtoseconds).
      Value : constant String := Iir_Int64'Image (Get_Physical_Value (Phys));
      Unit : constant Iir :=
        Get_Primary_Unit (Get_Base_Type (Get_Type (Phys)));
      UnitName : constant String := Image_Identifier (Unit);
      Image_Id : constant String8_Id := Str_Table.Create_String8;
      Length : Nat32 := Value'Length + UnitName'Length + 1;
   begin
      for I in Value'range loop
         -- Suppress the Ada +ve integer'image leading space
         if I > Value'first or else Value (I) /= ' ' then
            Str_Table.Append_String8_Char (Value (I));
         else
            Length := Length - 1;
         end if;
      end loop;
      Str_Table.Append_String8_Char (' ');
      for I in UnitName'range loop
         Str_Table.Append_String8_Char (UnitName (I));
      end loop;

      return Build_String (Image_Id, Length, Expr);
   end Eval_Physical_Image;

   function Build_Physical_Value (Val: String; Phys_Type, Expr: Iir) return Iir
   is
      function White (C : in Character) return Boolean is
         NBSP : constant Character := Character'Val (160);
         HT   : constant Character := Character'Val (9);
      begin
         return C = ' ' or C = NBSP or C = HT;
      end White;

      UnitName : String (Val'range);
      Mult : Iir_Int64;
      Sep : Natural;
      Found_Unit : Boolean := false;
      Found_Real : Boolean := false;
      Unit : Iir := Get_Primary_Unit (Phys_Type);
   begin
      -- Separate string into numeric value and make lowercase unit.
      for I in reverse Val'range loop
         UnitName (I) := Ada.Characters.Handling.To_Lower (Val (I));
         if White (Val (I)) and Found_Unit then
            Sep := I;
            exit;
         else
            Found_Unit := true;
         end if;
      end loop;

      -- Unit name  is UnitName(Sep+1..Unit'Last)
      for I in Val'First .. Sep loop
         if Val (I) = '.' then
            Found_Real := true;
         end if;
      end loop;

      -- Chain down the units looking for matching one
      Unit := Get_Primary_Unit (Phys_Type);
      while Unit /= Null_Iir loop
         exit when (UnitName (Sep + 1 .. UnitName'Last)
                      = Image_Identifier (Unit));
         Unit := Get_Chain (Unit);
      end loop;
      if Unit = Null_Iir then
         Warning_Msg_Sem ("Unit """ & UnitName (Sep + 1 .. UnitName'Last)
                         & """ not in physical type", Expr);
         return Build_Overflow (Expr);
      end if;

      Mult := Get_Value (Get_Physical_Unit_Value (Unit));
      if Found_Real then
         return Build_Physical
           (Iir_Int64 (Iir_Fp64'Value (Val (Val'First .. Sep))
                         * Iir_Fp64 (Mult)),
            Expr);
      else
         return Build_Physical
           (Iir_Int64'Value (Val (Val'First .. Sep)) * Mult, Expr);
      end if;
   end Build_Physical_Value;

   function Eval_Enum_To_String (Lit : Iir; Orig : Iir) return Iir
   is
      use Str_Table;
      Id : constant Name_Id := Get_Identifier (Lit);
      Image_Id : constant String8_Id := Str_Table.Create_String8;
      Len : Natural;
   begin
      if Get_Base_Type (Get_Type (Lit)) = Character_Type_Definition then
         --  LRM08 5.7 String representations
         --  - For a given value of type CHARACTER, the string representation
         --    contains one element that is the given value.
         Append_String8 (Nat8 (Get_Enum_Pos (Lit)));
         Len := 1;
      elsif Is_Character (Id) then
         --  LRM08 5.7 String representations
         --  - For a given value of an enumeration type other than CHARACTER,
         --    if the value is a character literal, the string representation
         --    contains a single element that is the character literal; [...]
         Append_String8_Char (Get_Character (Id));
         Len := 1;
      else
         --  LRM08 5.7 String representations
         --  - [...] otherwise, the string representation is the sequence of
         --    characters in the identifier that is the given value.
         --  FIXME: extended identifier.
         Image (Id);
         if Nam_Buffer (1) /= '\' then
            Append_String8_String (Nam_Buffer (1 .. Nam_Length));
            Len := Nam_Length;
         else
            declare
               Skip : Boolean;
               C : Character;
            begin
               Len := 0;
               Skip := False;
               for I in 2 .. Nam_Length - 1 loop
                  if Skip then
                     Skip := False;
                  else
                     C := Nam_Buffer (I);
                     Append_String8_Char (C);
                     Skip := C = '\';
                     Len := Len + 1;
                  end if;
               end loop;
            end;
         end if;
      end if;
      return Build_String (Image_Id, Nat32 (Len), Orig);
   end Eval_Enum_To_String;

   function Eval_Incdec (Expr : Iir; N : Iir_Int64; Origin : Iir) return Iir
   is
      P : Iir_Int64;
   begin
      case Get_Kind (Expr) is
         when Iir_Kind_Integer_Literal =>
            return Build_Integer (Get_Value (Expr) + N, Origin);
         when Iir_Kind_Enumeration_Literal =>
            P := Iir_Int64 (Get_Enum_Pos (Expr)) + N;
            if P < 0
              or else (P >= Iir_Int64
                         (Get_Nbr_Elements
                            (Get_Enumeration_Literal_List
                               (Get_Base_Type (Get_Type (Expr))))))
            then
               Warning_Msg_Sem ("static constant violates bounds", Expr);
               return Build_Overflow (Origin);
            else
               return Build_Enumeration (Iir_Index32 (P), Origin);
            end if;
         when Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Unit_Declaration =>
            return Build_Physical (Get_Physical_Value (Expr) + N, Origin);
         when others =>
            Error_Kind ("eval_incdec", Expr);
      end case;
   end Eval_Incdec;

   function Convert_Range (Rng : Iir; Res_Type : Iir; Loc : Iir) return Iir
   is
      Res_Btype : Iir;

      function Create_Bound (Val : Iir) return Iir
      is
         R : Iir;
      begin
         R := Create_Iir (Iir_Kind_Integer_Literal);
         Location_Copy (R, Loc);
         Set_Value (R, Get_Value (Val));
         Set_Type (R, Res_Btype);
         Set_Expr_Staticness (R, Locally);
         return R;
      end Create_Bound;

      Res : Iir;
   begin
      Res_Btype := Get_Base_Type (Res_Type);
      Res := Create_Iir (Iir_Kind_Range_Expression);
      Location_Copy (Res, Loc);
      Set_Type (Res, Res_Btype);
      Set_Left_Limit (Res, Create_Bound (Get_Left_Limit (Rng)));
      Set_Right_Limit (Res, Create_Bound (Get_Right_Limit (Rng)));
      Set_Direction (Res, Get_Direction (Rng));
      Set_Expr_Staticness (Res, Locally);
      return Res;
   end Convert_Range;

   function Eval_Array_Type_Conversion (Conv : Iir; Val : Iir) return Iir
   is
      Conv_Type : constant Iir := Get_Type (Conv);
      Val_Type : constant Iir := Get_Type (Val);
      Conv_Index_Type : constant Iir := Get_Index_Type (Conv_Type, 0);
      Val_Index_Type : constant Iir := Get_Index_Type (Val_Type, 0);
      Index_Type : Iir;
      Res_Type : Iir;
      Res : Iir;
      Rng : Iir;
   begin
      --  The expression is either a simple aggregate or a (bit) string.
      Res := Build_Constant (Val, Conv);
      if Get_Constraint_State (Conv_Type) = Fully_Constrained then
         Set_Type (Res, Conv_Type);
         if not Eval_Is_In_Bound (Val, Conv_Type) then
            Warning_Msg_Sem
              ("non matching length in type conversion", Conv);
            return Build_Overflow (Conv);
         end if;
         return Res;
      else
         if Get_Base_Type (Conv_Index_Type) = Get_Base_Type (Val_Index_Type)
         then
            Index_Type := Val_Index_Type;
         else
            --  Convert the index range.
            --  It is an integer type.
            Rng := Convert_Range (Get_Range_Constraint (Val_Index_Type),
                                  Conv_Index_Type, Conv);
            Index_Type := Create_Iir (Iir_Kind_Integer_Subtype_Definition);
            Location_Copy (Index_Type, Conv);
            Set_Range_Constraint (Index_Type, Rng);
            Set_Base_Type (Index_Type, Get_Base_Type (Conv_Index_Type));
            Set_Type_Staticness (Index_Type, Locally);
         end if;
         Res_Type := Create_Unidim_Array_From_Index
           (Get_Base_Type (Conv_Type), Index_Type, Conv);
         Set_Type (Res, Res_Type);
         Set_Type_Conversion_Subtype (Conv, Res_Type);
         return Res;
      end if;
   end Eval_Array_Type_Conversion;

   function Eval_Type_Conversion (Expr : Iir) return Iir
   is
      Val : Iir;
      Val_Type : Iir;
      Conv_Type : Iir;
      Res : Iir;
   begin
      Val := Eval_Static_Expr (Get_Expression (Expr));
      Val_Type := Get_Base_Type (Get_Type (Val));
      Conv_Type := Get_Base_Type (Get_Type (Expr));
      if Conv_Type = Val_Type then
         Res := Build_Constant (Val, Expr);
      else
         case Get_Kind (Conv_Type) is
            when Iir_Kind_Integer_Type_Definition =>
               case Get_Kind (Val_Type) is
                  when Iir_Kind_Integer_Type_Definition =>
                     Res := Build_Integer (Get_Value (Val), Expr);
                  when Iir_Kind_Floating_Type_Definition =>
                     Res := Build_Integer
                       (Iir_Int64 (Get_Fp_Value (Val)), Expr);
                  when others =>
                     Error_Kind ("eval_type_conversion(1)", Val_Type);
               end case;
            when Iir_Kind_Floating_Type_Definition =>
               case Get_Kind (Val_Type) is
                  when Iir_Kind_Integer_Type_Definition =>
                     Res := Build_Floating (Iir_Fp64 (Get_Value (Val)), Expr);
                  when Iir_Kind_Floating_Type_Definition =>
                     Res := Build_Floating (Get_Fp_Value (Val), Expr);
                  when others =>
                     Error_Kind ("eval_type_conversion(2)", Val_Type);
               end case;
            when Iir_Kind_Array_Type_Definition =>
               --  Not a scalar, do not check bounds.
               return Eval_Array_Type_Conversion (Expr, Val);
            when others =>
               Error_Kind ("eval_type_conversion(3)", Conv_Type);
         end case;
      end if;
      if not Eval_Is_In_Bound (Res, Get_Type (Expr)) then
         if Get_Kind (Res) /= Iir_Kind_Overflow_Literal then
            Warning_Msg_Sem ("result of conversion out of bounds", Expr);
            Res := Build_Overflow (Res);
         end if;
      end if;
      return Res;
   end Eval_Type_Conversion;

   function Eval_Physical_Literal (Expr : Iir) return Iir
   is
      Val : Iir;
   begin
      case Get_Kind (Expr) is
         when Iir_Kind_Physical_Fp_Literal =>
            Val := Expr;
         when Iir_Kind_Physical_Int_Literal =>
            if Get_Named_Entity (Get_Unit_Name (Expr))
              = Get_Primary_Unit (Get_Base_Type (Get_Type (Expr)))
            then
               return Expr;
            else
               Val := Expr;
            end if;
         when Iir_Kind_Unit_Declaration =>
            Val := Expr;
         when Iir_Kinds_Denoting_Name =>
            Val := Get_Named_Entity (Expr);
            pragma Assert (Get_Kind (Val) = Iir_Kind_Unit_Declaration);
         when others =>
            Error_Kind ("eval_physical_literal", Expr);
      end case;
      return Build_Physical (Get_Physical_Value (Val), Expr);
   end Eval_Physical_Literal;

   function Eval_Static_Expr (Expr: Iir) return Iir
   is
      Res : Iir;
      Val : Iir;
   begin
      case Get_Kind (Expr) is
         when Iir_Kinds_Denoting_Name =>
            return Eval_Static_Expr (Get_Named_Entity (Expr));

         when Iir_Kind_Integer_Literal
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Floating_Point_Literal
           | Iir_Kind_String_Literal8
           | Iir_Kind_Overflow_Literal
           | Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal =>
            return Expr;
         when Iir_Kind_Constant_Declaration =>
            Val := Eval_Static_Expr (Get_Default_Value (Expr));
            --  Type of the expression should be type of the constant
            --  declaration at least in case of array subtype.
            --  If the constant is declared as an unconstrained array, get type
            --  from the default value.
            --  FIXME: handle this during semantisation of the declaration:
            --    add an implicit subtype conversion node ?
            --  FIXME: this currently creates a node at each evalation.
            if Get_Kind (Get_Type (Val)) = Iir_Kind_Array_Type_Definition then
               Res := Build_Constant (Val, Expr);
               Set_Type (Res, Get_Type (Val));
               return Res;
            else
               return Val;
            end if;
         when Iir_Kind_Object_Alias_Declaration =>
            return Eval_Static_Expr (Get_Name (Expr));
         when Iir_Kind_Unit_Declaration =>
            return Get_Physical_Unit_Value (Expr);
         when Iir_Kind_Simple_Aggregate =>
            return Expr;

         when Iir_Kind_Parenthesis_Expression =>
            return Eval_Static_Expr (Get_Expression (Expr));
         when Iir_Kind_Qualified_Expression =>
            return Eval_Static_Expr (Get_Expression (Expr));
         when Iir_Kind_Type_Conversion =>
            return Eval_Type_Conversion (Expr);

         when Iir_Kinds_Monadic_Operator =>
            declare
               Operand : Iir;
            begin
               Operand := Eval_Static_Expr (Get_Operand (Expr));
               return Eval_Monadic_Operator (Expr, Operand);
            end;
         when Iir_Kinds_Dyadic_Operator =>
            declare
               Left : constant Iir := Get_Left (Expr);
               Right : constant Iir := Get_Right (Expr);
               Left_Val, Right_Val : Iir;
               Res : Iir;
            begin
               Left_Val := Eval_Static_Expr (Left);
               Right_Val := Eval_Static_Expr (Right);

               Res := Eval_Dyadic_Operator
                 (Expr, Get_Implementation (Expr), Left_Val, Right_Val);

               Free_Eval_Static_Expr (Left_Val, Left);
               Free_Eval_Static_Expr (Right_Val, Right);

               return Res;
            end;

         when Iir_Kind_Attribute_Name =>
            --  An attribute name designates an attribute value.
            declare
               Attr_Val : constant Iir := Get_Named_Entity (Expr);
               Attr_Expr : constant Iir :=
                 Get_Expression (Get_Attribute_Specification (Attr_Val));
               Val : Iir;
            begin
               Val := Eval_Static_Expr (Attr_Expr);
               --  FIXME: see constant_declaration.
               --  Currently, this avoids weird nodes, such as a string literal
               --  whose type is an unconstrained array type.
               Res := Build_Constant (Val, Expr);
               Set_Type (Res, Get_Type (Val));
               return Res;
            end;

         when Iir_Kind_Pos_Attribute =>
            declare
               Param : constant Iir := Get_Parameter (Expr);
               Val : Iir;
               Res : Iir;
            begin
               Val := Eval_Static_Expr (Param);
               --  FIXME: check bounds, handle overflow.
               Res := Build_Integer (Eval_Pos (Val), Expr);
               Free_Eval_Static_Expr (Val, Param);
               return Res;
            end;
         when Iir_Kind_Val_Attribute =>
            declare
               Expr_Type : constant Iir := Get_Type (Expr);
               Val_Expr : Iir;
               Val : Iir_Int64;
            begin
               Val_Expr := Eval_Static_Expr (Get_Parameter (Expr));
               Val := Eval_Pos (Val_Expr);
               --  Note: the type of 'val is a base type.
               --  FIXME: handle VHDL93 restrictions.
               if Get_Kind (Expr_Type) = Iir_Kind_Enumeration_Type_Definition
                 and then
                 not Eval_Int_In_Range (Val, Get_Range_Constraint (Expr_Type))
               then
                  Warning_Msg_Sem
                    ("static argument out of the type range", Expr);
                  return Build_Overflow (Expr);
               end if;
               if Get_Kind (Get_Base_Type (Get_Type (Expr)))
                 = Iir_Kind_Physical_Type_Definition
               then
                  return Build_Physical (Val, Expr);
               else
                  return Build_Discrete (Val, Expr);
               end if;
            end;
         when Iir_Kind_Image_Attribute =>
            declare
               Param : Iir;
               Param_Type : Iir;
            begin
               Param := Get_Parameter (Expr);
               Param := Eval_Static_Expr (Param);
               Set_Parameter (Expr, Param);

               --  Special case for overflow.
               if Get_Kind (Param) = Iir_Kind_Overflow_Literal then
                  return Build_Overflow (Expr);
               end if;

               Param_Type := Get_Base_Type (Get_Type (Param));
               case Get_Kind (Param_Type) is
                  when Iir_Kind_Integer_Type_Definition =>
                     return Eval_Integer_Image (Get_Value (Param), Expr);
                  when Iir_Kind_Floating_Type_Definition =>
                     return Eval_Floating_Image (Get_Fp_Value (Param), Expr);
                  when Iir_Kind_Enumeration_Type_Definition =>
                     return Eval_Enumeration_Image (Param, Expr);
                  when Iir_Kind_Physical_Type_Definition =>
                     return Eval_Physical_Image (Param, Expr);
                  when others =>
                     Error_Kind ("eval_static_expr('image)", Param);
               end case;
            end;
         when Iir_Kind_Value_Attribute =>
            declare
               Param : Iir;
               Param_Type : Iir;
            begin
               Param := Get_Parameter (Expr);
               Param := Eval_Static_Expr (Param);
               Set_Parameter (Expr, Param);
               if Get_Kind (Param) /= Iir_Kind_String_Literal8 then
                  --  FIXME: Isn't it an implementation restriction.
                  Warning_Msg_Sem ("'value argument not a string", Expr);
                  return Build_Overflow (Expr);
               else
                  -- what type are we converting the string to?
                  Param_Type := Get_Base_Type (Get_Type (Expr));
                  declare
                     Value : constant String := Image_String_Lit (Param);
                  begin
                     case Get_Kind (Param_Type) is
                     when Iir_Kind_Integer_Type_Definition =>
                        return Build_Discrete (Iir_Int64'Value (Value), Expr);
                     when Iir_Kind_Enumeration_Type_Definition =>
                        return Build_Enumeration_Value (Value, Param_Type,
                                                        Expr);
                     when Iir_Kind_Floating_Type_Definition =>
                        return Build_Floating (Iir_Fp64'value (Value), Expr);
                     when Iir_Kind_Physical_Type_Definition =>
                        return Build_Physical_Value (Value, Param_Type, Expr);
                     when others =>
                        Error_Kind ("eval_static_expr('value)", Param);
                     end case;
                  end;
               end if;
            end;

         when Iir_Kind_Left_Type_Attribute =>
            return Eval_Static_Expr
              (Get_Left_Limit (Eval_Static_Range (Get_Prefix (Expr))));
         when Iir_Kind_Right_Type_Attribute =>
            return Eval_Static_Expr
              (Get_Right_Limit (Eval_Static_Range (Get_Prefix (Expr))));
         when Iir_Kind_High_Type_Attribute =>
            return Eval_Static_Expr
              (Get_High_Limit (Eval_Static_Range (Get_Prefix (Expr))));
         when Iir_Kind_Low_Type_Attribute =>
            return Eval_Static_Expr
              (Get_Low_Limit (Eval_Static_Range (Get_Prefix (Expr))));
         when Iir_Kind_Ascending_Type_Attribute =>
            return Build_Boolean
              (Get_Direction (Eval_Static_Range (Get_Prefix (Expr))) = Iir_To);

         when Iir_Kind_Length_Array_Attribute =>
            declare
               Index : Iir;
            begin
               Index := Eval_Array_Attribute (Expr);
               return Build_Discrete (Eval_Discrete_Type_Length (Index), Expr);
            end;
         when Iir_Kind_Left_Array_Attribute =>
            declare
               Index : Iir;
            begin
               Index := Eval_Array_Attribute (Expr);
               return Eval_Static_Expr
                 (Get_Left_Limit (Get_Range_Constraint (Index)));
            end;
         when Iir_Kind_Right_Array_Attribute =>
            declare
               Index : Iir;
            begin
               Index := Eval_Array_Attribute (Expr);
               return Eval_Static_Expr
                 (Get_Right_Limit (Get_Range_Constraint (Index)));
            end;
         when Iir_Kind_Low_Array_Attribute =>
            declare
               Index : Iir;
            begin
               Index := Eval_Array_Attribute (Expr);
               return Eval_Static_Expr
                 (Get_Low_Limit (Get_Range_Constraint (Index)));
            end;
         when Iir_Kind_High_Array_Attribute =>
            declare
               Index : Iir;
            begin
               Index := Eval_Array_Attribute (Expr);
               return Eval_Static_Expr
                 (Get_High_Limit (Get_Range_Constraint (Index)));
            end;
         when Iir_Kind_Ascending_Array_Attribute =>
            declare
               Index : Iir;
            begin
               Index := Eval_Array_Attribute (Expr);
               return Build_Boolean
                 (Get_Direction (Get_Range_Constraint (Index)) = Iir_To);
            end;

         when Iir_Kind_Pred_Attribute =>
            Res := Eval_Incdec
              (Eval_Static_Expr (Get_Parameter (Expr)), -1, Expr);
            Eval_Check_Bound (Res, Get_Type (Get_Prefix (Expr)));
            return Res;
         when Iir_Kind_Succ_Attribute =>
            Res := Eval_Incdec
              (Eval_Static_Expr (Get_Parameter (Expr)), +1, Expr);
            Eval_Check_Bound (Res, Get_Type (Get_Prefix (Expr)));
            return Res;
         when Iir_Kind_Leftof_Attribute
           | Iir_Kind_Rightof_Attribute =>
            declare
               Rng : Iir;
               N : Iir_Int64;
               Prefix_Type : constant Iir := Get_Type (Get_Prefix (Expr));
               Res : Iir;
            begin
               Rng := Eval_Static_Range (Prefix_Type);
               case Get_Direction (Rng) is
                  when Iir_To =>
                     N := 1;
                  when Iir_Downto =>
                     N := -1;
               end case;
               case Get_Kind (Expr) is
                  when Iir_Kind_Leftof_Attribute =>
                     N := -N;
                  when Iir_Kind_Rightof_Attribute =>
                     null;
                  when others =>
                     raise Internal_Error;
               end case;
               Res := Eval_Incdec
                 (Eval_Static_Expr (Get_Parameter (Expr)), N, Expr);
               Eval_Check_Bound (Res, Prefix_Type);
               return Res;
            end;

         when Iir_Kind_Simple_Name_Attribute =>
            declare
               use Str_Table;
               Id : String8_Id;
            begin
               Id := Create_String8;
               Image (Get_Simple_Name_Identifier (Expr));
               for I in 1 .. Nam_Length loop
                  Append_String8_Char (Nam_Buffer (I));
               end loop;
               return Build_String (Id, Nat32 (Nam_Length), Expr);
            end;

         when Iir_Kind_Null_Literal =>
            return Expr;

         when Iir_Kind_Function_Call =>
            declare
               Imp : constant Iir := Get_Implementation (Expr);
               Left, Right : Iir;
            begin
               --  Note: there can't be association by name.
               Left := Get_Parameter_Association_Chain (Expr);
               Right := Get_Chain (Left);

               Left := Eval_Static_Expr (Get_Actual (Left));
               if Right = Null_Iir then
                  return Eval_Monadic_Operator (Expr, Left);
               else
                  Right := Eval_Static_Expr (Get_Actual (Right));
                  return Eval_Dyadic_Operator (Expr, Imp, Left, Right);
               end if;
            end;

         when Iir_Kind_Error =>
            return Expr;
         when others =>
            Error_Kind ("eval_static_expr", Expr);
      end case;
   end Eval_Static_Expr;

   --  If FORCE is true, always return a literal.
   function Eval_Expr_Keep_Orig (Expr : Iir; Force : Boolean) return Iir
   is
      Res : Iir;
   begin
      case Get_Kind (Expr) is
         when Iir_Kinds_Denoting_Name =>
            declare
               Orig : constant Iir := Get_Named_Entity (Expr);
            begin
               Res := Eval_Static_Expr (Orig);
               if Res /= Orig or else Force then
                  return Build_Constant (Res, Expr);
               else
                  return Expr;
               end if;
            end;
         when others =>
            Res := Eval_Static_Expr (Expr);
            if Res /= Expr
              and then Get_Literal_Origin (Res) /= Expr
            then
               --  Need to build a constant if the result is a different
               --  literal not tied to EXPR.
               return Build_Constant (Res, Expr);
            else
               return Res;
            end if;
      end case;
   end Eval_Expr_Keep_Orig;

   function Eval_Expr (Expr: Iir) return Iir is
   begin
      if Get_Expr_Staticness (Expr) /= Locally then
         Error_Msg_Sem ("expression must be locally static", Expr);
         return Expr;
      else
         return Eval_Expr_Keep_Orig (Expr, False);
      end if;
   end Eval_Expr;

   function Eval_Expr_If_Static (Expr : Iir) return Iir is
   begin
      if Expr /= Null_Iir and then Get_Expr_Staticness (Expr) = Locally then
         return Eval_Expr_Keep_Orig (Expr, False);
      else
         return Expr;
      end if;
   end Eval_Expr_If_Static;

   function Eval_Expr_Check (Expr : Iir; Sub_Type : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Eval_Expr_Keep_Orig (Expr, False);
      Eval_Check_Bound (Res, Sub_Type);
      return Res;
   end Eval_Expr_Check;

   function Eval_Expr_Check_If_Static (Expr : Iir; Atype : Iir) return Iir
   is
      Res : Iir;
   begin
      if Expr /= Null_Iir and then Get_Expr_Staticness (Expr) = Locally then
         --  Expression is static and can be evaluated.
         Res := Eval_Expr_Keep_Orig (Expr, False);

         if Res /= Null_Iir
           and then Get_Type_Staticness (Atype) = Locally
           and then Get_Kind (Atype) in Iir_Kinds_Range_Type_Definition
         then
            --  Check bounds (as this can be done).
            --  FIXME: create overflow_expr ?
            Eval_Check_Bound (Res, Atype);
         end if;

         return Res;
      else
         return Expr;
      end if;
   end Eval_Expr_Check_If_Static;

   function Eval_Int_In_Range (Val : Iir_Int64; Bound : Iir) return Boolean is
   begin
      case Get_Kind (Bound) is
         when Iir_Kind_Range_Expression =>
            case Get_Direction (Bound) is
               when Iir_To =>
                  if Val < Eval_Pos (Get_Left_Limit (Bound))
                    or else Val > Eval_Pos (Get_Right_Limit (Bound))
                  then
                     return False;
                  end if;
               when Iir_Downto =>
                  if Val > Eval_Pos (Get_Left_Limit (Bound))
                    or else Val < Eval_Pos (Get_Right_Limit (Bound))
                  then
                     return False;
                  end if;
            end case;
         when others =>
            Error_Kind ("eval_int_in_range", Bound);
      end case;
      return True;
   end Eval_Int_In_Range;

   function Eval_Phys_In_Range (Val : Iir_Int64; Bound : Iir) return Boolean
   is
      Left, Right : Iir_Int64;
   begin
      case Get_Kind (Bound) is
         when Iir_Kind_Range_Expression =>
            case Get_Kind (Get_Type (Get_Left_Limit (Bound))) is
               when Iir_Kind_Integer_Type_Definition
                 | Iir_Kind_Integer_Subtype_Definition =>
                  Left := Get_Value (Get_Left_Limit (Bound));
                  Right := Get_Value (Get_Right_Limit (Bound));
               when Iir_Kind_Physical_Type_Definition
                 | Iir_Kind_Physical_Subtype_Definition =>
                  Left := Get_Physical_Value (Get_Left_Limit (Bound));
                  Right := Get_Physical_Value (Get_Right_Limit (Bound));
               when others =>
                  Error_Kind ("eval_phys_in_range(1)", Get_Type (Bound));
            end case;
            case Get_Direction (Bound) is
               when Iir_To =>
                  if Val < Left or else Val > Right then
                     return False;
                  end if;
               when Iir_Downto =>
                  if Val > Left or else Val < Right then
                     return False;
                  end if;
            end case;
         when others =>
            Error_Kind ("eval_phys_in_range", Bound);
      end case;
      return True;
   end Eval_Phys_In_Range;

   function Eval_Fp_In_Range (Val : Iir_Fp64; Bound : Iir) return Boolean is
   begin
      case Get_Kind (Bound) is
         when Iir_Kind_Range_Expression =>
            case Get_Direction (Bound) is
               when Iir_To =>
                  if Val < Get_Fp_Value (Get_Left_Limit (Bound))
                    or else Val > Get_Fp_Value (Get_Right_Limit (Bound))
                  then
                     return False;
                  end if;
               when Iir_Downto =>
                  if Val > Get_Fp_Value (Get_Left_Limit (Bound))
                    or else Val < Get_Fp_Value (Get_Right_Limit (Bound))
                  then
                     return False;
                  end if;
            end case;
         when others =>
            Error_Kind ("eval_fp_in_range", Bound);
      end case;
      return True;
   end Eval_Fp_In_Range;

   --  Return FALSE if literal EXPR is not in SUB_TYPE bounds.
   function Eval_Is_In_Bound (Expr : Iir; Sub_Type : Iir) return Boolean
   is
      Type_Range : Iir;
      Val : Iir;
   begin
      case Get_Kind (Expr) is
         when Iir_Kind_Error =>
            --  Ignore errors.
            return True;
         when Iir_Kind_Overflow_Literal =>
            --  Never within bounds
            return False;
         when Iir_Kind_Simple_Name
           | Iir_Kind_Character_Literal
           | Iir_Kind_Selected_Name =>
            Val := Get_Named_Entity (Expr);
         when others =>
            Val := Expr;
      end case;

      case Get_Kind (Sub_Type) is
         when Iir_Kind_Integer_Subtype_Definition =>
            if Get_Expr_Staticness (Expr) /= Locally
              or else Get_Type_Staticness (Sub_Type) /= Locally
            then
               return True;
            end if;
            Type_Range := Get_Range_Constraint (Sub_Type);
            return Eval_Int_In_Range (Get_Value (Val), Type_Range);
         when Iir_Kind_Floating_Subtype_Definition =>
            if Get_Expr_Staticness (Expr) /= Locally
              or else Get_Type_Staticness (Sub_Type) /= Locally
            then
               return True;
            end if;
            Type_Range := Get_Range_Constraint (Sub_Type);
            return Eval_Fp_In_Range (Get_Fp_Value (Val), Type_Range);
         when Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition =>
            if Get_Expr_Staticness (Expr) /= Locally
              or else Get_Type_Staticness (Sub_Type) /= Locally
            then
               return True;
            end if;
            --  A check is required for an enumeration type definition for
            --  'val attribute.
            Type_Range := Get_Range_Constraint (Sub_Type);
            return Eval_Int_In_Range
              (Iir_Int64 (Get_Enum_Pos (Val)), Type_Range);
         when Iir_Kind_Physical_Subtype_Definition =>
            if Get_Expr_Staticness (Expr) /= Locally
              or else Get_Type_Staticness (Sub_Type) /= Locally
            then
               return True;
            end if;
            Type_Range := Get_Range_Constraint (Sub_Type);
            return Eval_Phys_In_Range (Get_Physical_Value (Val), Type_Range);

         when Iir_Kind_Base_Attribute =>
            if Get_Expr_Staticness (Expr) /= Locally
              or else Get_Type_Staticness (Sub_Type) /= Locally
            then
               return True;
            end if;
            return Eval_Is_In_Bound (Val, Get_Type (Sub_Type));

         when Iir_Kind_Array_Subtype_Definition =>
            declare
               Val_Type : constant Iir := Get_Type (Val);
            begin
               if Get_Constraint_State (Sub_Type) /= Fully_Constrained
                 or else
                 Get_Kind (Val_Type) /= Iir_Kind_Array_Subtype_Definition
                 or else
                 Get_Constraint_State (Val_Type) /= Fully_Constrained
               then
                  --  Cannot say no.
                  return True;
               end if;
               declare
                  E_Indexes : constant Iir_List :=
                    Get_Index_Subtype_List (Val_Type);
                  T_Indexes : constant Iir_List :=
                    Get_Index_Subtype_List (Sub_Type);
                  E_El : Iir;
                  T_El : Iir;
               begin
                  for I in Natural loop
                     E_El := Get_Index_Type (E_Indexes, I);
                     T_El := Get_Index_Type (T_Indexes, I);
                     exit when E_El = Null_Iir and T_El = Null_Iir;

                     if Get_Type_Staticness (E_El) = Locally
                       and then Get_Type_Staticness (T_El) = Locally
                       and then (Eval_Discrete_Type_Length (E_El)
                                   /= Eval_Discrete_Type_Length (T_El))
                     then
                        return False;
                     end if;
                  end loop;
                  return True;
               end;
            end;

         when Iir_Kind_Access_Type_Definition
           | Iir_Kind_Access_Subtype_Definition =>
            return True;

         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            --  FIXME: do it.
            return True;

         when others =>
            Error_Kind ("eval_is_in_bound", Sub_Type);
      end case;
   end Eval_Is_In_Bound;

   procedure Eval_Check_Bound (Expr : Iir; Sub_Type : Iir) is
   begin
      if Get_Kind (Expr) = Iir_Kind_Overflow_Literal then
         --  Nothing to check, and a message was already generated.
         return;
      end if;

      if not Eval_Is_In_Bound (Expr, Sub_Type) then
         Error_Msg_Sem ("static constant violates bounds", Expr);
      end if;
   end Eval_Check_Bound;

   function Eval_Is_Range_In_Bound
     (A_Range : Iir; Sub_Type : Iir; Any_Dir : Boolean)
     return Boolean
   is
      Type_Range : Iir;
      Range_Constraint : constant Iir := Eval_Static_Range (A_Range);
   begin
      Type_Range := Get_Range_Constraint (Sub_Type);
      if not Any_Dir
        and then Get_Direction (Type_Range) /= Get_Direction (Range_Constraint)
      then
         return True;
      end if;

      case Get_Kind (Sub_Type) is
         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition =>
            declare
               L, R : Iir_Int64;
            begin
               --  Check for null range.
               L := Eval_Pos (Get_Left_Limit (Range_Constraint));
               R := Eval_Pos (Get_Right_Limit (Range_Constraint));
               case Get_Direction (Range_Constraint) is
                  when Iir_To =>
                     if L > R then
                        return True;
                     end if;
                  when Iir_Downto =>
                     if L < R then
                        return True;
                     end if;
               end case;
               return Eval_Int_In_Range (L, Type_Range)
                 and then Eval_Int_In_Range (R, Type_Range);
            end;
         when Iir_Kind_Floating_Subtype_Definition =>
            declare
               L, R : Iir_Fp64;
            begin
               --  Check for null range.
               L := Get_Fp_Value (Get_Left_Limit (Range_Constraint));
               R := Get_Fp_Value (Get_Right_Limit (Range_Constraint));
               case Get_Direction (Range_Constraint) is
                  when Iir_To =>
                     if L > R then
                        return True;
                     end if;
                  when Iir_Downto =>
                     if L < R then
                        return True;
                     end if;
               end case;
               return Eval_Fp_In_Range (L, Type_Range)
                 and then Eval_Fp_In_Range (R, Type_Range);
            end;
         when others =>
            Error_Kind ("eval_is_range_in_bound", Sub_Type);
      end case;

      --  Should check L <= R or L >= R according to direction.
      --return Eval_Is_In_Bound (Get_Left_Limit (A_Range), Sub_Type)
      --  and then Eval_Is_In_Bound (Get_Right_Limit (A_Range), Sub_Type);
   end Eval_Is_Range_In_Bound;

   procedure Eval_Check_Range
     (A_Range : Iir; Sub_Type : Iir; Any_Dir : Boolean)
   is
   begin
      if not Eval_Is_Range_In_Bound (A_Range, Sub_Type, Any_Dir) then
         Error_Msg_Sem ("static range violates bounds", A_Range);
      end if;
   end Eval_Check_Range;

   function Eval_Discrete_Range_Length (Constraint : Iir) return Iir_Int64
   is
      --  We don't want to deal with very large ranges here.
      pragma Suppress (Overflow_Check);
      Res : Iir_Int64;
      Left, Right : Iir_Int64;
   begin
      Left := Eval_Pos (Get_Left_Limit (Constraint));
      Right := Eval_Pos (Get_Right_Limit (Constraint));
      case Get_Direction (Constraint) is
         when Iir_To =>
            if Right < Left then
               --  Null range.
               return 0;
            else
               Res := Right - Left + 1;
            end if;
         when Iir_Downto =>
            if Left < Right then
               --  Null range
               return 0;
            else
               Res := Left - Right + 1;
            end if;
      end case;
      return Res;
   end Eval_Discrete_Range_Length;

   function Eval_Discrete_Type_Length (Sub_Type : Iir) return Iir_Int64
   is
   begin
      case Get_Kind (Sub_Type) is
         when Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Subtype_Definition =>
            return Eval_Discrete_Range_Length
              (Get_Range_Constraint (Sub_Type));
         when others =>
            Error_Kind ("eval_discrete_type_length", Sub_Type);
      end case;
   end Eval_Discrete_Type_Length;

   function Eval_Pos (Expr : Iir) return Iir_Int64 is
   begin
      case Get_Kind (Expr) is
         when Iir_Kind_Integer_Literal =>
            return Get_Value (Expr);
         when Iir_Kind_Enumeration_Literal =>
            return Iir_Int64 (Get_Enum_Pos (Expr));
         when Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal
           | Iir_Kind_Unit_Declaration =>
            return Get_Physical_Value (Expr);
         when Iir_Kinds_Denoting_Name =>
            return Eval_Pos (Get_Named_Entity (Expr));
         when others =>
            Error_Kind ("eval_pos", Expr);
      end case;
   end Eval_Pos;

   function Eval_Static_Range (Rng : Iir) return Iir
   is
      Expr : Iir;
      Kind : Iir_Kind;
   begin
      Expr := Rng;
      loop
         Kind := Get_Kind (Expr);
         case Kind is
            when Iir_Kind_Range_Expression =>
               if Get_Expr_Staticness (Expr) /= Locally then
                  return Null_Iir;
               end if;

               --  Normalize the range expression.
               Set_Left_Limit
                 (Expr, Eval_Expr_Keep_Orig (Get_Left_Limit (Expr), True));
               Set_Right_Limit
                 (Expr, Eval_Expr_Keep_Orig (Get_Right_Limit (Expr), True));
               return Expr;
            when Iir_Kind_Integer_Subtype_Definition
              | Iir_Kind_Floating_Subtype_Definition
              | Iir_Kind_Enumeration_Type_Definition
              | Iir_Kind_Enumeration_Subtype_Definition
              | Iir_Kind_Physical_Subtype_Definition =>
               Expr := Get_Range_Constraint (Expr);
            when Iir_Kind_Range_Array_Attribute
              | Iir_Kind_Reverse_Range_Array_Attribute =>
               declare
                  Prefix : Iir;
                  Res : Iir;
               begin
                  Prefix := Get_Prefix (Expr);
                  if Get_Kind (Prefix) /= Iir_Kind_Array_Subtype_Definition
                  then
                     Prefix := Get_Type (Prefix);
                  end if;
                  if Get_Kind (Prefix) /= Iir_Kind_Array_Subtype_Definition
                  then
                     --  Unconstrained object.
                     return Null_Iir;
                  end if;
                  Expr := Get_Nth_Element
                    (Get_Index_Subtype_List (Prefix),
                     Natural (Eval_Pos (Get_Parameter (Expr))) - 1);
                  if Kind = Iir_Kind_Reverse_Range_Array_Attribute then
                     Expr := Eval_Static_Range (Expr);

                     Res := Create_Iir (Iir_Kind_Range_Expression);
                     Location_Copy (Res, Expr);
                     Set_Type (Res, Get_Type (Expr));
                     case Get_Direction (Expr) is
                        when Iir_To =>
                           Set_Direction (Res, Iir_Downto);
                        when Iir_Downto =>
                           Set_Direction (Res, Iir_To);
                     end case;
                     Set_Left_Limit (Res, Get_Right_Limit (Expr));
                     Set_Right_Limit (Res, Get_Left_Limit (Expr));
                     Set_Range_Origin (Res, Rng);
                     Set_Expr_Staticness (Res, Get_Expr_Staticness (Expr));
                     return Res;
                  end if;
               end;

            when Iir_Kind_Subtype_Declaration
              | Iir_Kind_Base_Attribute =>
               Expr := Get_Type (Expr);
            when Iir_Kind_Type_Declaration =>
               Expr := Get_Type_Definition (Expr);
            when Iir_Kind_Simple_Name
              | Iir_Kind_Selected_Name =>
               Expr := Get_Named_Entity (Expr);
            when others =>
               Error_Kind ("eval_static_range", Expr);
         end case;
      end loop;
   end Eval_Static_Range;

   function Eval_Range (Arange : Iir) return Iir is
      Res : Iir;
   begin
      Res := Eval_Static_Range (Arange);
      if Res /= Arange
        and then Get_Range_Origin (Res) /= Arange
      then
         return Build_Constant_Range (Res, Arange);
      else
         return Res;
      end if;
   end Eval_Range;

   function Eval_Range_If_Static (Arange : Iir) return Iir is
   begin
      if Get_Expr_Staticness (Arange) /= Locally then
         return Arange;
      else
         return Eval_Range (Arange);
      end if;
   end Eval_Range_If_Static;

   --  Return the range constraint of a discrete range.
   function Eval_Discrete_Range_Expression (Constraint : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Eval_Static_Range (Constraint);
      if Res = Null_Iir then
         Error_Kind ("eval_discrete_range_expression", Constraint);
      else
         return Res;
      end if;
   end Eval_Discrete_Range_Expression;

   function Eval_Discrete_Range_Left (Constraint : Iir) return Iir
   is
      Range_Expr : Iir;
   begin
      Range_Expr := Eval_Discrete_Range_Expression (Constraint);
      return Get_Left_Limit (Range_Expr);
   end Eval_Discrete_Range_Left;

   procedure Eval_Operator_Symbol_Name (Id : Name_Id)
   is
   begin
      Image (Id);
      Nam_Buffer (2 .. Nam_Length + 1) := Nam_Buffer (1 .. Nam_Length);
      Nam_Buffer (1) := '"'; --"
      Nam_Length := Nam_Length + 2;
      Nam_Buffer (Nam_Length) := '"'; --"
   end Eval_Operator_Symbol_Name;

   procedure Eval_Simple_Name (Id : Name_Id)
   is
   begin
      --  LRM 14.1
      --  E'SIMPLE_NAME
      --    Result: [...] but with apostrophes (in the case of a character
      --            literal)
      if Is_Character (Id) then
         Nam_Buffer (1) := ''';
         Nam_Buffer (2) := Get_Character (Id);
         Nam_Buffer (3) := ''';
         Nam_Length := 3;
         return;
      end if;
      case Id is
         when Std_Names.Name_Word_Operators
           | Std_Names.Name_First_Operator .. Std_Names.Name_Last_Operator =>
            Eval_Operator_Symbol_Name (Id);
            return;
         when Std_Names.Name_Xnor
           | Std_Names.Name_Shift_Operators =>
            if Flags.Vhdl_Std > Vhdl_87 then
               Eval_Operator_Symbol_Name (Id);
               return;
            end if;
         when others =>
            null;
      end case;
      Image (Id);
--       if Name_Buffer (1) = '\' then
--          declare
--             I : Natural;
--          begin
--             I := 2;
--             while I <= Name_Length loop
--                if Name_Buffer (I) = '\' then
--                   Name_Length := Name_Length + 1;
--                   Name_Buffer (I + 1 .. Name_Length) :=
--                     Name_Buffer (I .. Name_Length - 1);
--                   I := I + 1;
--                end if;
--                I := I + 1;
--             end loop;
--             Name_Length := Name_Length + 1;
--             Name_Buffer (Name_Length) := '\';
--          end;
--       end if;
   end Eval_Simple_Name;

   function Compare_String_Literals (L, R : Iir) return Compare_Type
   is
      type Str_Info is record
         El : Iir;
         Id : String8_Id;
         Len : Nat32;
         List : Iir_List;
      end record;

      Literal_List : Iir_List;

      --  Fill Res from EL.  This is used to speed up Lt and Eq operations.
      procedure Get_Info (Expr : Iir; Res : out Str_Info) is
      begin
         case Get_Kind (Expr) is
            when Iir_Kind_Simple_Aggregate =>
               Res := Str_Info'(El => Expr,
                                Id => Null_String8,
                                Len => 0,
                                List => Get_Simple_Aggregate_List (Expr));
               Res.Len := Nat32 (Get_Nbr_Elements (Res.List));
            when Iir_Kind_String_Literal8 =>
               Res := Str_Info'(El => Expr,
                                Id => Get_String8_Id (Expr),
                                Len => Get_String_Length (Expr),
                                List => Null_Iir_List);
            when others =>
               Error_Kind ("sem_string_choice_range.get_info", Expr);
         end case;
      end Get_Info;

      --  Return the position of element IDX of STR.
      function Get_Pos (Str : Str_Info; Idx : Nat32) return Iir_Int32
      is
         S : Iir;
         P : Nat32;
      begin
         case Get_Kind (Str.El) is
            when Iir_Kind_Simple_Aggregate =>
               S := Get_Nth_Element (Str.List, Natural (Idx));
            when Iir_Kind_String_Literal8 =>
               P := Str_Table.Element_String8 (Str.Id, Idx + 1);
               S := Get_Nth_Element (Literal_List, Natural (P));
            when others =>
               Error_Kind ("sem_string_choice_range.get_pos", Str.El);
         end case;
         return Get_Enum_Pos (S);
      end Get_Pos;

      L_Info, R_Info : Str_Info;
      L_Pos, R_Pos : Iir_Int32;
   begin
      Get_Info (L, L_Info);
      Get_Info (R, R_Info);

      if L_Info.Len /= R_Info.Len then
         raise Internal_Error;
      end if;

      Literal_List := Get_Enumeration_Literal_List
        (Get_Base_Type (Get_Element_Subtype (Get_Type (L))));

      for I in 0 .. L_Info.Len - 1 loop
         L_Pos := Get_Pos (L_Info, I);
         R_Pos := Get_Pos (R_Info, I);
         if L_Pos /= R_Pos then
            if L_Pos < R_Pos then
               return Compare_Lt;
            else
               return Compare_Gt;
            end if;
         end if;
      end loop;
      return Compare_Eq;
   end Compare_String_Literals;

   function Get_Path_Instance_Name_Suffix (Attr : Iir)
                                          return Path_Instance_Name_Type
   is
      --  Current path for name attributes.
      Path_Str : String_Acc := null;
      Path_Maxlen : Natural := 0;
      Path_Len : Natural;
      Path_Instance : Iir;

      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Name => String_Acc, Object => String);

      procedure Path_Reset is
      begin
         Path_Len := 0;
         Path_Instance := Null_Iir;
         if Path_Maxlen = 0 then
            Path_Maxlen := 256;
            Path_Str := new String (1 .. Path_Maxlen);
         end if;
      end Path_Reset;

      procedure Path_Add (Str : String)
      is
         N_Len : Natural;
         N_Path : String_Acc;
      begin
         N_Len := Path_Maxlen;
         loop
            exit when Path_Len + Str'Length <= N_Len;
            N_Len := N_Len * 2;
         end loop;
         if N_Len /= Path_Maxlen then
            N_Path := new String (1 .. N_Len);
            N_Path (1 .. Path_Len) := Path_Str (1 .. Path_Len);
            Deallocate (Path_Str);
            Path_Str := N_Path;
            Path_Maxlen := N_Len;
         end if;
         Path_Str (Path_Len + 1 .. Path_Len + Str'Length) := Str;
         Path_Len := Path_Len + Str'Length;
      end Path_Add;

      procedure Path_Add_Type_Name (Atype : Iir)
      is
         Adecl : Iir;
      begin
         Adecl := Get_Type_Declarator (Atype);
         Image (Get_Identifier (Adecl));
         Path_Add (Nam_Buffer (1 .. Nam_Length));
      end Path_Add_Type_Name;

      procedure Path_Add_Signature (Subprg : Iir)
      is
         Chain : Iir;
      begin
         Path_Add ("[");
         Chain := Get_Interface_Declaration_Chain (Subprg);
         while Chain /= Null_Iir loop
            Path_Add_Type_Name (Get_Type (Chain));
            Chain := Get_Chain (Chain);
            if Chain /= Null_Iir then
               Path_Add (",");
            end if;
         end loop;

         case Get_Kind (Subprg) is
            when Iir_Kind_Function_Declaration =>
               Path_Add (" return ");
               Path_Add_Type_Name (Get_Return_Type (Subprg));
            when others =>
               null;
         end case;
         Path_Add ("]");
      end Path_Add_Signature;

      procedure Path_Add_Name (N : Iir) is
      begin
         Eval_Simple_Name (Get_Identifier (N));
         if Nam_Buffer (1) /= 'P' then
            --  Skip anonymous processes.
            Path_Add (Nam_Buffer (1 .. Nam_Length));
         end if;
      end Path_Add_Name;

      procedure Path_Add_Element (El : Iir; Is_Instance : Boolean) is
      begin
         --  LRM 14.1
         --  E'INSTANCE_NAME
         --    There is one full path instance element for each component
         --    instantiation, block statement, generate statemenent, process
         --    statement, or subprogram body in the design hierarchy between
         --    the top design entity and the named entity denoted by the
         --    prefix.
         --
         --  E'PATH_NAME
         --    There is one path instance element for each component
         --    instantiation, block statement, generate statement, process
         --    statement, or subprogram body in the design hierarchy between
         --    the root design entity and the named entity denoted by the
         --    prefix.
         case Get_Kind (El) is
            when Iir_Kind_Library_Declaration =>
               Path_Add (":");
               Path_Add_Name (El);
               Path_Add (":");
            when Iir_Kind_Package_Declaration
              | Iir_Kind_Package_Body =>
               Path_Add_Element
                 (Get_Library (Get_Design_File (Get_Design_Unit (El))),
                  Is_Instance);
               Path_Add_Name (El);
               Path_Add (":");
            when Iir_Kind_Entity_Declaration =>
               Path_Instance := El;
            when Iir_Kind_Architecture_Body =>
               Path_Instance := El;
            when Iir_Kind_Design_Unit =>
               Path_Add_Element (Get_Library_Unit (El), Is_Instance);
            when Iir_Kind_Sensitized_Process_Statement
              | Iir_Kind_Process_Statement
              | Iir_Kind_Block_Statement =>
               Path_Add_Element (Get_Parent (El), Is_Instance);
               Path_Add_Name (El);
               Path_Add (":");
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration =>
               Path_Add_Element (Get_Parent (El), Is_Instance);
               Path_Add_Name (El);
               if Flags.Vhdl_Std >= Vhdl_02 then
                  --  Add signature.
                  Path_Add_Signature (El);
               end if;
               Path_Add (":");
            when Iir_Kind_Procedure_Body =>
               Path_Add_Element (Get_Subprogram_Specification (El),
                                 Is_Instance);
            when Iir_Kind_For_Generate_Statement =>
               Path_Instance := El;
            when Iir_Kind_If_Generate_Statement =>
               Path_Add_Element (Get_Parent (El), Is_Instance);
               Path_Add_Name (El);
               Path_Add (":");
            when Iir_Kind_Generate_Statement_Body =>
               declare
                  Parent : constant Iir := Get_Parent (El);
               begin
                  if Get_Kind (Parent) = Iir_Kind_For_Generate_Statement then
                     Path_Instance := El;
                  else
                     Path_Add_Element (Parent, Is_Instance);
                  end if;
               end;
            when Iir_Kinds_Sequential_Statement =>
               Path_Add_Element (Get_Parent (El), Is_Instance);
            when others =>
               Error_Kind ("path_add_element", El);
         end case;
      end Path_Add_Element;

      Prefix : constant Iir := Get_Named_Entity (Get_Prefix (Attr));
      Is_Instance : constant Boolean :=
        Get_Kind (Attr) = Iir_Kind_Instance_Name_Attribute;
   begin
      Path_Reset;

      --  LRM 14.1
      --  E'PATH_NAME
      --    The local item name in E'PATH_NAME equals E'SIMPLE_NAME, unless
      --    E denotes a library, package, subprogram or label. In this
      --    latter case, the package based path or instance based path,
      --    as appropriate, will not contain a local item name.
      --
      --  E'INSTANCE_NAME
      --    The local item name in E'INSTANCE_NAME equals E'SIMPLE_NAME,
      --    unless E denotes a library, package, subprogram, or label.  In
      --    this latter case, the package based path or full instance based
      --    path, as appropriate, will not contain a local item name.
      case Get_Kind (Prefix) is
         when Iir_Kind_Constant_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration =>
            Path_Add_Element (Get_Parent (Prefix), Is_Instance);
            Path_Add_Name (Prefix);
         when Iir_Kind_Library_Declaration
           | Iir_Kinds_Library_Unit_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kinds_Concurrent_Statement
           | Iir_Kinds_Sequential_Statement =>
            Path_Add_Element (Prefix, Is_Instance);
         when others =>
            Error_Kind ("get_path_instance_name_suffix", Prefix);
      end case;

      declare
         Result : constant Path_Instance_Name_Type :=
           (Len => Path_Len,
            Path_Instance => Path_Instance,
            Suffix => Path_Str (1 .. Path_Len));
      begin
         Deallocate (Path_Str);
         return Result;
      end;
   end Get_Path_Instance_Name_Suffix;

end Evaluation;
