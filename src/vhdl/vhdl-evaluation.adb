--  Evaluation of static expressions.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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

with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;
with Interfaces;

with Name_Table; use Name_Table;
with Str_Table;
with Flags; use Flags;
with Std_Names;
with Errorout; use Errorout;

with Vhdl.Scanner;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Std_Package; use Vhdl.Std_Package;
with Vhdl.Ieee.Std_Logic_1164;

with Elab.Vhdl_Objtypes;
with Elab.Vhdl_Types;
with Elab.Memtype;
with Synth.Vhdl_Eval;

with Grt.Types;
with Grt.Vhdl_Types;
with Grt.Fcvt;
with Grt.To_Strings;

package body Vhdl.Evaluation is
   --  If FORCE is true, always return a literal.
   function Eval_Expr_Keep_Orig (Expr : Iir; Force : Boolean) return Iir;

   function Eval_Check_Bound (Expr : Iir; Sub_Type : Iir) return Boolean;

   function Eval_Enum_To_String (Lit : Iir; Orig : Iir) return Iir;
   function Eval_Integer_Image (Val : Int64; Orig : Iir) return Iir;
   function Eval_Floating_Image (Val : Fp64; Orig : Iir) return Iir;
   function Eval_Floating_To_String_Format (Val : Fp64; Fmt : Iir; Orig : Iir)
                                           return Iir;

   function Eval_Scalar_Compare (Left, Right : Iir) return Compare_Type;

   function Get_Physical_Value (Expr : Iir) return Int64
   is
      pragma Unsuppress (Overflow_Check);
      Kind : constant Iir_Kind := Get_Kind (Expr);
      Unit : Iir;
   begin
      case Kind is
         when Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal =>
            --  Extract Unit.
            Unit := Get_Physical_Literal
              (Get_Named_Entity (Get_Unit_Name (Expr)));
            pragma Assert (Get_Kind (Unit) = Iir_Kind_Integer_Literal);
            case Iir_Kinds_Physical_Literal (Kind) is
               when Iir_Kind_Physical_Int_Literal =>
                  return Get_Value (Expr) * Get_Value (Unit);
               when Iir_Kind_Physical_Fp_Literal =>
                  return Int64 (Get_Fp_Value (Expr) * Fp64 (Get_Value (Unit)));
            end case;
         when Iir_Kind_Integer_Literal =>
            return Get_Value (Expr);
         when Iir_Kind_Unit_Declaration =>
            return Get_Value (Get_Physical_Literal (Expr));
         when others =>
            Error_Kind ("get_physical_value", Expr);
      end case;
   end Get_Physical_Value;

   function Build_Integer (Val : Int64; Lit_Type : Iir; Orig : Iir) return Iir
   is
      Res : Iir_Integer_Literal;
   begin
      Res := Create_Iir (Iir_Kind_Integer_Literal);
      Location_Copy (Res, Orig);
      Set_Value (Res, Val);
      Set_Type (Res, Lit_Type);
      Set_Expr_Staticness (Res, Locally);
      return Res;
   end Build_Integer;

   function Build_Integer (Val : Int64; Origin : Iir) return Iir
   is
      Res : Iir_Integer_Literal;
   begin
      Res := Build_Integer (Val, Get_Type (Origin), Origin);
      Set_Literal_Origin (Res, Origin);
      return Res;
   end Build_Integer;

   function Build_Floating (Val : Fp64; Origin : Iir)
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

   function Build_Enumeration_Constant
     (Val : Iir_Index32; Lit_Type : Iir; Orig : Iir) return Iir
   is
      Enum_Type : constant Iir := Get_Base_Type (Lit_Type);
      Enum_List : constant Iir_Flist :=
        Get_Enumeration_Literal_List (Enum_Type);
      Lit : constant Iir_Enumeration_Literal :=
        Get_Nth_Element (Enum_List, Integer (Val));
      Res : Iir_Enumeration_Literal;
   begin
      Res := Copy_Enumeration_Literal (Lit);
      Location_Copy (Res, Orig);
      return Res;
   end Build_Enumeration_Constant;

   function Build_Enumeration_Constant (Val : Iir_Index32; Origin : Iir)
     return Iir_Enumeration_Literal
   is
      Res : Iir_Enumeration_Literal;
   begin
      Res := Build_Enumeration_Constant (Val, Get_Type (Origin), Origin);
      Set_Literal_Origin (Res, Origin);
      return Res;
   end Build_Enumeration_Constant;

   function Build_Physical (Val : Int64; Origin : Iir)
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
   end Build_Physical;

   function Build_Discrete (Val : Int64; Origin : Iir) return Iir is
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

   function Build_String (Str : String; Orig : Iir) return Iir
   is
      use Str_Table;
      Id : String8_Id;
   begin
      Id := Create_String8;
      for I in Str'Range loop
         Append_String8_Char (Str (I));
      end loop;
      return Build_String (Id, Int32 (Str'Length), Orig);
   end Build_String;


   --  Build a simple aggregate composed of EL_LIST from ORIGIN.  STYPE is the
   --  type of the aggregate.  DEF_TYPE should be either Null_Iir or STYPE.  It
   --  is set only when a new subtype has been created for the aggregate.
   function Build_Simple_Aggregate (El_List : Iir_Flist;
                                    Origin : Iir;
                                    Stype : Iir;
                                    Def_Type : Iir := Null_Iir)
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
      Set_Literal_Subtype (Res, Def_Type);
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
      --  Expression is not static so that it will be an error if it needs
      --  to.  Otherwise, the error will occur at runtime.
      Set_Expr_Staticness (Res, None);
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
           | Iir_Kind_Physical_Fp_Literal
           | Iir_Kind_Unit_Declaration =>
            Res := Create_Iir (Iir_Kind_Integer_Literal);
            Set_Value (Res, Get_Physical_Value (Val));

         when Iir_Kind_String_Literal8 =>
            Res := Create_Iir (Iir_Kind_String_Literal8);
            Set_String8_Id (Res, Get_String8_Id (Val));
            Set_String_Length (Res, Get_String_Length (Val));

         when Iir_Kind_Simple_Aggregate =>
            Res := Create_Iir (Iir_Kind_Simple_Aggregate);
            Set_Simple_Aggregate_List (Res, Get_Simple_Aggregate_List (Val));

         when Iir_Kind_Aggregate =>
            --  FIXME: ownership violation: both RES and VAL are parents of
            --   association_choices_chain and aggregate_info.
            --  But this aggregate is always temporary.
            --  TODO: add maybe_ref_chain.
            Res := Create_Iir (Iir_Kind_Aggregate);
            Set_Association_Choices_Chain
              (Res, Get_Association_Choices_Chain (Val));
            Set_Aggregate_Info (Res, Get_Aggregate_Info (Val));
            Set_Aggregate_Expand_Flag (Res, Get_Aggregate_Expand_Flag (Val));

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

   function Copy_Constant (Val : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Build_Constant (Val, Val);
      Set_Literal_Origin (Res, Null_Iir);
      return Res;
   end Copy_Constant;

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
      Enum_List : constant Iir_Flist :=
        Get_Enumeration_Literal_List (Enum_Type);
   begin
      return Get_Nth_Element (Enum_List, Integer (Val));
   end Build_Enumeration;

   function Build_Enumeration (Val : Boolean; Origin : Iir)
                              return Iir_Enumeration_Literal
   is
      Enum_Type : constant Iir := Get_Base_Type (Get_Type (Origin));
      Enum_List : constant Iir_Flist :=
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
               return Build_Integer (Int64'Last, Origin);
            else
               return Build_Integer (Int64'First, Origin);
            end if;
         when others =>
            Error_Kind ("build_extreme_value", Orig_Type);
      end case;
   end Build_Extreme_Value;

   --  Check VAL fits in the base type.
   function Build_Integer_Check (Val : Int64; Origin : Iir)
     return Iir_Integer_Literal
   is
      Atype : constant Iir := Get_Base_Type (Get_Type (Origin));
      subtype Rng_32 is Int64 range Int64 (Int32'First) .. Int64 (Int32'Last);
   begin
      if Get_Scalar_Size (Atype) = Scalar_32
        and then Val not in Rng_32
      then
         Warning_Msg_Sem (Warnid_Runtime_Error, +Origin,
                          "arithmetic overflow in static expression");
         return Build_Overflow (Origin);
      end if;

      return Build_Integer (Val, Origin);
   end Build_Integer_Check;

   --  A_RANGE is a range expression, whose type, location, expr_staticness,
   --  left_limit and direction are set.
   --  Type of A_RANGE must have a range_constraint.
   --  Set the right limit of A_RANGE from LEN.
   procedure Set_Right_Limit_By_Length (A_Range : Iir; Len : Int64)
   is
      A_Type : constant Iir := Get_Type (A_Range);
      Left : constant Iir := Get_Left_Limit (A_Range);
      Right : Iir;
      Pos : Int64;
   begin
      pragma Assert (Get_Expr_Staticness (A_Range) = Locally);

      Pos := Eval_Pos (Left);
      case Get_Direction (A_Range) is
         when Dir_To =>
            Pos := Pos + Len - 1;
         when Dir_Downto =>
            Pos := Pos - Len + 1;
      end case;
      if Len > 0
        and then not Eval_Int_In_Range (Pos, Get_Range_Constraint (A_Type))
      then
         Error_Msg_Sem (+A_Range, "range length is beyond subtype length");
         Right := Left;
      else
         Right := Build_Discrete (Pos, A_Range);
         Set_Literal_Origin (Right, Null_Iir);
         Set_Right_Limit_Expr (A_Range, Right);
      end if;
      Set_Right_Limit (A_Range, Right);
   end Set_Right_Limit_By_Length;

   --  LRM08 9.3.2 Literals
   --  If there is a value to the left of the nominal leftmost bound (given by
   --  the 'LEFTOF) attribute, then the leftmost bound is the nominal leftmost
   --  bound, and the rightmost bound is the value to the left of the nominal
   --  leftmost bound.  Otherwise, the leftmost bound is the value to the
   --  right of the nominal leftmost bound, and the rightmost bound is the
   --  nominal leftmost bound.
   procedure Set_Enumeration_Null_Range_Limits (A_Range : Iir)
   is
      A_Type : constant Iir := Get_Type (A_Range);
      Btype : constant Iir := Get_Base_Type (A_Type);
      Enum_List : constant Iir_Flist := Get_Enumeration_Literal_List (Btype);
      Last_Enum : constant Natural := Flist_Last (Enum_List);
      Left : constant Iir := Get_Left_Limit (A_Range);
      Right : Iir;
      Pos : Int64;
      Invert : Boolean;
   begin
      pragma Assert (Get_Expr_Staticness (A_Range) = Locally);

      if Last_Enum = 0 then
         Error_Msg_Sem
           (+A_Range, "null range not supported for enumeration type %n",
            +A_Type);
         Right := Left;
      else
         Pos := Eval_Pos (Left);
         Invert := False;
         case Get_Direction (A_Range) is
            when Dir_To =>
               if Pos = 0 then
                  Pos := Pos + 1;
                  Invert := True;
               else
                  Pos := Pos - 1;
               end if;
            when Dir_Downto =>
               if Pos = Int64 (Last_Enum) then
                  Pos := Pos - 1;
                  Invert := True;
               else
                  Pos := Pos + 1;
               end if;
         end case;

         Right := Build_Discrete (Pos, A_Range);
         Set_Literal_Origin (Right, Null_Iir);

         if Invert then
            Set_Left_Limit_Expr (A_Range, Right);
            Set_Left_Limit (A_Range, Right);
            Set_Right_Limit (A_Range, Left);
         else
            Set_Right_Limit_Expr (A_Range, Right);
            Set_Right_Limit (A_Range, Right);
         end if;
      end if;
   end Set_Enumeration_Null_Range_Limits;

   --  Create a range of type A_TYPE whose length is LEN.
   --  Note: only two nodes are created:
   --  * the range_expression (node returned)
   --  * the right bound
   --  The left bound *IS NOT* created, but points to the left bound of A_TYPE.
   function Create_Range_By_Length
     (A_Type : Iir; Len : Int64; Loc : Location_Type)
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
      if Len = 0
        and then (Get_Kind (Get_Base_Type (A_Type))
                    = Iir_Kind_Enumeration_Type_Definition)
      then
         Set_Enumeration_Null_Range_Limits (Constraint);
      else
         Set_Right_Limit_By_Length (Constraint, Len);
      end if;
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
      Set_Parent_Type (Res, A_Type);
      Set_Type_Staticness (Res, Locally);

      return Res;
   end Create_Range_Subtype_From_Type;

   --  Create a subtype of A_TYPE whose length is LEN.
   --  This is used to create subtypes for strings or aggregates.
   function Create_Range_Subtype_By_Length
     (A_Type : Iir; Len : Int64; Loc : Location_Type)
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
      Set_Nth_Element (Get_Index_Subtype_List (Res), 0, Index_Type);
      Set_Type_Staticness (Res, Min (Get_Type_Staticness (Res),
                                     Get_Type_Staticness (Index_Type)));
      Set_Constraint_State (Res, Fully_Constrained);
      Set_Index_Constraint_Flag (Res, True);
      return Res;
   end Create_Unidim_Array_From_Index;

   function Create_Unidim_Array_By_Length
     (Base_Type : Iir; Len : Int64; Loc : Iir)
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
      L : Iir_Flist;
   begin
      if Res /= Orig then
         L := Get_Simple_Aggregate_List (Res);
         Destroy_Iir_Flist (L);
         Free_Iir (Res);
      end if;
   end Free_Eval_String_Literal;

   function String_Literal8_To_Simple_Aggregate (Str : Iir) return Iir
   is
      Element_Type : constant Iir := Get_Base_Type
        (Get_Element_Subtype (Get_Base_Type (Get_Type (Str))));
      Literal_List : constant Iir_Flist :=
        Get_Enumeration_Literal_List (Element_Type);

      Len : constant Nat32 := Get_String_Length (Str);
      Id : constant String8_Id := Get_String8_Id (Str);

      List : Iir_Flist;
      Lit : Iir;
   begin
      List := Create_Iir_Flist (Natural (Len));

      for I in 1 .. Len loop
         Lit := Get_Nth_Element
           (Literal_List, Natural (Str_Table.Element_String8 (Id, I)));
         Set_Nth_Element (List, Natural (I - 1), Lit);
      end loop;
      return Build_Simple_Aggregate (List, Str, Get_Type (Str));
   end String_Literal8_To_Simple_Aggregate;

   --  Return the offset of EXPR in RNG.  A result of 0 means the left bound,
   --  a result of 1 mean the next element after the left bound.
   --  Assume no overflow.
   function Eval_Pos_In_Range (Rng : Iir; Expr : Iir) return Iir_Index32
   is
      Left_Pos : constant Int64 := Eval_Pos (Get_Left_Limit (Rng));
      Pos : constant Int64 := Eval_Pos (Expr);
   begin
      case Get_Direction (Rng) is
         when Dir_To =>
            return Iir_Index32 (Pos - Left_Pos);
         when Dir_Downto =>
            return Iir_Index32 (Left_Pos - Pos);
      end case;
   end Eval_Pos_In_Range;

   procedure Build_Array_Choices_Vector (Vect : out Iir_Array;
                                         Choice_Range : Iir;
                                         Choices_Chain : Iir;
                                         Last_Dim : Boolean)
   is
      pragma Assert (Vect'First = 0);
      pragma Assert (Vect'Length = Eval_Discrete_Range_Length (Choice_Range));
      Assoc : Iir;
      Expr : Iir;
      Cur_Pos : Natural;
   begin
      --  Initialize Vect (to correctly handle 'others').
      Vect := (others => Null_Iir);

      Assoc := Choices_Chain;
      Cur_Pos := 0;
      Expr := Null_Iir;
      while Is_Valid (Assoc) loop
         if not Get_Same_Alternative_Flag (Assoc) then
            if Last_Dim then
               Expr := Get_Associated_Expr (Assoc);
            else
               Expr := Assoc;
            end if;
         end if;
         case Iir_Kinds_Array_Choice (Get_Kind (Assoc)) is
            when Iir_Kind_Choice_By_None =>
               if Get_Element_Type_Flag (Assoc) then
                  Vect (Cur_Pos) := Expr;
                  Cur_Pos := Cur_Pos + 1;
               else
                  declare
                     Assoc_Len : Int64;
                  begin
                     pragma Assert (Last_Dim);
                     Assoc_Len := Eval_Discrete_Type_Length
                       (Get_Index_Type (Get_Type (Expr), 0));
                     for I in 0 .. Iir_Index32 (Assoc_Len - 1) loop
                        Vect (Cur_Pos) :=
                          Eval_Indexed_Name_By_Offset (Expr, I);
                        Cur_Pos := Cur_Pos + 1;
                     end loop;
                  end;
               end if;
            when Iir_Kind_Choice_By_Range =>
               declare
                  Rng : constant Iir := Get_Choice_Range (Assoc);
                  Rng_Start : Iir;
                  Rng_Len : Int64;
                  E : Iir;
               begin
                  if Get_Direction (Rng) = Get_Direction (Choice_Range) then
                     Rng_Start := Get_Left_Limit (Rng);
                  else
                     Rng_Start := Get_Right_Limit (Rng);
                  end if;
                  Cur_Pos := Natural
                    (Eval_Pos_In_Range (Choice_Range, Rng_Start));
                  Rng_Len := Eval_Discrete_Range_Length (Rng);
                  for I in 1 .. Iir_Index32 (Rng_Len) loop
                     if Get_Element_Type_Flag (Assoc) then
                        E := Expr;
                     else
                        pragma Assert (Last_Dim);
                        E := Eval_Indexed_Name_By_Offset (Expr, I - 1);
                     end if;
                     Vect (Cur_Pos) := E;
                     Cur_Pos := Cur_Pos + 1;
                  end loop;
               end;
            when Iir_Kind_Choice_By_Expression =>
               Cur_Pos := Natural
                 (Eval_Pos_In_Range (Choice_Range,
                                     Get_Choice_Expression (Assoc)));
               Vect (Cur_Pos) := Expr;
            when Iir_Kind_Choice_By_Others =>
               for I in Vect'Range loop
                  if Vect (I) = Null_Iir then
                     Vect (I) := Expr;
                  end if;
               end loop;
         end case;
         Assoc := Get_Chain (Assoc);
      end loop;
   end Build_Array_Choices_Vector;

   function Array_Aggregate_To_Simple_Aggregate (Aggr : Iir) return Iir
   is
      Aggr_Type : constant Iir := Get_Type (Aggr);
      Index_Type : constant Iir := Get_Index_Type (Aggr_Type, 0);
      Index_Range : constant Iir := Eval_Static_Range (Index_Type);
      Len : constant Int64 := Eval_Discrete_Range_Length (Index_Range);
      Assocs : constant Iir := Get_Association_Choices_Chain (Aggr);
      Vect : Iir_Array (0 .. Integer (Len - 1));
      List : Iir_Flist;
      Assoc : Iir;
      Expr : Iir;
   begin
      Assoc := Assocs;
      while Is_Valid (Assoc) loop
         if not Get_Same_Alternative_Flag (Assoc) then
            Expr := Get_Associated_Expr (Assoc);
            if Get_Kind (Get_Type (Expr))
              in Iir_Kinds_Scalar_Type_And_Subtype_Definition
            then
               Expr := Eval_Expr_Keep_Orig (Expr, True);
               Set_Associated_Expr (Assoc, Expr);
            end if;
         end if;
         Assoc := Get_Chain (Assoc);
      end loop;

      Build_Array_Choices_Vector (Vect, Index_Range, Assocs, True);

      List := Create_Iir_Flist (Natural (Len));
      if Len > 0 then
         --  Workaround GNAT GPL2014 compiler bug.
         for I in Vect'Range loop
            Set_Nth_Element (List, I, Vect (I));
         end loop;
      end if;

      return Build_Simple_Aggregate (List, Aggr, Aggr_Type);
   end Array_Aggregate_To_Simple_Aggregate;

   function Eval_String_Literal (Str : Iir) return Iir is
   begin
      case Get_Kind (Str) is
         when Iir_Kind_String_Literal8 =>
            return String_Literal8_To_Simple_Aggregate (Str);

         when Iir_Kind_Aggregate =>
            return Array_Aggregate_To_Simple_Aggregate (Str);

         when Iir_Kind_Simple_Aggregate =>
            return Str;

         when others =>
            Error_Kind ("eval_string_literal", Str);
      end case;
   end Eval_String_Literal;

   package Synth_Helpers is
      use Elab.Vhdl_Objtypes;
      use Elab.Memtype;

      function Convert_Node_To_Typ (N : Iir) return Type_Acc;
      function Convert_Node_To_Memtyp (N : Iir) return Memtyp;
      function Convert_Memtyp_To_Node (Mt : Memtyp; Btype : Iir; Orig : Iir)
                                      return Iir;
   end Synth_Helpers;

   package body Synth_Helpers is
      use Elab.Vhdl_Types;

      function Convert_Discrete_Range (Rng : Iir) return Discrete_Range_Type is
      begin
         return Build_Discrete_Range_Type
           (Eval_Pos (Get_Left_Limit (Rng)),
            Eval_Pos (Get_Right_Limit (Rng)),
            Get_Direction (Rng));
      end Convert_Discrete_Range;

      function Convert_Node_To_Typ (N : Iir) return Type_Acc is
      begin
         case Get_Kind (N) is
            when Iir_Kind_Enumeration_Type_Definition =>
               return Elab_Enumeration_Type_Definition (N);
            when Iir_Kind_Integer_Type_Definition =>
               declare
                  Decl : constant Iir := Get_Type_Declarator (N);
                  St : constant Iir := Get_Subtype_Definition (Decl);
                  pragma Assert
                    (Get_Kind (St) = Iir_Kind_Integer_Subtype_Definition);
               begin
                  return Elab_Scalar_Type_Definition (N, St);
               end;
            when Iir_Kind_Integer_Subtype_Definition
              | Iir_Kind_Enumeration_Subtype_Definition =>
               declare
                  Rng : constant Iir := Get_Range_Constraint (N);
                  Base_Typ : Type_Acc;
                  Res_Rng : Discrete_Range_Type;
                  W : Uns32;
               begin
                  Base_Typ := Convert_Node_To_Typ (Get_Base_Type (N));
                  if Base_Typ.Kind in Type_Nets then
                     --  A subtype of a bit/logic type is still a bit/logic.
                     --  FIXME: bounds.
                     return Base_Typ;
                  end if;
                  Res_Rng := Convert_Discrete_Range (Rng);
                  W := Discrete_Range_Width (Res_Rng);
                  return Create_Discrete_Type (Res_Rng, Base_Typ.Sz, W);
               end;
            when Iir_Kind_Floating_Type_Definition =>
               return Create_Float_Type ((Dir_To, Fp64'First, Fp64'Last));
            when Iir_Kind_Array_Type_Definition =>
               declare
                  El : Type_Acc;
                  Idx : Type_Acc;
               begin
                  if Get_Nbr_Elements (Get_Index_Subtype_List (N)) /= 1 then
                     raise Internal_Error;
                  end if;
                  El := Convert_Node_To_Typ (Get_Element_Subtype (N));
                  Idx := Convert_Node_To_Typ (Get_Index_Type (N, 0));
                  if El.Kind in Type_Nets then
                     return Create_Unbounded_Vector (El, Idx);
                  else
                     return Create_Unbounded_Array (Idx, True, El);
                  end if;
               end;
            when Iir_Kind_Array_Subtype_Definition =>
               declare
                  Idx : constant Iir := Get_Index_Type (N, 0);
                  El_Typ : Type_Acc;
                  Res_Rng : Discrete_Range_Type;
               begin
                  El_Typ := Convert_Node_To_Typ (Get_Element_Subtype (N));
                  pragma Assert (El_Typ.Kind in Type_Nets);
                  Res_Rng := Convert_Discrete_Range
                    (Get_Range_Constraint (Idx));
                  return Create_Vector_Type
                    (Synth_Bounds_From_Range (Res_Rng), El_Typ);
               end;

            when others =>
               Error_Kind ("convert_node_to_typ", N);
         end case;
         return null;
      end Convert_Node_To_Typ;

      function Convert_Node_To_Memtyp (N : Iir; Typ : Type_Acc) return Memtyp
      is
         Res : Memtyp;
      begin
         case Get_Kind (N) is
            when Iir_Kind_Aggregate =>
               declare
                  Sa : Iir;
               begin
                  Sa := Array_Aggregate_To_Simple_Aggregate (N);
                  Res := Convert_Node_To_Memtyp (Sa, Typ);
                  --  TODO: destroy SA
                  return Res;
               end;
            when Iir_Kind_Simple_Aggregate =>
               declare
                  Els : constant Iir_Flist := Get_Simple_Aggregate_List (N);
                  Last : constant Natural := Flist_Last (Els);
                  Val : Iir;
               begin
                  pragma Assert (Typ.Kind = Type_Vector);
                  Res := Create_Memory (Typ);

                  for I in Flist_First .. Last loop
                     --  Elements are static.
                     Val := Get_Nth_Element (Els, I);
                     Write_Discrete (Res.Mem + Size_Type (I) * Typ.Arr_El.Sz,
                                     Typ.Arr_El, Eval_Pos (Val));
                  end loop;
               end;
            when Iir_Kind_String_Literal8 =>
               declare
                  Element_Type : constant Iir := Get_Base_Type
                    (Get_Element_Subtype (Get_Base_Type (Get_Type (N))));
                  Literal_List : constant Iir_Flist :=
                    Get_Enumeration_Literal_List (Element_Type);

                  Len : constant Nat32 := Get_String_Length (N);
                  Id : constant String8_Id := Get_String8_Id (N);

                  Lit : Iir;
               begin
                  Res := Create_Memory (Typ);

                  for I in 1 .. Len loop
                     Lit := Get_Nth_Element
                       (Literal_List,
                        Natural (Str_Table.Element_String8 (Id, I)));
                     Write_Discrete (Res.Mem + Size_Type (I - 1), Typ.Arr_El,
                                     Int64 (Get_Enum_Pos (Lit)));
                  end loop;
               end;
            when Iir_Kind_Integer_Literal
              | Iir_Kind_Enumeration_Literal =>
               Res := Create_Memory (Typ);
               Write_Discrete (Res.Mem, Typ, Eval_Pos (N));

            when Iir_Kind_Floating_Point_Literal =>
               Res := Create_Memory (Typ);
               Write_Fp64 (Res.Mem, Get_Fp_Value (N));

            when others =>
               Error_Kind ("convert_node_to_memtyp", N);
         end case;
         return Res;
      end Convert_Node_To_Memtyp;

      function Convert_Node_To_Memtyp (N : Iir) return Memtyp
      is
         Typ : Type_Acc;
      begin
         Typ := Convert_Node_To_Typ (Get_Type (N));
         return Convert_Node_To_Memtyp (N, Typ);
      end Convert_Node_To_Memtyp;

      function Convert_Discrete_To_Node (V : Int64; Vtype : Iir; Orig : Iir)
                                        return Iir is
      begin
         case Get_Kind (Vtype) is
            when Iir_Kind_Integer_Subtype_Definition =>
               return Build_Integer (V, Vtype, Orig);
            when Iir_Kind_Enumeration_Subtype_Definition
              | Iir_Kind_Enumeration_Type_Definition =>
               return Build_Enumeration_Constant
                 (Iir_Index32 (V), Vtype, Orig);
            when others =>
               Error_Kind ("convert_discrete_to_node", Vtype);
         end case;
      end Convert_Discrete_To_Node;

      function Convert_Bound_To_Node
        (Bnd : Bound_Type; Btype : Iir; Orig : Iir) return Iir
      is
         Rng : Iir;
         Limit : Iir;
      begin
         Rng := Create_Iir (Iir_Kind_Range_Expression);
         Location_Copy (Rng, Orig);
         Set_Expr_Staticness (Rng, Locally);
         Set_Type (Rng, Btype);
         Set_Direction (Rng, Bnd.Dir);
         Limit := Convert_Discrete_To_Node (Int64 (Bnd.Left), Btype, Orig);
         Set_Left_Limit_Expr (Rng, Limit);
         Set_Left_Limit (Rng, Limit);
         Limit := Convert_Discrete_To_Node (Int64 (Bnd.Right), Btype, Orig);
         Set_Right_Limit_Expr (Rng, Limit);
         Set_Right_Limit (Rng, Limit);
         return Rng;
      end Convert_Bound_To_Node;

      function Convert_Typ_To_Node (Typ : Type_Acc; Btype : Iir; Orig : Iir)
                                   return Iir
      is
         Res : Iir;
      begin
         case Get_Kind (Btype) is
            when Iir_Kind_Array_Type_Definition =>
               declare
                  Loc : constant Location_Type := Get_Location (Orig);
                  Base_Idx : constant Iir := Get_Index_Type (Btype, 0);
                  Rng : Iir;
                  Idx_Type : Iir;
               begin
                  Idx_Type := Create_Range_Subtype_From_Type (Base_Idx, Loc);
                  Rng := Convert_Bound_To_Node (Typ.Abound, Base_Idx, Orig);
                  Set_Range_Constraint (Idx_Type, Rng);

                  Res := Create_Array_Subtype (Btype, Loc);
                  Set_Nth_Element (Get_Index_Subtype_List (Res), 0, Idx_Type);
                  Set_Type_Staticness (Res, Locally);
                  Set_Constraint_State (Res, Fully_Constrained);
                  Set_Index_Constraint_Flag (Res, True);
                  return Res;
               end;
            when others =>
               Error_Kind ("convert_typ_to_node", Btype);
               return Null_Iir;
         end case;
      end Convert_Typ_To_Node;

      function Convert_Vect_To_Simple_Aggregate
        (Mt : Memtyp; Res_Type : Iir; Orig : Iir) return Iir
      is
         Element_Type : constant Iir := Get_Base_Type
           (Get_Element_Subtype (Get_Base_Type (Res_Type)));
         Literal_List : constant Iir_Flist :=
           Get_Enumeration_Literal_List (Element_Type);

         Len : constant Nat32 := Nat32 (Mt.Typ.Abound.Len);

         List : Iir_Flist;
         El : Int64;
         Lit : Iir;
      begin
         List := Create_Iir_Flist (Natural (Len));

         for I in 1 .. Len loop
            El := Read_Discrete (Mt.Mem + Size_Type (I - 1),
                                 Mt.Typ.Arr_El);
            Lit := Get_Nth_Element (Literal_List, Natural (El));
            Set_Nth_Element (List, Natural (I - 1), Lit);
         end loop;
         return Build_Simple_Aggregate (List, Orig, Res_Type, Res_Type);
      end Convert_Vect_To_Simple_Aggregate;

      function Convert_Memtyp_To_Node (Mt : Memtyp; Btype : Iir; Orig : Iir)
                                      return Iir
      is
         Res_Type : Iir;
      begin
         case Mt.Typ.Kind is
            when Type_Vector
              | Type_Array =>
               Res_Type := Convert_Typ_To_Node (Mt.Typ, Btype, Orig);
               return Convert_Vect_To_Simple_Aggregate
                 (Mt, Res_Type, Orig);
            when Type_Logic
              | Type_Bit =>
               return Convert_Discrete_To_Node
                 (Read_Discrete (Mt), Btype, Orig);
            when Type_Float =>
               return Build_Floating (Read_Fp64 (Mt), Orig);
            when Type_Discrete =>
               Res_Type := Get_Type (Orig);
               case Iir_Kinds_Discrete_Type_Definition (Get_Kind (Res_Type)) is
                  when Iir_Kind_Integer_Type_Definition
                    | Iir_Kind_Integer_Subtype_Definition =>
                     return Build_Integer (Read_Discrete (Mt), Orig);
                  when Iir_Kind_Enumeration_Type_Definition
                    | Iir_Kind_Enumeration_Subtype_Definition =>
                     --  Cannot happen: only bit and std_ulogic are involed in
                     --  static operations and those are handled by Type_Logic
                     --  and Type_Bit.
                     raise Internal_Error;
               end case;
            when others =>
               raise Internal_Error;
         end case;
      end Convert_Memtyp_To_Node;
   end Synth_Helpers;

   function Eval_Ieee_Operation
     (Orig : Iir; Imp : Iir; Left : Iir; Right : Iir) return Iir
   is
      use Elab.Vhdl_Objtypes;
      use Synth.Vhdl_Eval;
      use Synth_Helpers;

      Res_Type : constant Iir := Get_Return_Type (Imp);
      Marker : Mark_Type;
      Left_Mt, Right_Mt : Memtyp;
      Res_Typ : Type_Acc;
      Res_Mt : Memtyp;
      Res : Iir;
   begin
      Mark_Expr_Pool (Marker);

      Res_Typ := Convert_Node_To_Typ (Res_Type);
      Left_Mt := Convert_Node_To_Memtyp (Left);
      if Right /= Null_Iir then
         Right_Mt := Convert_Node_To_Memtyp (Right);
      else
         Right_Mt := Null_Memtyp;
      end if;

      Res_Mt := Eval_Static_Predefined_Function_Call
        (null, Left_Mt, Right_Mt, Res_Typ, Orig);

      Res := Convert_Memtyp_To_Node (Res_Mt, Res_Type, Orig);
      Release_Expr_Pool (Marker);

      return Res;
   end Eval_Ieee_Operation;

   function Eval_Predefined_Call (Orig : Iir;
                                  Call : Iir;
                                  Param1, Param2 : Iir) return Iir
   is
      use Elab.Vhdl_Objtypes;
      use Synth.Vhdl_Eval;
      use Synth_Helpers;

      Imp : constant Iir := Get_Implementation (Call);
      Res_Type : constant Iir := Get_Return_Type (Imp);
      Marker : Mark_Type;
      Param1_Mt, Param2_Mt : Memtyp;
      Res_Typ : Type_Acc;
      Res_Mt : Memtyp;
      Res : Iir;
   begin
      Mark_Expr_Pool (Marker);

      Res_Typ := Convert_Node_To_Typ (Res_Type);
      Param1_Mt := Convert_Node_To_Memtyp (Param1);
      if Param2 /= Null_Iir then
         Param2_Mt := Convert_Node_To_Memtyp (Param2);
      else
         Param2_Mt := Null_Memtyp;
      end if;
      Res_Mt := Eval_Static_Predefined_Function_Call
        (null, Param1_Mt, Param2_Mt, Res_Typ, Call);
      Res := Convert_Memtyp_To_Node (Res_Mt, Res_Type, Orig);
      Release_Expr_Pool (Marker);

      return Res;
   end Eval_Predefined_Call;

   function Eval_Monadic_Operator (Orig : Iir; Operand : Iir) return Iir
   is
      pragma Unsuppress (Overflow_Check);
      subtype Iir_Predefined_Vector_Minmax is Iir_Predefined_Functions range
        Iir_Predefined_Vector_Minimum .. Iir_Predefined_Vector_Maximum;

      Imp : constant Iir := Get_Implementation (Orig);
      Func : Iir_Predefined_Functions;
   begin
      if Is_Overflow_Literal (Operand) then
         --  Propagate overflow.
         return Build_Overflow (Orig);
      end if;

      Func := Get_Implicit_Definition (Imp);
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

         when Iir_Predefined_Bit_Condition =>
            return Build_Enumeration (Get_Enum_Pos (Operand) = 1, Orig);

         when Iir_Predefined_TF_Array_Not =>
            declare
               Lit_Val : Iir;
               O_List : Iir_Flist;
               R_List : Iir_Flist;
               El : Iir;
               Lit : Iir;
            begin
               Lit_Val := Eval_String_Literal (Operand);
               O_List := Get_Simple_Aggregate_List (Lit_Val);
               R_List := Create_Iir_Flist (Get_Nbr_Elements (O_List));

               for I in Flist_First .. Flist_Last (O_List) loop
                  El := Get_Nth_Element (O_List, I);
                  case Get_Enum_Pos (El) is
                     when 0 =>
                        Lit := Bit_1;
                     when 1 =>
                        Lit := Bit_0;
                     when others =>
                        raise Internal_Error;
                  end case;
                  Set_Nth_Element (R_List, I, Lit);
               end loop;
               Free_Eval_String_Literal (Lit_Val, Operand);
               return Build_Simple_Aggregate
                 (R_List, Orig, Get_Type (Operand));
            end;

         when Iir_Predefined_Enum_To_String =>
            return Eval_Enum_To_String (Operand, Orig);
         when Iir_Predefined_Integer_To_String =>
            return Eval_Integer_Image (Get_Value (Operand), Orig);
         when Iir_Predefined_Floating_To_String =>
            return Eval_Floating_Image (Get_Fp_Value (Operand), Orig);

         when Iir_Predefined_Array_Char_To_String =>
            --  LRM08 5.7 String representation
            --  - For a given value that is of a one-dimensional array type
            --    whose element type is a character type that contains only
            --    character literals, the string representation has the same
            --    length as the given value.  Each element of the string
            --    representation is the same character literal as the matching
            --    element of the given value.
            declare
               Saggr : Iir;
               Lits : Iir_Flist;
               El : Iir;
               C : Character;
               String_Id : String8_Id;
               Len : Natural;
            begin
               Saggr := Eval_String_Literal (Operand);
               Lits := Get_Simple_Aggregate_List (Saggr);
               Len := Get_Nbr_Elements (Lits);
               String_Id := Str_Table.Create_String8;
               for I in Flist_First .. Flist_Last (Lits) loop
                  El := Get_Nth_Element (Lits, I);
                  C := Get_Character (Get_Identifier (El));
                  Str_Table.Append_String8_Char (C);
               end loop;
               Free_Eval_String_Literal (Saggr, Operand);

               return Build_String (String_Id, Nat32 (Len), Orig);
            end;

         when Iir_Predefined_Vector_Minimum
           | Iir_Predefined_Vector_Maximum =>
            --  LRM08 5.3.2.4 Predefined operations on array types
            declare
               Saggr : Iir;
               Lits : Iir_Flist;
               Res : Iir;
               El : Iir;
               Cmp : Compare_Type;
            begin
               Saggr := Eval_String_Literal (Operand);
               Lits := Get_Simple_Aggregate_List (Saggr);

               if Get_Nbr_Elements (Lits) = 0 then
                  declare
                     Typ : constant Iir :=
                       Get_Type (Get_Implementation (Orig));
                     Rng : constant Iir := Eval_Static_Range (Typ);
                  begin
                     case Iir_Predefined_Vector_Minmax (Func) is
                        when Iir_Predefined_Vector_Minimum =>
                           Res := Get_High_Limit (Rng);
                        when Iir_Predefined_Vector_Maximum =>
                           Res := Get_Low_Limit (Rng);
                     end case;
                     Res := Eval_Static_Expr (Res);
                  end;
               else
                  Res := Get_Nth_Element (Lits, 0);
                  for I in Flist_First .. Flist_Last (Lits) loop
                     El := Get_Nth_Element (Lits, I);
                     Cmp := Eval_Scalar_Compare (El, Res);
                     case Iir_Predefined_Vector_Minmax (Func) is
                        when Iir_Predefined_Vector_Minimum =>
                           if Cmp <= Compare_Eq then
                              Res := El;
                           end if;
                        when Iir_Predefined_Vector_Maximum =>
                           if Cmp >= Compare_Eq then
                              Res := El;
                           end if;
                     end case;
                  end loop;
               end if;
               Free_Eval_String_Literal (Saggr, Operand);
               return Res;
            end;

         when Iir_Predefined_IEEE_Explicit
            | Iir_Predefined_Bit_Vector_To_Hstring
            | Iir_Predefined_Bit_Vector_To_Ostring =>
            return Eval_Ieee_Operation (Orig, Imp, Operand, Null_Iir);

         when others =>
            Error_Internal (Orig, "eval_monadic_operator: " &
                            Iir_Predefined_Functions'Image (Func));
      end case;
   exception
      when Constraint_Error =>
         --  Can happen for absolute.
         Warning_Msg_Sem (Warnid_Runtime_Error, +Orig,
                          "arithmetic overflow in static expression");
         return Build_Overflow (Orig);
   end Eval_Monadic_Operator;

   function Eval_Dyadic_Bit_Array_Operator
     (Expr : Iir;
      Left, Right : Iir;
      Func : Iir_Predefined_Dyadic_TF_Array_Functions) return Iir
   is
      Expr_Type : constant Iir := Get_Type (Expr);
      El_Type : constant Iir :=
        Get_Base_Type (Get_Element_Subtype (Expr_Type));
      Enum_List : constant Iir_Flist := Get_Enumeration_Literal_List (El_Type);
      Cst_0 : constant Iir := Get_Nth_Element (Enum_List, 0);
      Cst_1 : constant Iir := Get_Nth_Element (Enum_List, 1);
      Left_Val, Right_Val : Iir;
      R_List, L_List : Iir_Flist;
      Len : Natural;
      Res : Iir;
      Res_List : Iir_Flist;
      El : Iir;
   begin
      Left_Val := Eval_String_Literal (Left);
      Right_Val := Eval_String_Literal (Right);

      L_List := Get_Simple_Aggregate_List (Left_Val);
      R_List := Get_Simple_Aggregate_List (Right_Val);
      Len := Get_Nbr_Elements (L_List);

      if Len /= Get_Nbr_Elements (R_List) then
         Warning_Msg_Sem (Warnid_Runtime_Error, +Expr,
                          "length of left and right operands mismatch");
         Res := Build_Overflow (Expr);
      else
         Res_List := Create_Iir_Flist (Len);

         case Func is
            when Iir_Predefined_TF_Array_And =>
               for I in 0 .. Len - 1 loop
                  El := Get_Nth_Element (L_List, I);
                  case Get_Enum_Pos (El) is
                     when 0 =>
                        null;
                     when 1 =>
                        El := Get_Nth_Element (R_List, I);
                     when others =>
                        raise Internal_Error;
                  end case;
                  Set_Nth_Element (Res_List, I, El);
               end loop;
            when Iir_Predefined_TF_Array_Nand =>
               for I in 0 .. Len - 1 loop
                  El := Get_Nth_Element (L_List, I);
                  case Get_Enum_Pos (El) is
                     when 0 =>
                        El := Cst_1;
                     when 1 =>
                        El := Get_Nth_Element (R_List, I);
                        case Get_Enum_Pos (El) is
                           when 0 =>
                              El := Cst_1;
                           when 1 =>
                              El := Cst_0;
                           when others =>
                              raise Internal_Error;
                        end case;
                     when others =>
                        raise Internal_Error;
                  end case;
                  Set_Nth_Element (Res_List, I, El);
               end loop;
            when Iir_Predefined_TF_Array_Or =>
               for I in 0 .. Len - 1 loop
                  El := Get_Nth_Element (L_List, I);
                  case Get_Enum_Pos (El) is
                     when 1 =>
                        null;
                     when 0 =>
                        El := Get_Nth_Element (R_List, I);
                     when others =>
                        raise Internal_Error;
                  end case;
                  Set_Nth_Element (Res_List, I, El);
               end loop;
            when Iir_Predefined_TF_Array_Nor =>
               for I in 0 .. Len - 1 loop
                  El := Get_Nth_Element (L_List, I);
                  case Get_Enum_Pos (El) is
                     when 1 =>
                        El := Cst_0;
                     when 0 =>
                        El := Get_Nth_Element (R_List, I);
                        case Get_Enum_Pos (El) is
                           when 0 =>
                              El := Cst_1;
                           when 1 =>
                              El := Cst_0;
                           when others =>
                              raise Internal_Error;
                        end case;
                     when others =>
                        raise Internal_Error;
                  end case;
                  Set_Nth_Element (Res_List, I, El);
               end loop;
            when Iir_Predefined_TF_Array_Xor =>
               for I in 0 .. Len - 1 loop
                  El := Get_Nth_Element (L_List, I);
                  case Get_Enum_Pos (El) is
                     when 1 =>
                        El := Get_Nth_Element (R_List, I);
                        case Get_Enum_Pos (El) is
                           when 0 =>
                              El := Cst_1;
                           when 1 =>
                              El := Cst_0;
                           when others =>
                              raise Internal_Error;
                        end case;
                     when 0 =>
                        El := Get_Nth_Element (R_List, I);
                     when others =>
                        raise Internal_Error;
                  end case;
                  Set_Nth_Element (Res_List, I, El);
               end loop;
            when others =>
               Error_Internal (Expr, "eval_dyadic_bit_array_functions: " &
                                 Iir_Predefined_Functions'Image (Func));
         end case;

         Res := Build_Simple_Aggregate (Res_List, Expr, Expr_Type);
      end if;

      Free_Eval_Static_Expr (Left_Val, Left);
      Free_Eval_Static_Expr (Right_Val, Right);

      --  The unconstrained type is replaced by the constrained one.
      Set_Type (Res, Get_Type (Left));
      return Res;
   end Eval_Dyadic_Bit_Array_Operator;

   --  Return TRUE if VAL /= 0.
   function Check_Integer_Division_By_Zero (Expr : Iir; Val : Iir)
                                           return Boolean
   is
   begin
      if Get_Value (Val) = 0 then
         Warning_Msg_Sem (Warnid_Runtime_Error, +Expr, "division by 0");
         return False;
      else
         return True;
      end if;
   end Check_Integer_Division_By_Zero;

   function Eval_Shift_Operator
     (Left, Right : Iir; Origin : Iir; Func : Iir_Predefined_Shift_Functions)
     return Iir
   is
      Count : constant Int64 := Get_Value (Right);
      Arr_List : constant Iir_Flist := Get_Simple_Aggregate_List (Left);
      Len : constant Natural := Get_Nbr_Elements (Arr_List);
      Cnt : Natural;
      Res_List : Iir_Flist;
      Dir_Left : Boolean;
      E : Iir;
   begin
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
               Enum_List : constant Iir_Flist :=
                 Get_Enumeration_Literal_List
                 (Get_Base_Type (Get_Element_Subtype (Get_Type (Left))));
            begin
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

      Res_List := Create_Iir_Flist (Len);

      case Func is
         when Iir_Predefined_Array_Sll
           | Iir_Predefined_Array_Srl
           | Iir_Predefined_Array_Sla
           | Iir_Predefined_Array_Sra =>
            if Dir_Left then
               if Cnt < Len then
                  for I in Cnt .. Len - 1 loop
                     Set_Nth_Element
                       (Res_List, I - Cnt, Get_Nth_Element (Arr_List, I));
                  end loop;
               else
                  Cnt := Len;
               end if;
               for I in 0 .. Cnt - 1 loop
                  Set_Nth_Element (Res_List, Len - Cnt + I, E);
               end loop;
            else
               if Cnt > Len then
                  Cnt := Len;
               end if;
               for I in 0 .. Cnt - 1 loop
                  Set_Nth_Element (Res_List, I, E);
               end loop;
               for I in Cnt .. Len - 1 loop
                  Set_Nth_Element
                    (Res_List, I, Get_Nth_Element (Arr_List, I - Cnt));
               end loop;
            end if;
         when Iir_Predefined_Array_Rol
           | Iir_Predefined_Array_Ror =>
            for I in 1 .. Len loop
               Set_Nth_Element
                 (Res_List, I - 1, Get_Nth_Element (Arr_List, Cnt));
               Cnt := Cnt + 1;
               if Cnt = Len then
                  Cnt := 0;
               end if;
            end loop;
      end case;
      return Build_Simple_Aggregate (Res_List, Origin, Get_Type (Left));
   end Eval_Shift_Operator;

   --  Concatenate all the elements of OPERANDS.
   --  The first element of OPERANDS is the rightest one, the last the
   --  leftest one.  All the elements are concatenation operators.
   --  All the elements are static.
   function Eval_Concatenation (Operands : Iir_Array) return Iir
   is
      pragma Assert (Operands'First = 1);
      Orig : constant Iir := Operands (1);
      Origin_Type : constant Iir := Get_Type (Orig);

      Ops_Val : Iir_Array (Operands'Range);
      Str_Lits : Iir_Array (Operands'Range);
      Left_Op : Iir;
      Left_Val : Iir;
      Left_Lit : Iir;
      Res_List : Iir_Flist;
      Res_Len : Natural;
      Res_Type : Iir;
      Def, Left_Def : Iir_Predefined_Functions;
      Op : Iir;
      El : Iir;
      El_List : Iir_Flist;
      El_Len : Natural;
      Err_Orig : Iir;

      --  To compute the index range of the result for vhdl87.
      Leftest_Non_Null : Iir;
      Bounds_From_Subtype : Boolean;
   begin
      --  Eval operands, compute length of the result.
      Err_Orig := Null_Iir;
      Res_Len := 0;
      for I in Operands'Range loop
         Op := Operands (I);
         Def := Get_Implicit_Definition (Get_Implementation (Op));
         if Get_Kind (Op) = Iir_Kind_Function_Call then
            El := Get_Actual
              (Get_Chain (Get_Parameter_Association_Chain (Op)));
         else
            El := Get_Right (Op);
         end if;
         Ops_Val (I) := Eval_Static_Expr (El);
         if Is_Overflow_Literal (Ops_Val (I)) then
            Err_Orig := El;
         else
            case Iir_Predefined_Concat_Functions (Def) is
               when Iir_Predefined_Array_Element_Concat
                 | Iir_Predefined_Element_Element_Concat =>
                  Res_Len := Res_Len + 1;
               when Iir_Predefined_Element_Array_Concat
                 | Iir_Predefined_Array_Array_Concat =>
                  Str_Lits (I) := Eval_String_Literal (Ops_Val (I));
                  El_List := Get_Simple_Aggregate_List (Str_Lits (I));
                  Res_Len := Res_Len + Get_Nbr_Elements (El_List);
            end case;
         end if;
      end loop;

      Op := Operands (Operands'Last);
      if Get_Kind (Op) = Iir_Kind_Function_Call then
         Left_Op := Get_Actual (Get_Parameter_Association_Chain (Op));
      else
         Left_Op := Get_Left (Op);
      end if;
      Left_Val := Eval_Static_Expr (Left_Op);
      if Is_Overflow_Literal (Left_Val) then
         Err_Orig := Left_Op;
      else
         Left_Def := Def;
         case Iir_Predefined_Concat_Functions (Left_Def) is
            when Iir_Predefined_Element_Array_Concat
              | Iir_Predefined_Element_Element_Concat =>
               Res_Len := Res_Len + 1;
            when Iir_Predefined_Array_Element_Concat
              | Iir_Predefined_Array_Array_Concat =>
               Left_Lit := Eval_String_Literal (Left_Val);
               El_List := Get_Simple_Aggregate_List (Left_Lit);
               Res_Len := Res_Len + Get_Nbr_Elements (El_List);
         end case;
      end if;

      --  Handle overflow.
      if Err_Orig /= Null_Iir then
         --  Free all.
         for I in Ops_Val'Range loop
            Free_Eval_Static_Expr (Ops_Val (I), Operands (I));
         end loop;
         Free_Eval_Static_Expr (Left_Val, Left_Op);

         return Build_Overflow (Err_Orig);
      end if;

      Res_List := Create_Iir_Flist (Res_Len);

      --  Do the concatenation.
      --  Left:
      Leftest_Non_Null := Null_Iir;
      case Iir_Predefined_Concat_Functions (Left_Def) is
         when Iir_Predefined_Element_Array_Concat
           | Iir_Predefined_Element_Element_Concat =>
            Set_Nth_Element (Res_List, 0, Left_Val);
            Bounds_From_Subtype := True;
            Res_Len := 1;
         when Iir_Predefined_Array_Element_Concat
           | Iir_Predefined_Array_Array_Concat =>
            El_List := Get_Simple_Aggregate_List (Left_Lit);
            Res_Len := Get_Nbr_Elements (El_List);
            for I in 0 .. Res_Len - 1 loop
               Set_Nth_Element (Res_List, I, Get_Nth_Element (El_List, I));
            end loop;
            Bounds_From_Subtype := Def = Iir_Predefined_Array_Element_Concat;
            if Res_Len > 0 then
               Leftest_Non_Null := Get_Type (Left_Lit);
            end if;
            Free_Eval_String_Literal (Left_Lit, Left_Val);
      end case;

      --  Right:
      for I in reverse Operands'Range loop
         Def := Get_Implicit_Definition (Get_Implementation (Operands (I)));
         case Iir_Predefined_Concat_Functions (Def) is
            when Iir_Predefined_Array_Element_Concat
              | Iir_Predefined_Element_Element_Concat =>
               Set_Nth_Element (Res_List, Res_Len, Ops_Val (I));
               Bounds_From_Subtype := True;
               Res_Len := Res_Len + 1;
            when Iir_Predefined_Element_Array_Concat
              | Iir_Predefined_Array_Array_Concat =>
               El_List := Get_Simple_Aggregate_List (Str_Lits (I));
               El_Len := Get_Nbr_Elements (El_List);
               for I in 0 .. El_Len - 1 loop
                  Set_Nth_Element
                    (Res_List, Res_Len + I, Get_Nth_Element (El_List, I));
               end loop;
               Bounds_From_Subtype := Bounds_From_Subtype
                 or Def = Iir_Predefined_Element_Array_Concat;
               if Leftest_Non_Null = Null_Iir and then El_Len /= 0 then
                  Leftest_Non_Null := Get_Type (Ops_Val (I));
               end if;
               Free_Eval_String_Literal (Str_Lits (I), Ops_Val (I));
               Res_Len := Res_Len + El_Len;
         end case;
      end loop;

      --  Compute subtype...
      if Flags.Vhdl_Std > Vhdl_87 then
         --  LRM93 7.2.4
         --  If both operands are null arrays, then the result of the
         --  concatenation is the right operand.
         if Res_Len = 0 then
            Res_Type := Get_Type (Get_Right (Operands (1)));
         else
            --  LRM93 7.2.4
            --  Otherwise, the direction and bounds of the result are
            --  determined as follows: let S be the index subtype of the base
            --  type of the result.  The direction of the result of the
            --  concatenation is the direction of S, and the left bound of the
            --  result is S'LEFT.
            Res_Type := Create_Unidim_Array_By_Length
              (Origin_Type, Int64 (Res_Len), Orig);
         end if;
      else
         --  LRM87 7.2.3
         --  The left bound of the result is the left operand, [...]
         --
         --  LRM87 7.2.3
         --  The direction of the result is the direction of the left
         --  operand, [...]
         --
         --  LRM87 7.2.3
         --  [...], unless the left operand is a null array, in which case
         --  the result of the concatenation is the right operand.

         --  Look for the first operand that is either an element or
         --  a non-null array.  If it is an element, create the bounds
         --  by length.  If it is an array, create the bounds from it.  If
         --  there is no such operand, use the leftest operands for the
         --  bounds.
         if Bounds_From_Subtype then
            --  There is at least one concatenation with an element.
            Res_Type := Create_Unidim_Array_By_Length
              (Origin_Type, Int64 (Res_Len), Orig);
         else
            if Res_Len = 0 then
               Res_Type := Get_Type (Get_Right (Operands (1)));
            else
               declare
                  Left_Index : constant Iir :=
                    Get_Index_Type (Leftest_Non_Null, 0);
                  Left_Range : constant Iir :=
                    Get_Range_Constraint (Left_Index);
                  Ret_Type : constant Iir :=
                    Get_Return_Type (Get_Implementation (Orig));
                  Rng_Type : constant Iir := Get_Index_Type (Ret_Type, 0);
                  A_Range : Iir;
                  Index_Type : Iir;
               begin
                  A_Range := Create_Iir (Iir_Kind_Range_Expression);
                  Location_Copy (A_Range, Orig);
                  Set_Type (A_Range, Rng_Type);
                  Set_Expr_Staticness (A_Range, Locally);
                  Set_Left_Limit (A_Range, Get_Left_Limit (Left_Range));
                  Set_Direction (A_Range, Get_Direction (Left_Range));
                  Set_Right_Limit_By_Length (A_Range, Int64 (Res_Len));

                  Index_Type := Create_Range_Subtype_From_Type
                    (Rng_Type, Get_Location (Orig));
                  Set_Range_Constraint (Index_Type, A_Range);
                  Res_Type := Create_Unidim_Array_From_Index
                    (Origin_Type, Index_Type, Orig);
               end;
            end if;
         end if;
      end if;

      for I in Ops_Val'Range loop
         Free_Eval_Static_Expr (Ops_Val (I), Operands (I));
      end loop;
      Free_Eval_Static_Expr (Left_Val, Left_Op);

      --  FIXME: this is not necessarily a string, it may be an aggregate if
      --  element type is not a character type.
      return Build_Simple_Aggregate (Res_List, Orig, Res_Type, Res_Type);
   end Eval_Concatenation;

   function Eval_Scalar_Compare (Left, Right : Iir) return Compare_Type
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
         when Iir_Kind_Physical_Type_Definition =>
            declare
               L_Val : constant Int64 := Get_Physical_Value (Left);
               R_Val : constant Int64 := Get_Physical_Value (Right);
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
         when Iir_Kind_Integer_Type_Definition =>
            declare
               L_Val : constant Int64 := Get_Value (Left);
               R_Val : constant Int64 := Get_Value (Right);
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
         when Iir_Kind_Floating_Type_Definition =>
            declare
               L_Val : constant Fp64 := Get_Fp_Value (Left);
               R_Val : constant Fp64 := Get_Fp_Value (Right);
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
            Error_Kind ("eval_scalar_compare", Ltype);
      end case;
   end Eval_Scalar_Compare;

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
            R_List, L_List : Iir_Flist;
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
               Res := Eval_Scalar_Compare (Get_Nth_Element (L_List, P),
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

   function Eval_Logic_Match_Equality (L, R : Iir_Int32; Loc : Iir)
                                      return Iir_Index32
   is
      use Vhdl.Ieee.Std_Logic_1164;
      Lb, Rb : Boolean;
   begin
      if L = Std_Logic_D_Pos or R = Std_Logic_D_Pos then
         Warning_Msg_Sem
           (Warnid_Analyze_Assert, +Loc,
            "STD_LOGIC_1164: '-' operand for matching ordering operator");
         return Std_Logic_1_Pos;
      end if;
      if L = Std_Logic_U_Pos or R = Std_Logic_U_Pos then
         return Std_Logic_U_Pos;
      end if;
      if L = Std_Logic_X_Pos
        or L = Std_Logic_Z_Pos
        or L = Std_Logic_W_Pos
      then
         return Std_Logic_X_Pos;
      end if;
      if R = Std_Logic_X_Pos
        or R = Std_Logic_Z_Pos
        or R = Std_Logic_W_Pos
      then
         return Std_Logic_X_Pos;
      end if;
      Lb := L = Std_Logic_1_Pos or L = Std_Logic_H_Pos;
      Rb := R = Std_Logic_1_Pos or R = Std_Logic_H_Pos;
      if Lb = Rb then
         return Std_Logic_1_Pos;
      else
         return Std_Logic_0_Pos;
      end if;
   end Eval_Logic_Match_Equality;

   function Eval_Logic_Or (L, R : Iir_Index32) return Iir_Index32
   is
      use Vhdl.Ieee.Std_Logic_1164;
   begin
      if L = Std_Logic_1_Pos or L = Std_Logic_H_Pos
        or R = Std_Logic_1_Pos or R = Std_Logic_H_Pos
      then
         return Std_Logic_1_Pos;
      elsif (L = Std_Logic_0_Pos or L = Std_Logic_L_Pos)
        and (R = Std_Logic_0_Pos or R = Std_Logic_L_Pos)
      then
         return Std_Logic_0_Pos;
      elsif L = Std_Logic_U_Pos or R = Std_Logic_U_Pos then
         return Std_Logic_U_Pos;
      else
         return Std_Logic_X_Pos;
      end if;
   end Eval_Logic_Or;

   function Eval_Logic_Not (X : Iir_Index32) return Iir_Index32
   is
      use Vhdl.Ieee.Std_Logic_1164;
   begin
      if X = Std_Logic_0_Pos or X = Std_Logic_L_Pos then
         return Std_Logic_1_Pos;
      elsif X = Std_Logic_1_Pos or X = Std_Logic_H_Pos then
         return Std_Logic_0_Pos;
      elsif X = Std_Logic_U_Pos then
         return Std_Logic_U_Pos;
      else
         return Std_Logic_X_Pos;
      end if;
   end Eval_Logic_Not;

   function Eval_Logic_Match_Inequality (L, R : Iir_Int32; Loc : Iir)
                                         return Iir_Index32
   is
      E : Iir_Index32;
   begin
      --  Defined as the not operator applied to the equal operator
      E := Eval_Logic_Match_Equality (L, R, Loc);
      return Eval_Logic_Not (E);
   end Eval_Logic_Match_Inequality;

   function Eval_Logic_Match_Less (L, R : Iir_Int32; Loc : Iir)
                                   return Iir_Index32
   is
      use Vhdl.Ieee.Std_Logic_1164;
   begin
      --  LRM19 9.2.3 table
      --  '-' always returns 'X'
      if L = Std_Logic_D_Pos or R = Std_Logic_D_Pos then
         Warning_Msg_Sem
           (Warnid_Analyze_Assert, +Loc,
            "STD_LOGIC_1164: '-' operand for matching ordering operator");
         return Std_Logic_X_Pos;
      end if;

      --  'U' always returns 'U'
      if L = Std_Logic_U_Pos or R = Std_Logic_U_Pos then
         return Std_Logic_U_Pos;
      end if;

      --  Only when R is '1' or 'H' will we ever return '1'
      if R = Std_Logic_1_Pos or R = Std_Logic_H_Pos then
         if L = Std_Logic_0_Pos or L = Std_Logic_L_Pos then
            --  L = [0,L] R = [1,H]
            return Std_Logic_1_Pos;
         elsif L = Std_Logic_1_Pos or L = Std_Logic_H_Pos then
            --  L = [1,H] R = [1,H]
            return Std_Logic_0_Pos;
         else
            --  Everything else is 'X'
            return Std_Logic_X_Pos;
         end if;
      elsif R = Std_Logic_0_Pos or R = Std_Logic_L_Pos then
         --  R = [0,1]
         return Std_Logic_0_Pos;
      else
         --  Everything else is 'X'
         return Std_Logic_X_Pos;
      end if;
   end Eval_Logic_Match_Less;

   function Eval_Logic_Match_Less_Equal (L, R : Iir_Int32; Loc : Iir)
                                         return Iir_Index32
   is
      Less : Iir_Index32;
      Equal : Iir_Index32;
   begin
      --  LRM19 9.2.3
      --  ?<= is defined as (< or =)
      Less := Eval_Logic_Match_Less (L, R, Loc);
      Equal := Eval_Logic_Match_Equality (L, R, Loc);
      return Eval_Logic_Or (Less, Equal);
   end Eval_Logic_Match_Less_Equal;

   function Eval_Logic_Match_Greater (L, R : Iir_Int32; Loc : Iir)
                                      return Iir_Index32
   is
      Le : Iir_Index32;
   begin
      --  LRM19 9.2.3
      --  ?> is defined as not(?<=)
      Le := Eval_Logic_Match_Less_Equal (L, R, Loc);
      return Eval_Logic_Not (Le);
   end Eval_Logic_Match_Greater;

   function Eval_Logic_Match_Greater_Equal (L, R : Iir_Int32; Loc : Iir)
                                            return Iir_Index32
   is
      Less : Iir_Index32;
   begin
      --  LRM19 9.2.3
      --  ?>= is defined as not(?<)
      Less := Eval_Logic_Match_Less (L, R, Loc);
      return Eval_Logic_Not (Less);
   end Eval_Logic_Match_Greater_Equal;

   function Eval_Equality (Left, Right : Iir) return Boolean;

   --  CHOICES is a chain of choice from a record aggregate; FEL is an Flist
   --  whose length is the number of element of the record type.
   --  Fill FEL with the associated expressions from CHOICES, so that it is
   --  easier to deal than the aggregate as elements are ordered.
   procedure Fill_Flist_From_Record_Aggregate (Choices : Iir; Fel : Iir_Flist)
   is
      Pos : Natural;
      Ch : Iir;
      Expr : Iir;
   begin
      Pos := 0;
      Ch := Choices;
      while Ch /= Null_Iir loop
         Expr := Get_Associated_Expr (Ch);
         case Iir_Kinds_Record_Choice (Get_Kind (Ch)) is
            when Iir_Kind_Choice_By_None =>
               Set_Nth_Element (Fel, Pos, Expr);
               Pos := Pos + 1;
            when Iir_Kind_Choice_By_Name =>
               Pos := Natural (Get_Element_Position
                                 (Get_Named_Entity (Get_Choice_Name (Ch))));
               Set_Nth_Element (Fel, Pos, Expr);
            when Iir_Kind_Choice_By_Others =>
               for I in 0 .. Get_Nbr_Elements (Fel) - 1 loop
                  if Get_Nth_Element (Fel, I) = Null_Iir then
                     Set_Nth_Element (Fel, I, Expr);
                  end if;
               end loop;
         end case;
         Ch := Get_Chain (Ch);
      end loop;
   end Fill_Flist_From_Record_Aggregate;

   function Eval_Record_Equality (Left, Right : Iir) return Boolean
   is
      pragma Assert (Get_Kind (Left) = Iir_Kind_Aggregate);
      pragma Assert (Get_Kind (Right) = Iir_Kind_Aggregate);
      Lch, Rch : Iir;
   begin
      Lch := Get_Association_Choices_Chain (Left);
      Rch := Get_Association_Choices_Chain (Right);

      if Get_Kind (Lch) = Iir_Kind_Choice_By_None
        and then Get_Kind (Rch) = Iir_Kind_Choice_By_None
      then
         --  All choices are positionnal.
         while Lch /= Null_Iir loop
            pragma Assert (Rch /= Null_Iir);
            pragma Assert (Get_Kind (Lch) = Iir_Kind_Choice_By_None);
            pragma Assert (Get_Kind (Rch) = Iir_Kind_Choice_By_None);
            if not Eval_Equality (Get_Associated_Expr (Lch),
                                  Get_Associated_Expr (Rch))
            then
               return False;
            end if;
            Lch := Get_Chain (Lch);
            Rch := Get_Chain (Rch);
         end loop;
         pragma Assert (Rch = Null_Iir);
         return True;
      else
         declare
            Els : constant Iir_Flist :=
              Get_Elements_Declaration_List (Get_Type (Left));
            Nels : constant Natural := Get_Nbr_Elements (Els);
            Lel, Rel : Iir_Flist;
            Res : Boolean;
         begin
            Lel := Create_Iir_Flist (Nels);
            Rel := Create_Iir_Flist (Nels);
            Fill_Flist_From_Record_Aggregate (Lch, Lel);
            Fill_Flist_From_Record_Aggregate (Rch, Rel);

            Res := True;
            for I in 0 .. Nels - 1 loop
               if not Eval_Equality (Get_Nth_Element (Lel, I),
                                     Get_Nth_Element (Rel, I))
               then
                  Res := False;
                  exit;
               end if;
            end loop;

            Destroy_Iir_Flist (Lel);
            Destroy_Iir_Flist (Rel);

            return Res;
         end;
      end if;
   end Eval_Record_Equality;

   function Eval_Equality (Left, Right : Iir) return Boolean
   is
      Ltype : constant Iir := Get_Base_Type (Get_Type (Left));
   begin
      pragma Assert
        (Get_Kind (Ltype) = Get_Kind (Get_Base_Type (Get_Type (Right))));

      case Get_Kind (Ltype) is
         when Iir_Kind_Enumeration_Type_Definition =>
            return Get_Enum_Pos (Left) = Get_Enum_Pos (Right);
         when Iir_Kind_Physical_Type_Definition =>
            return Get_Physical_Value (Left) = Get_Physical_Value (Right);
         when Iir_Kind_Integer_Type_Definition =>
            return Get_Value (Left) = Get_Value (Right);
         when Iir_Kind_Floating_Type_Definition =>
            return Get_Fp_Value (Left) = Get_Fp_Value (Right);
         when Iir_Kind_Array_Type_Definition =>
            return Eval_Array_Compare (Left, Right) = Compare_Eq;
         when Iir_Kind_Record_Type_Definition =>
            return Eval_Record_Equality (Left, Right);
         when others =>
            Error_Kind ("eval_equality", Ltype);
      end case;
   end Eval_Equality;

   --  ORIG is either a dyadic operator or a function call.
   function Eval_Dyadic_Operator (Orig : Iir; Imp : Iir; Left, Right : Iir)
                                 return Iir
   is
      pragma Unsuppress (Overflow_Check);
      Func : constant Iir_Predefined_Functions :=
        Get_Implicit_Definition (Imp);
   begin
      if Is_Overflow_Literal (Left) or else Is_Overflow_Literal (Right) then
         return Build_Overflow (Orig);
      end if;

      case Func is
         when Iir_Predefined_Integer_Plus =>
            return Build_Integer_Check
              (Get_Value (Left) + Get_Value (Right), Orig);
         when Iir_Predefined_Integer_Minus =>
            return Build_Integer_Check
              (Get_Value (Left) - Get_Value (Right), Orig);
         when Iir_Predefined_Integer_Mul =>
            return Build_Integer_Check
              (Get_Value (Left) * Get_Value (Right), Orig);
         when Iir_Predefined_Integer_Div =>
            if Check_Integer_Division_By_Zero (Orig, Right) then
               return Build_Integer_Check
                 (Get_Value (Left) / Get_Value (Right), Orig);
            else
               return Build_Overflow (Orig);
            end if;
         when Iir_Predefined_Integer_Mod =>
            if Check_Integer_Division_By_Zero (Orig, Right) then
               return Build_Integer_Check
                 (Get_Value (Left) mod Get_Value (Right), Orig);
            else
               return Build_Overflow (Orig);
            end if;
         when Iir_Predefined_Integer_Rem =>
            if Check_Integer_Division_By_Zero (Orig, Right) then
               return Build_Integer_Check
                 (Get_Value (Left) rem Get_Value (Right), Orig);
            else
               return Build_Overflow (Orig);
            end if;
         when Iir_Predefined_Integer_Exp =>
            declare
               Exp : Int64;
               Val : Int64;
               Res : Int64;
            begin
               Val := Get_Value (Left);
               --  LRM08 9.2.8 Misellaneous operators
               --  Exponentiation with a negative exponent is only allowed for
               --  a list operand of a floating-point type.
               Exp := Get_Value (Right);
               if Exp < 0 then
                  raise Constraint_Error;
               end if;
               --  LRM08 9.2.8 Misellaneous operators
               --  Exponentiation with an integer exponent is equivalent to
               --  repeated multiplication of the left operand by itself for
               --  a number of times indicated by the absolute value of the
               --  exponent and from left to right; [...]
               --  GHDL: use the standard power-of-2 approach.  This is not
               --  strictly equivalent however.
               Res := 1;
               loop
                  if Exp mod 2 = 1 then
                     Res := Res * Val;
                  end if;
                  Exp := Exp / 2;
                  exit when Exp = 0;
                  Val := Val * Val;
               end loop;
               return Build_Integer_Check (Res, Orig);
            end;

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
               Warning_Msg_Sem (Warnid_Runtime_Error, +Orig,
                                "right operand of division is 0");
               return Build_Overflow (Orig);
            else
               return Build_Floating
                 (Get_Fp_Value (Left) / Get_Fp_Value (Right), Orig);
            end if;
         when Iir_Predefined_Floating_Exp =>
            declare
               Exp : Int64;
               Res : Fp64;
               Val : Fp64;
            begin
               Res := 1.0;
               Val := Get_Fp_Value (Left);
               --  LRM08 9.2.8 Misellaneous operators
               --  Exponentiation with an integer exponent is equivalent to
               --  repeated multiplication of the left operand by itself for
               --  a number of times indicated by the absolute value of the
               --  exponent and from left to right; [...]
               --  GHDL: use the standard power-of-2 approach.  This is not
               --  strictly equivalent however.
               Exp := abs Get_Value (Right);
               while Exp /= 0 loop
                  if Exp mod 2 = 1 then
                     Res := Res * Val;
                  end if;
                  Exp := Exp / 2;
                  Val := Val * Val;
               end loop;
               --  LRM08 9.2.8 Misellaneous operators
               --  [...] if the exponent is negative then the result is the
               --  reciprocal of that [...]
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
              (Int64 (Get_Fp_Value (Left)
                          * Fp64 (Get_Physical_Value (Right))), Orig);
         when Iir_Predefined_Physical_Real_Mul =>
            --  FIXME: overflow??
            return Build_Physical
              (Int64 (Fp64 (Get_Physical_Value (Left))
                          * Get_Fp_Value (Right)), Orig);
         when Iir_Predefined_Physical_Real_Div =>
            --  FIXME: overflow??
            return Build_Physical
              (Int64 (Fp64 (Get_Physical_Value (Left))
                          / Get_Fp_Value (Right)), Orig);

         when Iir_Predefined_Physical_Mod =>
            return Build_Physical
              (Get_Physical_Value (Left) mod Get_Value (Right), Orig);
         when Iir_Predefined_Physical_Rem =>
            return Build_Physical
              (Get_Physical_Value (Left) rem Get_Value (Right), Orig);

         when Iir_Predefined_Physical_Minimum =>
            return Build_Physical (Int64'Min (Get_Physical_Value (Left),
                                                  Get_Physical_Value (Right)),
                                   Orig);
         when Iir_Predefined_Physical_Maximum =>
            return Build_Physical (Int64'Max (Get_Physical_Value (Left),
                                                  Get_Physical_Value (Right)),
                                   Orig);

         when Iir_Predefined_Element_Array_Concat
           | Iir_Predefined_Array_Element_Concat
           | Iir_Predefined_Array_Array_Concat
           | Iir_Predefined_Element_Element_Concat =>
            raise Internal_Error;

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
              (Get_Fp_Value (Left) * Fp64 (Get_Value (Right)), Orig);
         when Iir_Predefined_Universal_I_R_Mul =>
            return Build_Floating
              (Fp64 (Get_Value (Left)) * Get_Fp_Value (Right), Orig);
         when Iir_Predefined_Universal_R_I_Div =>
            return Build_Floating
              (Get_Fp_Value (Left) / Fp64 (Get_Value (Right)), Orig);

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

         when Iir_Predefined_Record_Equality =>
            return Build_Boolean (Eval_Record_Equality (Left, Right));
         when Iir_Predefined_Record_Inequality =>
            return Build_Boolean (not Eval_Record_Equality (Left, Right));

         when Iir_Predefined_Real_To_String_Format =>
            return Eval_Floating_To_String_Format
              (Get_Fp_Value (Left), Right, Orig);

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
           | Iir_Predefined_Access_Equality
           | Iir_Predefined_Access_Inequality
           | Iir_Predefined_TF_Array_Not
           | Iir_Predefined_Now_Function
           | Iir_Predefined_Real_Now_Function
           | Iir_Predefined_Frequency_Function
           | Iir_Predefined_Deallocate
           | Iir_Predefined_Write
           | Iir_Predefined_Read
           | Iir_Predefined_Read_Length
           | Iir_Predefined_Flush
           | Iir_Predefined_File_Open
           | Iir_Predefined_File_Open_Status
           | Iir_Predefined_File_Close
           | Iir_Predefined_Endfile
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

         when Iir_Predefined_Std_Ulogic_Match_Equality =>
            return Build_Enumeration
              (Eval_Logic_Match_Equality (Get_Enum_Pos (Left),
                                          Get_Enum_Pos (Right), Orig),
               Orig);

         when Iir_Predefined_Std_Ulogic_Match_Inequality =>
            return Build_Enumeration
              (Eval_Logic_Match_Inequality (Get_Enum_Pos (Left),
                                            Get_Enum_Pos (Right), Orig),
               Orig);

         when Iir_Predefined_Std_Ulogic_Match_Less =>
            return Build_Enumeration
              (Eval_Logic_Match_Less (Get_Enum_Pos (Left),
                                      Get_Enum_Pos (Right), Orig),
               Orig);

         when Iir_Predefined_Std_Ulogic_Match_Greater =>
            return Build_Enumeration
              (Eval_Logic_Match_Greater (Get_Enum_Pos (Left),
                                         Get_Enum_Pos (Right), Orig),
               Orig);

         when Iir_Predefined_Std_Ulogic_Match_Greater_Equal =>
            return Build_Enumeration
              (Eval_Logic_Match_Greater_Equal (Get_Enum_Pos (Left),
                                               Get_Enum_Pos (Right), Orig),
               Orig);

         when Iir_Predefined_Std_Ulogic_Match_Less_Equal =>
            return Build_Enumeration
              (Eval_Logic_Match_Less_Equal (Get_Enum_Pos (Left),
                                            Get_Enum_Pos (Right), Orig),
               Orig);

         when Iir_Predefined_Real_To_String_Digits =>
            return Eval_Predefined_Call (Orig, Orig, Left, Right);
         when Iir_Predefined_Time_To_String_Unit =>
            --  TODO: to_string with a format parameter
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
            return Eval_Ieee_Operation (Orig, Imp, Left, Right);

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
            return Eval_Ieee_Operation (Orig, Imp, Left, Right);

         when Iir_Predefined_Enum_To_String
           | Iir_Predefined_Integer_To_String
           | Iir_Predefined_Floating_To_String
           | Iir_Predefined_Physical_To_String =>
            --  Not dyadic
            raise Internal_Error;

         when Iir_Predefined_IEEE_Explicit =>
            return Eval_Ieee_Operation (Orig, Imp, Left, Right);

         when Iir_Predefined_None =>
            --  Not static
            raise Internal_Error;
      end case;
   exception
      when Constraint_Error =>
         Warning_Msg_Sem (Warnid_Runtime_Error, +Orig,
                          "arithmetic overflow in static expression");
         return Build_Overflow (Orig);
   end Eval_Dyadic_Operator;

   --  Get the parameter of an attribute, or 1 if doesn't exist.
   function Eval_Attribute_Parameter_Or_1 (Attr : Iir) return Natural
   is
      Parameter : constant Iir := Get_Parameter (Attr);
   begin
      if Is_Null (Parameter) or else Is_Error (Parameter) then
         return 1;
      else
         return Natural (Get_Value (Parameter));
      end if;
   end Eval_Attribute_Parameter_Or_1;

   --  Evaluate any array attribute, return the type for the prefix.
   function Eval_Array_Attribute (Attr : Iir) return Iir
   is
      Prefix : Iir;
      Prefix_Type : Iir;
      Dim : Natural;
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
           | Iir_Kind_Attribute_Value
           | Iir_Kind_Attribute_Name
           | Iir_Kind_Subtype_Attribute
           | Iir_Kind_Element_Attribute =>
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

      Dim := Eval_Attribute_Parameter_Or_1 (Attr);
      return Get_Nth_Element (Get_Index_Subtype_List (Prefix_Type), Dim - 1);
   end Eval_Array_Attribute;

   function Eval_Integer_Image (Val : Int64; Orig : Iir) return Iir
   is
      Img : String (1 .. 24); --  23 is enough, 24 is rounded.
      L : Natural;
      V : Int64;
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
      return Build_String (Img (L + 1 .. Img'Last), Orig);
   end Eval_Integer_Image;

   function Eval_Floating_Image (Val : Fp64; Orig : Iir) return Iir
   is
      --  Sign (1) + digit (1) + dot (1) + digits (15) + 'e' (1) + sign (1)
      --  + exp_digits (4) -> 24.
      Str : String (1 .. 25);
      P : Natural;

      Res : Iir;
   begin
      P := Str'First;

      Grt.Fcvt.Format_Image (Str, P, Interfaces.IEEE_Float_64 (Val));

      Res := Build_String (Str (1 .. P), Orig);
      --  FIXME: this is not correct since the type is *not* constrained.
      Set_Type (Res, Create_Unidim_Array_By_Length
                (Get_Type (Orig), Int64 (P), Orig));
      return Res;
   end Eval_Floating_Image;

   function Eval_Floating_To_String_Format (Val : Fp64; Fmt : Iir; Orig : Iir)
                                           return Iir
   is
      pragma Assert (Get_Kind (Fmt) = Iir_Kind_String_Literal8);
      Fmt_Len : constant Int32 := Get_String_Length (Fmt);
   begin
      if Fmt_Len > 32 then
         Warning_Msg_Sem (Warnid_Runtime_Error, +Orig,
                          "format parameter too long");
         return Build_Overflow (Orig);
      end if;
      declare
         use Str_Table;
         use Grt.Types;
         use Grt.To_Strings;
         Fmt_Id : constant String8_Id := Get_String8_Id (Fmt);
         Fmt_Str : String (1 .. Natural (Fmt_Len) + 1);

         Res : String_Real_Format;
         Last : Natural;
      begin
         for I in 1 .. Fmt_Len loop
            Fmt_Str (Positive (I)) := Char_String8 (Fmt_Id, I);
         end loop;
         Fmt_Str (Fmt_Str'Last) := ASCII.NUL;

         Grt.To_Strings.To_String
           (Res, Last, Ghdl_F64 (Val), To_Ghdl_C_String (Fmt_Str'Address));

         return Build_String (Res (1 .. Last), Orig);
      end;
   end Eval_Floating_To_String_Format;

   function Eval_Enumeration_Image (Lit : Iir; Orig : Iir) return Iir
   is
      Name : constant String := Image_Identifier (Lit);
   begin
      return Build_String (Name, Orig);
   end Eval_Enumeration_Image;

   function Build_Enumeration_Value (Val : String; Enum, Expr : Iir) return Iir
   is
      List  : constant Iir_Flist := Get_Enumeration_Literal_List (Enum);
      Value : String (Val'range);
      Id : Name_Id;
      Res : Iir;
   begin
      if Val'Length = 3
        and then Val (Val'First) = ''' and then Val (Val'Last) = '''
      then
         --  A single character.
         Id := Get_Identifier (Val (Val'First + 1));
      else
         for I in Val'range loop
            Value (I) := Ada.Characters.Handling.To_Lower (Val (I));
         end loop;
         Id := Get_Identifier (Value);
      end if;
      Res := Find_Name_In_Flist (List, Id);
      if Res /= Null_Iir then
         return Build_Constant (Res, Expr);
      else
         Warning_Msg_Sem (Warnid_Runtime_Error, +Expr,
                          "value %i not in enumeration %n", (+Id, +Enum));
         return Build_Overflow (Expr);
      end if;
   end Build_Enumeration_Value;

   function Eval_Physical_Image (Phys, Expr: Iir) return Iir
   is
      --  Reduces to the base unit (e.g. femtoseconds).
      Value : constant String := Int64'Image (Get_Physical_Value (Phys));
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
      UnitName : String (Val'range);
      Mult : Int64;
      Sep : Natural;
      Found_Unit : Boolean := false;
      Found_Real : Boolean := false;
      Unit : Iir;
   begin
      -- Separate string into numeric value and make lowercase unit.
      for I in reverse Val'range loop
         UnitName (I) := Ada.Characters.Handling.To_Lower (Val (I));
         if Vhdl.Scanner.Is_Whitespace (Val (I)) and Found_Unit then
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
         Warning_Msg_Sem (Warnid_Runtime_Error, +Expr,
                          "Unit """ & UnitName (Sep + 1 .. UnitName'Last)
                            & """ not in physical type");
         return Build_Overflow (Expr);
      end if;

      Mult := Get_Value (Get_Physical_Literal (Unit));
      if Found_Real then
         return Build_Physical
           (Int64 (Fp64'Value (Val (Val'First .. Sep))
                         * Fp64 (Mult)),
            Expr);
      else
         return Build_Physical
           (Int64'Value (Val (Val'First .. Sep)) * Mult, Expr);
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
         declare
            Img : constant String := Image (Id);
         begin
            if Img (Img'First) /= '\' then
               Append_String8_String (Img);
               Len := Img'Length;
            else
               declare
                  Skip : Boolean;
                  C : Character;
               begin
                  Len := 0;
                  Skip := False;
                  for I in Img'First + 1 .. Img'Last - 1 loop
                     if Skip then
                        Skip := False;
                     else
                        C := Img (I);
                        Append_String8_Char (C);
                        Skip := C = '\';
                        Len := Len + 1;
                     end if;
                  end loop;
               end;
            end if;
         end;
      end if;
      return Build_String (Image_Id, Nat32 (Len), Orig);
   end Eval_Enum_To_String;

   function Eval_Incdec (Expr : Iir; N : Int64; Origin : Iir) return Iir
   is
      P : Int64;
   begin
      case Get_Kind (Expr) is
         when Iir_Kind_Integer_Literal =>
            return Build_Integer (Get_Value (Expr) + N, Origin);
         when Iir_Kind_Enumeration_Literal =>
            P := Int64 (Get_Enum_Pos (Expr)) + N;
            if P < 0
              or else (P >= Int64
                         (Get_Nbr_Elements
                            (Get_Enumeration_Literal_List
                               (Get_Base_Type (Get_Type (Expr))))))
            then
               Warning_Msg_Sem (Warnid_Runtime_Error, +Expr,
                                "static constant violates bounds");
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
      Lit : Iir;
   begin
      Res_Btype := Get_Base_Type (Res_Type);
      Res := Create_Iir (Iir_Kind_Range_Expression);
      Location_Copy (Res, Loc);
      Set_Type (Res, Res_Btype);
      Lit := Create_Bound (Get_Left_Limit (Rng));
      Set_Left_Limit (Res, Lit);
      Set_Left_Limit_Expr (Res, Lit);
      Lit := Create_Bound (Get_Right_Limit (Rng));
      Set_Right_Limit (Res, Lit);
      Set_Right_Limit_Expr (Res, Lit);
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
         if not Eval_Is_In_Bound (Val, Conv_Type, True) then
            Warning_Msg_Sem (Warnid_Runtime_Error, +Conv,
                             "non matching length in type conversion");
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
            Set_Parent_Type (Index_Type, Conv_Index_Type);
            Set_Type_Staticness (Index_Type, Locally);
         end if;
         Res_Type := Create_Unidim_Array_From_Index
           (Get_Base_Type (Conv_Type), Index_Type, Conv);
         Set_Type (Res, Res_Type);
         Set_Type_Conversion_Subtype (Conv, Res_Type);
         return Res;
      end if;
   end Eval_Array_Type_Conversion;

   function Eval_Type_Conversion (Conv : Iir) return Iir
   is
      Expr : constant Iir := Get_Expression (Conv);
      Val : Iir;
      Val_Type : Iir;
      Conv_Type : Iir;
      Res : Iir;
   begin
      Val := Eval_Static_Expr (Expr);
      Val_Type := Get_Base_Type (Get_Type (Val));
      Conv_Type := Get_Base_Type (Get_Type (Conv));
      if Conv_Type = Val_Type then
         Res := Build_Constant (Val, Conv);
      else
         case Get_Kind (Conv_Type) is
            when Iir_Kind_Integer_Type_Definition =>
               case Get_Kind (Val_Type) is
                  when Iir_Kind_Integer_Type_Definition =>
                     Res := Build_Integer (Get_Value (Val), Conv);
                  when Iir_Kind_Floating_Type_Definition =>
                     Res := Build_Integer
                       (Int64 (Get_Fp_Value (Val)), Conv);
                  when others =>
                     Error_Kind ("eval_type_conversion(1)", Val_Type);
               end case;
            when Iir_Kind_Floating_Type_Definition =>
               case Get_Kind (Val_Type) is
                  when Iir_Kind_Integer_Type_Definition =>
                     Res := Build_Floating (Fp64 (Get_Value (Val)), Conv);
                  when Iir_Kind_Floating_Type_Definition =>
                     Res := Build_Floating (Get_Fp_Value (Val), Conv);
                  when others =>
                     Error_Kind ("eval_type_conversion(2)", Val_Type);
               end case;
            when Iir_Kind_Array_Type_Definition =>
               --  Not a scalar, do not check bounds.
               return Eval_Array_Type_Conversion (Conv, Val);
            when others =>
               Error_Kind ("eval_type_conversion(3)", Conv_Type);
         end case;
      end if;
      if not Eval_Is_In_Bound (Res, Get_Type (Conv), True) then
         Warning_Msg_Sem (Warnid_Runtime_Error, +Conv,
                          "result of conversion out of bounds");
         Free_Eval_Static_Expr (Res, Conv);
         Res := Build_Overflow (Conv);
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
            --  Create a copy even if the literal has the primary unit.  This
            --  is required for ownership rule.
            Val := Expr;
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

   function Eval_Value_Attribute
     (Value : String; Atype : Iir; Orig : Iir) return Iir
   is
      Base_Type : constant Iir := Get_Base_Type (Atype);
      First, Last : Positive;
   begin
      --  LRM93 14.1 Predefined attributes.
      --  Leading and trailing whitespace are ignored.
      First := Value'First;
      Last := Value'Last;
      while First <= Last loop
         exit when not Vhdl.Scanner.Is_Whitespace (Value (First));
         First := First + 1;
      end loop;
      while Last >= First loop
         exit when not Vhdl.Scanner.Is_Whitespace (Value (Last));
         Last := Last - 1;
      end loop;

      --  TODO: do not use 'value, use the same function as the scanner.
      declare
         Value1 : String renames Value (First .. Last);
      begin
         case Get_Kind (Base_Type) is
            when Iir_Kind_Integer_Type_Definition =>
               declare
                  use Grt.To_Strings;
                  use Grt.Types;
                  use Grt.Vhdl_Types;
                  Res : Value_I64_Result;
               begin
                  Res := Value_I64 (To_Std_String_Basep (Value1'Address),
                                    Value1'Length, 0);
                  if Res.Status = Value_Ok then
                     return Build_Discrete (Int64 (Res.Val), Orig);
                  else
                     Warning_Msg_Sem
                       (Warnid_Runtime_Error, +Get_Parameter (Orig),
                        "incorrect parameter for value attribute");
                     return Build_Overflow (Orig);
                  end if;
               end;
            when Iir_Kind_Enumeration_Type_Definition =>
               return Build_Enumeration_Value (Value1, Base_Type, Orig);
            when Iir_Kind_Floating_Type_Definition =>
               return Build_Floating (Fp64'Value (Value1), Orig);
            when Iir_Kind_Physical_Type_Definition =>
               return Build_Physical_Value (Value1, Base_Type, Orig);
            when others =>
               Error_Kind ("eval_value_attribute", Base_Type);
         end case;
      end;
   end Eval_Value_Attribute;

   --  Be sure that all expressions within an aggregate have been evaluated.
   procedure Eval_Aggregate (Aggr : Iir)
   is
      Assoc : Iir;
      Expr : Iir;
   begin
      Assoc := Get_Association_Choices_Chain (Aggr);
      while Is_Valid (Assoc) loop
         case Iir_Kinds_Choice (Get_Kind (Assoc)) is
            when Iir_Kind_Choice_By_None =>
               null;
            when Iir_Kind_Choice_By_Name =>
               null;
            when Iir_Kind_Choice_By_Range =>
               Set_Choice_Range
                 (Assoc, Eval_Range (Get_Choice_Range (Assoc)));
            when Iir_Kind_Choice_By_Expression =>
               Set_Choice_Expression
                 (Assoc, Eval_Expr (Get_Choice_Expression (Assoc)));
            when Iir_Kind_Choice_By_Others =>
               null;
         end case;
         if not Get_Same_Alternative_Flag (Assoc) then
            Expr := Get_Associated_Expr (Assoc);
         end if;
         if Get_Kind (Expr) = Iir_Kind_Aggregate then
            Eval_Aggregate (Expr);
         end if;
         Assoc := Get_Chain (Assoc);
      end loop;
   end Eval_Aggregate;

   function Eval_Selected_Element (Expr : Iir) return Iir
   is
      Selected_El : constant Iir := Get_Named_Entity (Expr);
      El_Pos : constant Iir_Index32 := Get_Element_Position (Selected_El);
      Prefix : Iir;
      Cur_Pos : Iir_Index32;
      Assoc : Iir;
      Assoc_Expr : Iir;
      Res : Iir;
   begin
      Prefix := Get_Prefix (Expr);
      Prefix := Eval_Static_Expr (Prefix);
      if Is_Overflow_Literal (Prefix) then
         return Build_Overflow (Expr, Get_Type (Expr));
      end if;

      pragma Assert (Get_Kind (Prefix) = Iir_Kind_Aggregate);
      Assoc := Get_Association_Choices_Chain (Prefix);
      Cur_Pos := 0;
      Assoc_Expr := Null_Iir;
      loop
         if not Get_Same_Alternative_Flag (Assoc) then
            Assoc_Expr := Assoc;
         end if;
         case Iir_Kinds_Record_Choice (Get_Kind (Assoc)) is
            when Iir_Kind_Choice_By_None =>
               exit when Cur_Pos = El_Pos;
               Cur_Pos := Cur_Pos + 1;
            when Iir_Kind_Choice_By_Name =>
               declare
                  Choice : constant Iir := Get_Choice_Name (Assoc);
               begin
                  exit when Get_Element_Position (Get_Named_Entity (Choice))
                    = El_Pos;
               end;
            when Iir_Kind_Choice_By_Others =>
               exit;
         end case;
         Assoc := Get_Chain (Assoc);
      end loop;

      --  Eval element and save it.
      Res := Eval_Expr_Keep_Orig (Get_Associated_Expr (Assoc_Expr), True);
      Set_Associated_Expr (Assoc_Expr, Res);
      return Res;
   end Eval_Selected_Element;

   function Eval_Indexed_Aggregate (Prefix : Iir; Expr : Iir) return Iir
   is
      Indexes : constant Iir_Flist := Get_Index_List (Expr);
      Prefix_Type : constant Iir := Get_Type (Prefix);
      Indexes_Type : constant Iir_Flist :=
        Get_Index_Subtype_List (Prefix_Type);
      Idx : Iir;
      Assoc : Iir;
      Assoc_Expr : Iir;
      Aggr_Bounds : Iir;
      Aggr : Iir;
      Cur_Pos : Int64;
      Res : Iir;
   begin
      Aggr := Prefix;

      for Dim in Flist_First .. Flist_Last (Indexes) loop
         Idx := Get_Nth_Element (Indexes, Dim);

         --  Find Idx in choices.
         Assoc := Get_Association_Choices_Chain (Aggr);
         Aggr_Bounds := Eval_Static_Range
           (Get_Nth_Element (Indexes_Type, Dim));
         Cur_Pos := Eval_Pos (Eval_Discrete_Range_Left (Aggr_Bounds));
         Assoc_Expr := Null_Iir;
         loop
            if not Get_Same_Alternative_Flag (Assoc) then
               Assoc_Expr := Assoc;
            end if;
            case Get_Kind (Assoc) is
               when Iir_Kind_Choice_By_None =>
                  exit when Cur_Pos = Eval_Pos (Idx);
                  case Get_Direction (Aggr_Bounds) is
                     when Dir_To =>
                        Cur_Pos := Cur_Pos + 1;
                     when Dir_Downto =>
                        Cur_Pos := Cur_Pos - 1;
                  end case;
               when Iir_Kind_Choice_By_Expression =>
                  exit when Eval_Is_Eq (Get_Choice_Expression (Assoc), Idx);
               when Iir_Kind_Choice_By_Range =>
                  declare
                     Rng : Iir;
                  begin
                     Rng := Get_Choice_Range (Assoc);
                     Rng := Eval_Static_Range (Rng);
                     exit when Eval_Int_In_Range (Eval_Pos (Idx), Rng);
                  end;
               when Iir_Kind_Choice_By_Others =>
                  exit;
               when others =>
                  raise Internal_Error;
            end case;
            Assoc := Get_Chain (Assoc);
         end loop;
         Aggr := Get_Associated_Expr (Assoc_Expr);
      end loop;

      --  Eval element and save it.
      Res := Eval_Expr_Keep_Orig (Aggr, True);
      Set_Associated_Expr (Assoc_Expr, Res);

      return Res;
   end Eval_Indexed_Aggregate;

   function Eval_Indexed_String_Literal8 (Str : Iir; Expr : Iir) return Iir
   is
      Str_Type : constant Iir := Get_Type (Str);

      Index_Type : constant Iir := Get_Index_Type (Str_Type, 0);
      Index_Range : constant Iir := Eval_Static_Range (Index_Type);

      Indexes : constant Iir_Flist := Get_Index_List (Expr);

      Id : constant String8_Id := Get_String8_Id (Str);

      Idx : Iir;
      Pos : Iir_Index32;
   begin
      Idx := Eval_Static_Expr (Get_Nth_Element (Indexes, 0));
      Pos := Eval_Pos_In_Range (Index_Range, Idx);

      return Build_Enumeration_Constant
        (Iir_Index32 (Str_Table.Element_String8 (Id, Int32 (Pos + 1))), Expr);
   end Eval_Indexed_String_Literal8;

   function Eval_Indexed_Simple_Aggregate (Aggr : Iir; Expr : Iir) return Iir
   is
      Aggr_Type : constant Iir := Get_Type (Aggr);

      Index_Type : constant Iir := Get_Index_Type (Aggr_Type, 0);
      Index_Range : constant Iir := Eval_Static_Range (Index_Type);

      Indexes : constant Iir_Flist := Get_Index_List (Expr);

      Idx : Iir;
      Pos : Iir_Index32;
      El : Iir;
   begin
      Idx := Eval_Static_Expr (Get_Nth_Element (Indexes, 0));
      Set_Nth_Element (Indexes, 0, Idx);
      Pos := Eval_Pos_In_Range (Index_Range, Idx);

      El := Get_Nth_Element (Get_Simple_Aggregate_List (Aggr), Natural (Pos));
      return Build_Constant (El, Expr);
   end Eval_Indexed_Simple_Aggregate;

   function Eval_Indexed_Name (Expr : Iir) return Iir
   is
      Prefix : Iir;
   begin
      Prefix := Get_Prefix (Expr);
      Prefix := Eval_Static_Expr (Prefix);

      declare
         Prefix_Type : constant Iir := Get_Type (Prefix);
         Indexes_Type : constant Iir_Flist :=
           Get_Index_Subtype_List (Prefix_Type);
         Indexes_List : constant Iir_Flist := Get_Index_List (Expr);
         Prefix_Index : Iir;
         Index : Iir;
      begin
         for I in Flist_First .. Flist_Last (Indexes_Type) loop
            Prefix_Index := Get_Nth_Element (Indexes_Type, I);

            --  Eval index.
            Index := Get_Nth_Element (Indexes_List, I);
            Index := Eval_Static_Expr (Index);
            Set_Nth_Element (Indexes_List, I, Index);

            --  Return overflow if out of range.
            if not Eval_Is_In_Bound (Index, Prefix_Index) then
               return Build_Overflow (Expr, Get_Type (Expr));
            end if;
         end loop;
      end;

      case Get_Kind (Prefix) is
         when Iir_Kind_Aggregate =>
            return Eval_Indexed_Aggregate (Prefix, Expr);
         when Iir_Kind_String_Literal8 =>
            return Eval_Indexed_String_Literal8 (Prefix, Expr);
         when Iir_Kind_Simple_Aggregate =>
            return Eval_Indexed_Simple_Aggregate (Prefix, Expr);
         when Iir_Kind_Overflow_Literal =>
            return Build_Overflow (Expr, Get_Type (Expr));
         when others =>
            Error_Kind ("eval_indexed_name", Prefix);
      end case;
   end Eval_Indexed_Name;

   function Eval_Indexed_Aggregate_By_Offset
     (Aggr : Iir; Off : Iir_Index32; Dim : Natural := 0) return Iir
   is
      Prefix_Type : constant Iir := Get_Type (Aggr);
      Indexes_Type : constant Iir_Flist :=
        Get_Index_Subtype_List (Prefix_Type);
      Assoc : Iir;
      Assoc_Expr : Iir;
      Assoc_Len : Iir_Index32;
      Aggr_Bounds : Iir;
      Cur_Off : Iir_Index32;
      Res : Iir;
      Left_Pos : Int64;
      Assoc_Pos : Int64;
   begin
      Aggr_Bounds := Eval_Static_Range (Get_Nth_Element (Indexes_Type, Dim));
      Left_Pos := Eval_Pos (Eval_Discrete_Range_Left (Aggr_Bounds));

      Cur_Off := 0;
      Assoc := Get_Association_Choices_Chain (Aggr);
      Assoc_Expr := Null_Iir;
      while Assoc /= Null_Iir loop
         if not Get_Same_Alternative_Flag (Assoc) then
            Assoc_Expr := Assoc;
         end if;
         case Get_Kind (Assoc) is
            when Iir_Kind_Choice_By_None =>
               if Get_Element_Type_Flag (Assoc) then
                  if Off = Cur_Off then
                     return Get_Associated_Expr (Assoc);
                  end if;
                  Assoc_Len := 1;
               else
                  Res := Get_Associated_Expr (Assoc);
                  Assoc_Len := Iir_Index32
                    (Eval_Discrete_Range_Length
                       (Get_Index_Type (Get_Type (Res), 0)));
                  if Off >= Cur_Off and then Off < Cur_Off + Assoc_Len then
                     return Eval_Indexed_Name_By_Offset (Res, Off - Cur_Off);
                  end if;
               end if;
               Cur_Off := Cur_Off + Assoc_Len;
            when Iir_Kind_Choice_By_Expression =>
               Assoc_Pos := Eval_Pos (Get_Choice_Expression (Assoc));
               case Get_Direction (Aggr_Bounds) is
                  when Dir_To =>
                     Cur_Off := Iir_Index32 (Assoc_Pos - Left_Pos);
                  when Dir_Downto =>
                     Cur_Off := Iir_Index32 (Left_Pos - Assoc_Pos);
               end case;
               if Cur_Off = Off then
                  return Get_Associated_Expr (Assoc);
               end if;
            when Iir_Kind_Choice_By_Range =>
               declare
                  Rng : Iir;
                  Left : Int64;
                  Right : Int64;
                  Hi, Lo : Int64;
                  Lo_Off, Hi_Off : Iir_Index32;
               begin
                  Rng := Eval_Range (Get_Choice_Range (Assoc));
                  Set_Choice_Range (Assoc, Rng);

                  Left := Eval_Pos (Get_Left_Limit (Rng));
                  Right := Eval_Pos (Get_Right_Limit (Rng));
                  case Get_Direction (Rng) is
                     when Dir_To =>
                        Lo := Left;
                        Hi := Right;
                     when Dir_Downto =>
                        Lo := Right;
                        Hi := Left;
                  end case;
                  case Get_Direction (Aggr_Bounds) is
                     when Dir_To =>
                        Lo_Off := Iir_Index32 (Lo - Left_Pos);
                        Hi_Off := Iir_Index32 (Hi - Left_Pos);
                     when Dir_Downto =>
                        Lo_Off := Iir_Index32 (Left_Pos - Lo);
                        Hi_Off := Iir_Index32 (Left_Pos - Hi);
                  end case;
                  if Off >= Lo_Off and then Off <= Hi_Off then
                     Res := Get_Associated_Expr (Assoc);
                     if Get_Element_Type_Flag (Assoc) then
                        return Res;
                     else
                        return Eval_Indexed_Name_By_Offset
                          (Res, Off - Lo_Off);
                     end if;
                  end if;
               end;
            when Iir_Kind_Choice_By_Others =>
               return Get_Associated_Expr (Assoc_Expr);
            when others =>
               raise Internal_Error;
         end case;
         Assoc := Get_Chain (Assoc);
      end loop;
      raise Internal_Error;
   end Eval_Indexed_Aggregate_By_Offset;

   function Eval_Indexed_Name_By_Offset (Prefix : Iir; Off : Iir_Index32)
                                        return Iir
   is
   begin
      case Get_Kind (Prefix) is
         when Iir_Kinds_Denoting_Name =>
            return Eval_Indexed_Name_By_Offset
              (Get_Named_Entity (Prefix), Off);
         when Iir_Kind_Constant_Declaration =>
            return Eval_Indexed_Name_By_Offset
              (Get_Default_Value (Prefix), Off);
         when Iir_Kind_Aggregate =>
            return Eval_Indexed_Aggregate_By_Offset (Prefix, Off);
         when Iir_Kind_String_Literal8 =>
            declare
               Id : constant String8_Id := Get_String8_Id (Prefix);
               El_Type : constant Iir :=
                 Get_Element_Subtype (Get_Type (Prefix));
               Enums : constant Iir_Flist :=
                 Get_Enumeration_Literal_List (Get_Base_Type (El_Type));
               Lit : Pos32;
            begin
               Lit := Str_Table.Element_String8 (Id, Int32 (Off + 1));
               return Get_Nth_Element (Enums, Natural (Lit));
            end;
         when Iir_Kind_Simple_Aggregate =>
            return Get_Nth_Element (Get_Simple_Aggregate_List (Prefix),
                                    Natural (Off));
         when others =>
            Error_Kind ("eval_indexed_name_by_offset", Prefix);
      end case;
   end Eval_Indexed_Name_By_Offset;

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
            return Get_Physical_Literal (Expr);
         when Iir_Kind_Simple_Aggregate =>
            return Expr;
         when Iir_Kind_Aggregate =>
            Eval_Aggregate (Expr);
            return Expr;

         when Iir_Kind_Selected_Element =>
            return Eval_Selected_Element (Expr);
         when Iir_Kind_Indexed_Name =>
            return Eval_Indexed_Name (Expr);

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
               Imp : constant Iir := Get_Implementation (Expr);
               Left : constant Iir := Get_Left (Expr);
               Right : constant Iir := Get_Right (Expr);
               Left_Val, Right_Val : Iir;
               Res : Iir;
            begin
               if (Get_Implicit_Definition (Imp)
                     in Iir_Predefined_Concat_Functions)
               then
                  return Eval_Concatenation ((1 => Expr));
               else
                  Left_Val := Eval_Static_Expr (Left);
                  Right_Val := Eval_Static_Expr (Right);

                  Res := Eval_Dyadic_Operator (Expr, Imp, Left_Val, Right_Val);

                  Free_Eval_Static_Expr (Left_Val, Left);
                  Free_Eval_Static_Expr (Right_Val, Right);

                  return Res;
               end if;
            end;

         when Iir_Kind_Attribute_Name =>
            --  An attribute name designates an attribute value.
            declare
               Attr_Expr : constant Iir :=
                 Get_Attribute_Name_Expression (Expr);
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
               Val : Int64;
            begin
               Val_Expr := Eval_Static_Expr (Get_Parameter (Expr));
               Val := Eval_Pos (Val_Expr);
               --  Note: the type of 'val is a base type.
               --  FIXME: handle VHDL93 restrictions.
               if Get_Kind (Expr_Type) = Iir_Kind_Enumeration_Type_Definition
                 and then
                 not Eval_Int_In_Range (Val, Get_Range_Constraint (Expr_Type))
               then
                  Warning_Msg_Sem (Warnid_Runtime_Error, +Expr,
                                   "static argument out of the type range");
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
               if not Eval_Is_In_Bound (Param, Get_Type (Get_Prefix (Expr)))
               then
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
            begin
               Param := Get_Parameter (Expr);
               Param := Eval_Static_Expr (Param);
               Set_Parameter (Expr, Param);
               if Get_Kind (Param) /= Iir_Kind_String_Literal8 then
                  --  FIXME: Isn't it an implementation restriction.
                  Warning_Msg_Sem (Warnid_Runtime_Error, +Expr,
                                   "'value argument not a string");
                  return Build_Overflow (Expr);
               else
                  return Eval_Value_Attribute
                    (Image_String_Lit (Param), Get_Type (Expr), Expr);
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
              (Get_Direction (Eval_Static_Range (Get_Prefix (Expr))) = Dir_To);

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
                 (Get_Direction (Get_Range_Constraint (Index)) = Dir_To);
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
               N : Int64;
               Prefix_Type : constant Iir := Get_Type (Get_Prefix (Expr));
               Res : Iir;
            begin
               Rng := Eval_Static_Range (Prefix_Type);
               case Get_Direction (Rng) is
                  when Dir_To =>
                     N := 1;
                  when Dir_Downto =>
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
               Img : constant String :=
                 Image (Get_Simple_Name_Identifier (Expr));
               Id : String8_Id;
            begin
               Id := Create_String8;
               for I in Img'Range loop
                  Append_String8_Char (Img (I));
               end loop;
               return Build_String (Id, Nat32 (Img'Length), Expr);
            end;

         when Iir_Kind_Null_Literal =>
            return Expr;

         when Iir_Kind_Function_Call =>
            declare
               Imp : constant Iir := Get_Implementation (Expr);
               Def : constant Iir_Predefined_Functions :=
                 Get_Implicit_Definition (Imp);
               Left, Right : Iir;
            begin
               if Def in Iir_Predefined_Concat_Functions then
                  return Eval_Concatenation ((1 => Expr));
               end if;

               Left := Get_Parameter_Association_Chain (Expr);
               Right := Get_Chain (Left);

               if Def in Iir_Predefined_IEEE_Explicit then
                  --  Note: what about association by name ?
                  pragma Assert
                    (Get_Kind (Left)
                       = Iir_Kind_Association_Element_By_Expression);
                  Left := Eval_Static_Expr (Get_Actual (Left));
                  if Right /= Null_Node then
                     pragma Assert
                       (Get_Kind (Right)
                          = Iir_Kind_Association_Element_By_Expression);
                     Right := Eval_Static_Expr (Get_Actual (Right));
                  end if;
                  return Eval_Ieee_Operation (Expr, Imp, Left, Right);
               end if;

               --  Note: no association by name as the interfaces are
               --  anonymous.
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
         Error_Msg_Sem (+Expr, "expression must be locally static");
         return Expr;
      else
         return Eval_Expr_Keep_Orig (Expr, False);
      end if;
   end Eval_Expr;

   --  Subroutine of Can_Eval_Composite_Value.  Return True iff EXPR is
   --  considered as a small composite.
   function Is_Small_Composite_Value (Expr : Iir) return Boolean
   is
      Expr_Type : constant Iir := Get_Type (Expr);
      Indexes : Iir_Flist;
      Len : Int64;
   begin
      --  Consider only arrays.  Records are never composite.
      if Get_Kind (Expr_Type) /= Iir_Kind_Array_Subtype_Definition then
         return False;
      end if;

      --  Element must be scalar.
      if Get_Kind (Get_Element_Subtype (Expr_Type))
        not in Iir_Kinds_Scalar_Type_And_Subtype_Definition
      then
         return False;
      end if;

      Indexes := Get_Index_Subtype_List (Expr_Type);

      --  Multi-dimensional arrays aren't considered as small.
      if Get_Nbr_Elements (Indexes) /= 1 then
         return False;
      end if;

      Len := Eval_Discrete_Type_Length (Get_Nth_Element (Indexes, 0));
      return Len <= 128;
   end Is_Small_Composite_Value;

   function Can_Eval_Composite_Value (Expr : Iir; Top : Boolean := False)
                                     return Boolean;

   --  Return True if EXPR should be evaluated.
   function Can_Eval_Value (Expr : Iir; Top : Boolean) return Boolean is
   begin
      --  Always evaluate scalar values.
      if Get_Kind (Get_Type (Expr))
        in Iir_Kinds_Scalar_Type_And_Subtype_Definition
      then
         return True;
      end if;
      return Can_Eval_Composite_Value (Expr, Top);
   end Can_Eval_Value;

   --  For composite values.
   --  Evaluating a composite value is a trade-off: it can simplify the
   --  generated code if the value is small enough, or it can be a bad idea if
   --  the value is very large.  It is very easy to create large static
   --  composite values (like: bit_vector'(1 to 10**4 => '0'))
   function Can_Eval_Composite_Value (Expr : Iir; Top : Boolean := False)
                                     return Boolean
   is
      --  We are only considering static values.
      pragma Assert (Get_Expr_Staticness (Expr) = Locally);

      --  We are only considering composite types.
      pragma Assert (Get_Kind (Get_Type (Expr))
                       not in Iir_Kinds_Scalar_Type_And_Subtype_Definition);
   begin
      case Get_Kind (Expr) is
         when Iir_Kind_Type_Conversion
           | Iir_Kind_Qualified_Expression =>
            --  Not yet handled.
            return False;
         when Iir_Kinds_Denoting_Name =>
            return Can_Eval_Composite_Value (Get_Named_Entity (Expr), Top);
         when Iir_Kind_Constant_Declaration =>
            --  Pass through names only for small values.
            if Top or else not Is_Small_Composite_Value (Expr) then
               return False;
            else
               return Can_Eval_Composite_Value (Get_Default_Value (Expr));
            end if;
         when Iir_Kind_Attribute_Name =>
            if Top or else not Is_Small_Composite_Value (Expr) then
               return False;
            else
               return Can_Eval_Composite_Value
                 (Get_Attribute_Name_Expression (Expr));
            end if;
         when Iir_Kinds_Dyadic_Operator =>
            --  Concatenation can increase the size.
            --  Others (rol, ror...) don't.
            return Can_Eval_Value (Get_Left (Expr), False)
              and then Can_Eval_Value (Get_Right (Expr), False);
         when Iir_Kinds_Monadic_Operator =>
            --  For not.
            return Can_Eval_Composite_Value (Get_Operand (Expr));
         when Iir_Kind_Aggregate =>
            return Is_Small_Composite_Value (Expr);
         when Iir_Kinds_Literal
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Simple_Aggregate
           | Iir_Kind_Image_Attribute
           | Iir_Kind_Simple_Name_Attribute =>
            return True;
         when Iir_Kind_Overflow_Literal =>
            return True;
         when Iir_Kind_Function_Call =>
            --  Either using post-fixed notation or implicit functions like
            --  to_string.
            --  Cannot be a user function (won't be locally static).
            declare
               Assoc : Iir;
               Assoc_Expr : Iir;
            begin
               Assoc := Get_Parameter_Association_Chain (Expr);
               while Is_Valid (Assoc) loop
                  case Iir_Kinds_Association_Element_Parameters
                    (Get_Kind (Assoc))
                  is
                     when Iir_Kind_Association_Element_By_Expression
                        | Iir_Kind_Association_Element_By_Name =>
                        Assoc_Expr := Get_Actual (Assoc);
                        if not Can_Eval_Value (Assoc_Expr, False) then
                           return False;
                        end if;
                     when Iir_Kind_Association_Element_Open =>
                        null;
                     when Iir_Kind_Association_Element_By_Individual =>
                        return False;
                  end case;
                  Assoc := Get_Chain (Assoc);
               end loop;
               return True;
            end;

         when others =>
            --  Be safe, don't crash on unhandled expression.
            --  Error_Kind ("can_eval_composite_value", Expr);
            return False;
      end case;
   end Can_Eval_Composite_Value;

   function Eval_Expr_If_Static (Expr : Iir) return Iir is
   begin
      if Expr /= Null_Iir and then Get_Expr_Staticness (Expr) = Locally then
         --  Evaluate only when there is a positive effect.
         if Can_Eval_Value (Expr, True) then
            return Eval_Expr_Keep_Orig (Expr, False);
         else
            return Expr;
         end if;
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
         --  Expression is static and can be evaluated.  Don't try to
         --  evaluate non-scalar expressions, that may create too large data.
         if Get_Kind (Atype) in Iir_Kinds_Scalar_Type_And_Subtype_Definition
         then
            Res := Eval_Expr_Keep_Orig (Expr, False);
         else
            Res := Expr;
         end if;

         if Res /= Null_Iir
           and then Get_Type_Staticness (Atype) = Locally
           and then Get_Kind (Atype) in Iir_Kinds_Range_Type_Definition
         then
            --  Check bounds (as this can be done).
            if not Eval_Check_Bound (Res, Atype) then
               Res := Build_Overflow (Res, Atype);
            end if;
         end if;

         return Res;
      else
         return Expr;
      end if;
   end Eval_Expr_Check_If_Static;

   function Eval_Int_In_Range (Val : Int64; Bound : Iir) return Boolean
   is
      L, R : Iir;
   begin
      case Get_Kind (Bound) is
         when Iir_Kind_Range_Expression =>
            L := Get_Left_Limit (Bound);
            R := Get_Right_Limit (Bound);
            if Get_Kind (L) = Iir_Kind_Overflow_Literal
              or else Get_Kind (R) = Iir_Kind_Overflow_Literal
            then
               return True;
            end if;
            case Get_Direction (Bound) is
               when Dir_To =>
                  return Val >= Eval_Pos (L) and then Val <= Eval_Pos (R);
               when Dir_Downto =>
                  return Val <= Eval_Pos (L) and then Val >= Eval_Pos (R);
            end case;
         when others =>
            Error_Kind ("eval_int_in_range", Bound);
      end case;
      return True;
   end Eval_Int_In_Range;

   function Eval_Phys_In_Range (Val : Int64; Bound : Iir) return Boolean
   is
      Left, Right : Int64;
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
               when Dir_To =>
                  if Val < Left or else Val > Right then
                     return False;
                  end if;
               when Dir_Downto =>
                  if Val > Left or else Val < Right then
                     return False;
                  end if;
            end case;
         when others =>
            Error_Kind ("eval_phys_in_range", Bound);
      end case;
      return True;
   end Eval_Phys_In_Range;

   function Eval_Fp_In_Range (Val : Fp64; Bound : Iir) return Boolean is
   begin
      case Get_Kind (Bound) is
         when Iir_Kind_Range_Expression =>
            case Get_Direction (Bound) is
               when Dir_To =>
                  if Val < Get_Fp_Value (Get_Left_Limit (Bound))
                    or else Val > Get_Fp_Value (Get_Right_Limit (Bound))
                  then
                     return False;
                  end if;
               when Dir_Downto =>
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
   function Eval_Is_In_Bound
     (Expr : Iir; Sub_Type : Iir; Overflow : Boolean := False) return Boolean
   is
      Type_Range : Iir;
      Val : Iir;
   begin
      case Get_Kind (Expr) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Character_Literal
           | Iir_Kind_Selected_Name
           | Iir_Kind_Parenthesis_Name =>
            Val := Get_Named_Entity (Expr);
         when others =>
            Val := Expr;
      end case;

      case Get_Kind (Val) is
         when Iir_Kind_Error =>
            --  Ignore errors.
            return True;
         when Iir_Kind_Overflow_Literal =>
            return Overflow;
         when others =>
            null;
      end case;

      case Get_Kind (Sub_Type) is
         when Iir_Kind_Integer_Subtype_Definition =>
            if Get_Expr_Staticness (Val) /= Locally
              or else Get_Type_Staticness (Sub_Type) /= Locally
            then
               return True;
            end if;
            Type_Range := Get_Range_Constraint (Sub_Type);
            return Eval_Int_In_Range (Get_Value (Val), Type_Range);

         when Iir_Kind_Floating_Subtype_Definition =>
            if Get_Expr_Staticness (Val) /= Locally
              or else Get_Type_Staticness (Sub_Type) /= Locally
            then
               return True;
            end if;
            Type_Range := Get_Range_Constraint (Sub_Type);
            return Eval_Fp_In_Range (Get_Fp_Value (Val), Type_Range);

         when Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition =>
            if Get_Expr_Staticness (Val) /= Locally
              or else Get_Type_Staticness (Sub_Type) /= Locally
            then
               return True;
            end if;
            --  A check is required for an enumeration type definition for
            --  'val attribute.
            Type_Range := Get_Range_Constraint (Sub_Type);
            return Eval_Int_In_Range
              (Int64 (Get_Enum_Pos (Val)), Type_Range);

         when Iir_Kind_Physical_Subtype_Definition =>
            if Get_Expr_Staticness (Val) /= Locally
              or else Get_Type_Staticness (Sub_Type) /= Locally
            then
               return True;
            end if;
            Type_Range := Get_Range_Constraint (Sub_Type);
            return Eval_Phys_In_Range (Get_Physical_Value (Val), Type_Range);

         when Iir_Kind_Base_Attribute =>
            if Get_Expr_Staticness (Val) /= Locally
              or else Get_Type_Staticness (Sub_Type) /= Locally
            then
               return True;
            end if;
            return Eval_Is_In_Bound (Val, Get_Type (Sub_Type));

         when Iir_Kind_Array_Subtype_Definition =>
            declare
               Val_Type : constant Iir := Get_Type (Val);
            begin
               if Is_Null (Val_Type) then
                  --  Punt on errors.
                  return True;
               end if;

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
                  E_Indexes : constant Iir_Flist :=
                    Get_Index_Subtype_List (Val_Type);
                  T_Indexes : constant Iir_Flist :=
                    Get_Index_Subtype_List (Sub_Type);
                  E_El : Iir;
                  T_El : Iir;
               begin
                  for I in Flist_First .. Flist_Last (E_Indexes) loop
                     E_El := Get_Index_Type (E_Indexes, I);
                     T_El := Get_Index_Type (T_Indexes, I);

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

         when Iir_Kind_File_Type_Definition
           | Iir_Kind_File_Subtype_Definition =>
            return True;

         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Floating_Type_Definition =>
            return True;

         when Iir_Kind_Interface_Type_Definition
           | Iir_Kind_Protected_Type_Declaration =>
            return True;

         when Iir_Kind_Foreign_Vector_Type_Definition =>
            return True;

         when Iir_Kind_Error =>
            return True;

         when others =>
            Error_Kind ("eval_is_in_bound", Sub_Type);
      end case;
   end Eval_Is_In_Bound;

   function Eval_Check_Bound (Expr : Iir; Sub_Type : Iir) return Boolean is
   begin
      --  Note: use True not to repeat a message in case of overflow.
      if Eval_Is_In_Bound (Expr, Sub_Type, True) then
         return True;
      end if;

      Warning_Msg_Sem (Warnid_Runtime_Error, +Expr,
                       "static expression violates bounds");
      return False;
   end Eval_Check_Bound;

   procedure Eval_Check_Bound (Expr : Iir; Sub_Type : Iir)
   is
      Res : Boolean;
   begin
      Res := Eval_Check_Bound (Expr, Sub_Type);
      pragma Unreferenced (Res);
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
               L_Expr : constant Iir := Get_Left_Limit (Range_Constraint);
               R_Expr : constant Iir := Get_Right_Limit (Range_Constraint);
               L, R : Int64;
            begin
               if Is_Overflow_Literal (L_Expr)
                 or else Is_Overflow_Literal (R_Expr)
               then
                  return False;
               end if;
               --  Check for null range.
               L := Eval_Pos (L_Expr);
               R := Eval_Pos (R_Expr);
               case Get_Direction (Range_Constraint) is
                  when Dir_To =>
                     if L > R then
                        return True;
                     end if;
                  when Dir_Downto =>
                     if L < R then
                        return True;
                     end if;
               end case;
               return Eval_Int_In_Range (L, Type_Range)
                 and then Eval_Int_In_Range (R, Type_Range);
            end;
         when Iir_Kind_Floating_Subtype_Definition =>
            declare
               L, R : Fp64;
            begin
               --  Check for null range.
               L := Get_Fp_Value (Get_Left_Limit (Range_Constraint));
               R := Get_Fp_Value (Get_Right_Limit (Range_Constraint));
               case Get_Direction (Range_Constraint) is
                  when Dir_To =>
                     if L > R then
                        return True;
                     end if;
                  when Dir_Downto =>
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
         Warning_Msg_Sem (Warnid_Runtime_Error, +A_Range,
                          "static range violates bounds");
      end if;
   end Eval_Check_Range;

   function Eval_Discrete_Range_Length (Constraint : Iir) return Int64
   is
      --  We don't want to deal with very large ranges here.
      pragma Suppress (Overflow_Check);
      Left_Expr : constant Iir := Get_Left_Limit (Constraint);
      Right_Expr : constant Iir := Get_Right_Limit (Constraint);
      Res : Int64;
      Left, Right : Int64;
   begin
      if Is_Overflow_Literal (Left_Expr)
        or else Is_Overflow_Literal (Right_Expr)
      then
         return -1;
      end if;

      Left := Eval_Pos (Left_Expr);
      Right := Eval_Pos (Right_Expr);
      case Get_Direction (Constraint) is
         when Dir_To =>
            if Right < Left then
               --  Null range.
               return 0;
            else
               Res := Right - Left + 1;
            end if;
         when Dir_Downto =>
            if Left < Right then
               --  Null range
               return 0;
            else
               Res := Left - Right + 1;
            end if;
      end case;
      return Res;
   end Eval_Discrete_Range_Length;

   function Eval_Discrete_Type_Length (Sub_Type : Iir) return Int64
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

   function Eval_Is_Null_Discrete_Range (Rng : Iir) return Boolean
   is
      Left, Right : Int64;
   begin
      Left := Eval_Pos (Get_Left_Limit (Rng));
      Right := Eval_Pos (Get_Right_Limit (Rng));
      case Get_Direction (Rng) is
         when Dir_To =>
            return Right < Left;
         when Dir_Downto =>
            return Left < Right;
      end case;
   end Eval_Is_Null_Discrete_Range;

   function Eval_Pos (Expr : Iir) return Int64 is
   begin
      case Get_Kind (Expr) is
         when Iir_Kind_Integer_Literal =>
            return Get_Value (Expr);
         when Iir_Kind_Enumeration_Literal =>
            return Int64 (Get_Enum_Pos (Expr));
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
               declare
                  Left : Iir;
                  Right : Iir;
               begin
                  Left := Get_Left_Limit_Expr (Expr);
                  if Is_Valid (Left) then
                     Left := Eval_Expr_Keep_Orig (Left, False);
                     Set_Left_Limit_Expr (Expr, Left);
                     Set_Left_Limit (Expr, Left);
                  end if;
                  Right := Get_Right_Limit_Expr (Expr);
                  if Is_Valid (Right) then
                     Right := Eval_Expr_Keep_Orig (Right, False);
                     Set_Right_Limit_Expr (Expr, Right);
                     Set_Right_Limit (Expr, Right);
                  end if;
               end;
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
                  Indexes_List : Iir_Flist;
                  Prefix : Iir;
                  Res : Iir;
                  Dim : Natural;
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
                  Indexes_List := Get_Index_Subtype_List (Prefix);
                  Dim := Eval_Attribute_Parameter_Or_1 (Expr);
                  if Dim < 1
                    or else Dim > Get_Nbr_Elements (Indexes_List)
                  then
                     --  Avoid cascaded errors.
                     Dim := 1;
                  end if;
                  Expr := Get_Nth_Element (Indexes_List, Dim - 1);
                  if Kind = Iir_Kind_Reverse_Range_Array_Attribute then
                     Expr := Eval_Static_Range (Expr);

                     Res := Create_Iir (Iir_Kind_Range_Expression);
                     Location_Copy (Res, Expr);
                     Set_Type (Res, Get_Type (Expr));
                     case Get_Direction (Expr) is
                        when Dir_To =>
                           Set_Direction (Res, Dir_Downto);
                        when Dir_Downto =>
                           Set_Direction (Res, Dir_To);
                     end case;
                     Set_Left_Limit (Res, Get_Right_Limit (Expr));
                     Set_Right_Limit (Res, Get_Left_Limit (Expr));
                     Set_Range_Origin (Res, Rng);
                     Set_Expr_Staticness (Res, Get_Expr_Staticness (Expr));
                     return Res;
                  end if;
               end;

            when Iir_Kind_Subtype_Declaration
              | Iir_Kind_Base_Attribute
              | Iir_Kind_Subtype_Attribute
              | Iir_Kind_Element_Attribute =>
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

   function Eval_Is_Eq (L, R : Iir) return Boolean
   is
      Expr_Type : constant Iir := Get_Type (L);
   begin
      case Get_Kind (Expr_Type) is
         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition =>
            return Eval_Pos (L) = Eval_Pos (R);
         when Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Floating_Type_Definition =>
            return Get_Fp_Value (L) = Get_Fp_Value (R);
         when others =>
            Error_Kind ("eval_is_eq", Expr_Type);
      end case;
   end Eval_Is_Eq;

   function Eval_Operator_Symbol_Name (Id : Name_Id) return String is
   begin
      return '"' & Image (Id) & '"';
   end Eval_Operator_Symbol_Name;

   function Eval_Simple_Name (Id : Name_Id) return String is
   begin
      --  LRM 14.1
      --  E'SIMPLE_NAME
      --    Result: [...] but with apostrophes (in the case of a character
      --            literal)
      if Is_Character (Id) then
         return ''' & Get_Character (Id) & ''';
      end if;
      case Id is
         when Std_Names.Name_Word_Operators
           | Std_Names.Name_First_Operator .. Std_Names.Name_Last_Operator =>
            return Eval_Operator_Symbol_Name (Id);
         when Std_Names.Name_Xnor
           | Std_Names.Name_Shift_Operators =>
            if Flags.Vhdl_Std > Vhdl_87 then
               return Eval_Operator_Symbol_Name (Id);
            end if;
         when others =>
            null;
      end case;
      return Image (Id);
   end Eval_Simple_Name;

   package body String_Utils is
      --  Fill Res from EL.  This is used to speed up Lt and Eq operations.
      function Get_Str_Info (Expr : Iir) return Str_Info is
      begin
         case Get_Kind (Expr) is
            when Iir_Kind_Simple_Aggregate =>
               declare
                  List : constant Iir_Flist :=
                    Get_Simple_Aggregate_List (Expr);
               begin
                  return Str_Info'(Is_String => False,
                                   Len => Nat32 (Get_Nbr_Elements (List)),
                                   List => List);
               end;
            when Iir_Kind_String_Literal8 =>
               return Str_Info'(Is_String => True,
                                Len => Get_String_Length (Expr),
                                Id => Get_String8_Id (Expr));
            when others =>
               Error_Kind ("string_utils.get_info", Expr);
         end case;
      end Get_Str_Info;

      --  Return the position of element IDX of STR.
      function Get_Pos (Str : Str_Info; Idx : Nat32) return Iir_Int32
      is
         S : Iir;
         P : Nat32;
      begin
         case Str.Is_String is
            when False =>
               S := Get_Nth_Element (Str.List, Natural (Idx));
               return Get_Enum_Pos (S);
            when True =>
               P := Str_Table.Element_String8 (Str.Id, Idx + 1);
               return Iir_Int32 (P);
         end case;
      end Get_Pos;
   end String_Utils;

   function Compare_String_Literals (L, R : Iir) return Compare_Type
   is
      use String_Utils;
      L_Info : constant Str_Info := Get_Str_Info (L);
      R_Info : constant Str_Info := Get_Str_Info (R);
      L_Pos, R_Pos : Iir_Int32;
   begin
      if L_Info.Len /= R_Info.Len then
         raise Internal_Error;
      end if;

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
         Mark : Iir;
      begin
         if Get_Kind (Atype) in Iir_Kinds_Denoting_Name then
            Mark := Atype;
         else
            Mark := Get_Subtype_Type_Mark (Atype);
         end if;
         Path_Add (Image (Get_Identifier (Mark)));
      end Path_Add_Type_Name;

      procedure Path_Add_Signature (Subprg : Iir)
      is
         Inter : Iir;
         Inter_Type, Prev_Type : Iir;
      begin
         Path_Add ("[");
         Prev_Type := Null_Iir;
         Inter := Get_Interface_Declaration_Chain (Subprg);
         while Inter /= Null_Iir loop
            Inter_Type := Get_Subtype_Indication (Inter);
            if Inter_Type = Null_Iir then
               Inter_Type := Prev_Type;
            end if;
            Path_Add_Type_Name (Inter_Type);
            Prev_Type := Inter_Type;

            Inter := Get_Chain (Inter);
            if Inter /= Null_Iir then
               Path_Add (",");
            end if;
         end loop;

         case Get_Kind (Subprg) is
            when Iir_Kind_Function_Declaration =>
               Path_Add (" return ");
               Path_Add_Type_Name (Get_Return_Type_Mark (Subprg));
            when others =>
               null;
         end case;
         Path_Add ("]");
      end Path_Add_Signature;

      procedure Path_Add_Name (N : Iir)
      is
         Img : constant String := Eval_Simple_Name (Get_Identifier (N));
      begin
         if Img (Img'First) /= 'P' then
            --  Skip anonymous processes.
            Path_Add (Img);
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
              | Iir_Kind_Package_Body
              | Iir_Kind_Package_Instantiation_Declaration =>
               if Is_Nested_Package (El) then
                  Path_Add_Element (Get_Parent (El), Is_Instance);
               else
                  Path_Add_Element
                    (Get_Library (Get_Design_File (Get_Design_Unit (El))),
                     Is_Instance);
               end if;
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
              | Iir_Kind_Block_Statement
              | Iir_Kind_Protected_Type_Body =>
               Path_Add_Element (Get_Parent (El), Is_Instance);
               Path_Add_Name (El);
               Path_Add (":");
            when Iir_Kind_Protected_Type_Declaration =>
               declare
                  Decl : constant Iir := Get_Type_Declarator (El);
               begin
                  Path_Add_Element (Get_Parent (Decl), Is_Instance);
                  Path_Add_Name (Decl);
                  Path_Add (":");
               end;
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
           | Iir_Kinds_Library_Unit
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

end Vhdl.Evaluation;
