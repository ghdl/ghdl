--  Expressions synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

with Ada.Unchecked_Conversion;
with Types_Utils; use Types_Utils;
with Std_Names;
with Str_Table;
with Mutils; use Mutils;

with Vhdl.Ieee.Std_Logic_1164; use Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Std_Package;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Evaluation; use Vhdl.Evaluation;
with Vhdl.Annotations; use Vhdl.Annotations;

with Netlists.Gates; use Netlists.Gates;
with Netlists.Builders; use Netlists.Builders;
with Netlists.Folds; use Netlists.Folds;

with Synth.Errors; use Synth.Errors;
with Synth.Environment;
with Synth.Decls;
with Synth.Stmts; use Synth.Stmts;
with Synth.Oper; use Synth.Oper;
with Synth.Heap; use Synth.Heap;
with Synth.Debugger;

package body Synth.Expr is
   function Synth_Name (Syn_Inst : Synth_Instance_Acc; Name : Node)
                       return Value_Acc;

   procedure Set_Location (N : Net; Loc : Node)
     renames Synth.Source.Set_Location;

   function Get_Static_Discrete (V : Value_Acc) return Int64
   is
      N : Net;
   begin
      case V.Kind is
         when Value_Discrete =>
            return V.Scal;
         when Value_Const =>
            return V.C_Val.Scal;
         when Value_Net =>
            N := V.N;
         when Value_Wire =>
            N := Synth.Environment.Get_Const_Wire (V.W);
         when others =>
            raise Internal_Error;
      end case;
      return Get_Net_Int64 (N);
   end Get_Static_Discrete;

   procedure From_Std_Logic (Enum : Int64; Val : out Uns32; Zx : out Uns32) is
   begin
      case Enum is
         when Vhdl.Ieee.Std_Logic_1164.Std_Logic_0_Pos
           |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_L_Pos =>
            Val := 0;
            Zx := 0;
         when Vhdl.Ieee.Std_Logic_1164.Std_Logic_1_Pos
           |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_H_Pos =>
            Val := 1;
            Zx := 0;
         when Vhdl.Ieee.Std_Logic_1164.Std_Logic_U_Pos
           |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_X_Pos
           |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_D_Pos =>
            Val := 1;
            Zx := 1;
         when Vhdl.Ieee.Std_Logic_1164.Std_Logic_Z_Pos
           |  Vhdl.Ieee.Std_Logic_1164.Std_Logic_W_Pos =>
            Val := 0;
            Zx := 1;
         when others =>
            --  Only 9 values.
            raise Internal_Error;
      end case;
   end From_Std_Logic;

   procedure From_Bit (Enum : Int64; Val : out Uns32) is
   begin
      if Enum = 0 then
         Val := 0;
      elsif Enum = 1 then
         Val := 1;
      else
         raise Internal_Error;
      end if;
   end From_Bit;

   procedure To_Logic
     (Enum : Int64; Etype : Type_Acc; Val : out Uns32; Zx : out Uns32) is
   begin
      if Etype = Logic_Type then
         pragma Assert (Etype.Kind = Type_Logic);
         From_Std_Logic (Enum, Val, Zx);
      elsif Etype = Boolean_Type or Etype = Bit_Type then
         pragma Assert (Etype.Kind = Type_Bit);
         From_Bit (Enum, Val);
         Zx := 0;
      else
         raise Internal_Error;
      end if;
   end To_Logic;


   procedure Value2logvec (Val : Value_Acc;
                           Vec : in out Logvec_Array;
                           Off : in out Uns32;
                           Has_Zx : in out Boolean) is
   begin
      if Val.Kind = Value_Const then
         Value2logvec (Val.C_Val, Vec, Off, Has_Zx);
         return;
      end if;

      case Val.Typ.Kind is
         when Type_Bit =>
            declare
               Idx : constant Digit_Index := Digit_Index (Off / 32);
               Pos : constant Natural := Natural (Off mod 32);
               Va : Uns32;
            begin
               Va := Uns32 (Val.Scal);
               Va := Shift_Left (Va, Pos);
               Vec (Idx).Val := Vec (Idx).Val or Va;
               Vec (Idx).Zx := 0;
               Off := Off + 1;
            end;
         when Type_Logic =>
            declare
               Idx : constant Digit_Index := Digit_Index (Off / 32);
               Pos : constant Natural := Natural (Off mod 32);
               Va : Uns32;
               Zx : Uns32;
            begin
               From_Std_Logic (Val.Scal, Va, Zx);
               Has_Zx := Has_Zx or Zx /= 0;
               Va := Shift_Left (Va, Pos);
               Zx := Shift_Left (Zx, Pos);
               Vec (Idx).Val := Vec (Idx).Val or Va;
               Vec (Idx).Zx := Vec (Idx).Zx or Zx;
               Off := Off + 1;
            end;
         when Type_Discrete =>
            for I in 0 .. Val.Typ.W - 1 loop
               declare
                  B : constant Uns32 :=
                    Uns32 (Shift_Right (To_Uns64 (Val.Scal), Natural (I)))
                    and 1;
                  Idx : constant Digit_Index := Digit_Index (Off / 32);
                  Pos : constant Natural := Natural (Off mod 32);
               begin
                  Vec (Idx).Val := Vec (Idx).Val or Shift_Left (B, Pos);
               end;
               Off := Off + 1;
            end loop;
         when Type_Vector =>
            --  TODO: optimize off mod 32 = 0.
            for I in reverse Val.Arr.V'Range loop
               Value2logvec (Val.Arr.V (I), Vec, Off, Has_Zx);
            end loop;
         when Type_Array =>
            for I in reverse Val.Arr.V'Range loop
               Value2logvec (Val.Arr.V (I), Vec, Off, Has_Zx);
            end loop;
         when Type_Record =>
            for I in Val.Rec.V'Range loop
               Value2logvec (Val.Rec.V (I), Vec, Off, Has_Zx);
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Value2logvec;

   function Bit_Extract (Val : Value_Acc; Off : Uns32; Loc : Node)
                        return Value_Acc
   is
      N : Net;
   begin
      case Val.Kind is
         when Value_Array
           | Value_Const_Array =>
            pragma Assert (Val.Typ.Vbound.Len >= Off);
            return Val.Arr.V (Iir_Index32 (Val.Typ.Vbound.Len - Off));
         when Value_Net
           | Value_Wire =>
            N := Build_Extract_Bit (Build_Context, Get_Net (Val), Off);
            Set_Location (N, Loc);
            return Create_Value_Net (N, Val.Typ.Vec_El);
         when others =>
            raise Internal_Error;
      end case;
   end Bit_Extract;

   --  Resize for a discrete value.
   function Synth_Resize (Val : Value_Acc; W : Width; Loc : Node) return Net
   is
      Wn : constant Width := Val.Typ.W;
      N : Net;
      Res : Net;
   begin
      if Is_Static (Val) then
         if Wn /= W then
            pragma Assert (Val.Kind = Value_Discrete);
            if Val.Typ.Drange.Is_Signed then
               Res := Build2_Const_Int (Build_Context, Val.Scal, W);
            else
               Res := Build2_Const_Uns (Build_Context, To_Uns64 (Val.Scal), W);
            end if;
            Set_Location (Res, Loc);
            return Res;
         end if;
      end if;

      N := Get_Net (Val);
      if Wn > W then
         return Build2_Trunc (Build_Context, Id_Utrunc, N, W,
                              Get_Location (Loc));
      elsif Wn < W then
         if Val.Typ.Drange.Is_Signed then
            Res := Build_Extend (Build_Context, Id_Sextend, N, W);
         else
            Res := Build_Extend (Build_Context, Id_Uextend, N, W);
         end if;
         Set_Location (Res, Loc);
         return Res;
      else
         return N;
      end if;
   end Synth_Resize;

   function Get_Index_Offset
     (Index : Int64; Bounds : Bound_Type; Expr : Iir) return Uns32
   is
      Left : constant Int64 := Int64 (Bounds.Left);
      Right : constant Int64 := Int64 (Bounds.Right);
   begin
      case Bounds.Dir is
         when Iir_To =>
            if Index >= Left and then Index <= Right then
               -- to
               return Uns32 (Index - Left);
            end if;
         when Iir_Downto =>
            if Index <= Left and then Index >= Right then
               -- downto
               return Uns32 (Left - Index);
            end if;
      end case;
      Error_Msg_Synth (+Expr, "index out of bounds");
      return 0;
   end Get_Index_Offset;

   function Get_Index_Offset
     (Index : Value_Acc; Bounds : Bound_Type; Expr : Iir) return Uns32 is
   begin
      if Index.Kind = Value_Discrete then
         return Get_Index_Offset (Index.Scal, Bounds, Expr);
      else
         raise Internal_Error;
      end if;
   end Get_Index_Offset;

   function Get_Array_Bound (Typ : Type_Acc; Dim : Natural)
                            return Bound_Type is
   begin
      case Typ.Kind is
         when Type_Vector =>
            pragma Assert (Dim = 0);
            return Typ.Vbound;
         when Type_Array =>
            return Typ.Abounds.D (Iir_Index32 (Dim + 1));
         when others =>
            raise Internal_Error;
      end case;
   end Get_Array_Bound;

   function Get_Range_Length (Rng : Discrete_Range_Type) return Uns32
   is
      Len : Int64;
   begin
      case Rng.Dir is
         when Iir_To =>
            Len := Rng.Right - Rng.Left + 1;
         when Iir_Downto =>
            Len := Rng.Left - Rng.Right + 1;
      end case;
      if Len < 0 then
         return 0;
      else
         return Uns32 (Len);
      end if;
   end Get_Range_Length;

   type Stride_Array is array (Dim_Type range <>) of Iir_Index32;

   function Fill_Stride (Typ : Type_Acc) return Stride_Array is
   begin
      case Typ.Kind is
         when Type_Vector =>
            return (1 => 1);
         when Type_Array =>
            declare
               Bnds : constant Bound_Array_Acc := Typ.Abounds;
               Res : Stride_Array (1 .. Dim_Type (Bnds.Len));
               Stride : Iir_Index32;
            begin
               Stride := 1;
               for I in reverse 2 .. Bnds.Len loop
                  Res (Dim_Type (I)) := Stride;
                  Stride := Stride * Iir_Index32 (Bnds.D (I).Len);
               end loop;
               Res (1) := Stride;
               return Res;
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Fill_Stride;

   procedure Fill_Array_Aggregate (Syn_Inst : Synth_Instance_Acc;
                                   Aggr : Node;
                                   Res : Value_Array_Acc;
                                   Typ : Type_Acc;
                                   First_Pos : Iir_Index32;
                                   Strides : Stride_Array;
                                   Dim : Dim_Type;
                                   Const_P : out Boolean)
   is
      Bound : constant Bound_Type := Get_Array_Bound (Typ, Natural (Dim - 1));
      El_Typ : constant Type_Acc := Get_Array_Element (Typ);
      Stride : constant Iir_Index32 := Strides (Dim);
      Value : Node;
      Assoc : Node;

      procedure Set_Elem (Pos : Iir_Index32)
      is
         Sub_Const : Boolean;
         Val : Value_Acc;
      begin
         if Dim = Strides'Last then
            Val := Synth_Expression_With_Type (Syn_Inst, Value, El_Typ);
            pragma Assert (Res.V (Pos) = null);
            Res.V (Pos) := Val;
            if Const_P and then not Is_Static (Val) then
               Const_P := False;
            end if;
         else
            Fill_Array_Aggregate
              (Syn_Inst, Value, Res, Typ, Pos, Strides, Dim + 1, Sub_Const);
            if not Sub_Const then
               Const_P := False;
            end if;
         end if;
      end Set_Elem;

      procedure Set_Vector
        (Pos : Iir_Index32; Len : Iir_Index32; Val : Value_Acc) is
      begin
         pragma Assert (Dim = Strides'Last);
         if Len = 0 then
            return;
         end if;
         --  FIXME: factorize with bit_extract ?
         case Val.Kind is
            when Value_Array
              | Value_Const_Array =>
               declare
                  E : Value_Acc;
               begin
                  for I in 1 .. Len loop
                     E := Val.Arr.V (Len - I);
                     Res.V (Pos + I - 1) := E;
                  end loop;
                  Const_P := Const_P and then Val.Kind = Value_Const_Array;
               end;
            when Value_Net
              | Value_Wire =>
               declare
                  N : Net;
                  E : Net;
               begin
                  N := Get_Net (Val);
                  for I in 1 .. Len loop
                     E := Build_Extract (Build_Context, N,
                                         Uns32 (Len - I) * El_Typ.W, El_Typ.W);
                     Res.V (Pos + I - 1) := Create_Value_Net (E, El_Typ);
                  end loop;
                  Const_P := False;
               end;
            when others =>
               raise Internal_Error;
         end case;
      end Set_Vector;

      Pos : Iir_Index32;
   begin
      Assoc := Get_Association_Choices_Chain (Aggr);
      Pos := First_Pos;
      Const_P := True;
      while Is_Valid (Assoc) loop
         Value := Get_Associated_Expr (Assoc);
         loop
            case Get_Kind (Assoc) is
               when Iir_Kind_Choice_By_None =>
                  if not Get_Element_Type_Flag (Assoc) then
                     raise Internal_Error;
                  end if;
                  if Pos >= First_Pos + Stride * Iir_Index32 (Bound.Len) then
                     Error_Msg_Synth (+Assoc, "element out of array bound");
                  else
                     Set_Elem (Pos);
                     Pos := Pos + Stride;
                  end if;
               when Iir_Kind_Choice_By_Others =>
                  pragma Assert (Get_Element_Type_Flag (Assoc));
                  declare
                     Last_Pos : constant Iir_Index32 :=
                       First_Pos + Iir_Index32 (Bound.Len) * Stride;
                  begin
                     while Pos < Last_Pos loop
                        if Res.V (Pos) = null then
                           Set_Elem (Pos);
                        end if;
                        Pos := Pos + Stride;
                     end loop;
                  end;
               when Iir_Kind_Choice_By_Expression =>
                  pragma Assert (Get_Element_Type_Flag (Assoc));
                  declare
                     Ch : constant Node := Get_Choice_Expression (Assoc);
                     Idx : Value_Acc;
                     Off : Iir_Index32;
                  begin
                     Idx := Synth_Expression (Syn_Inst, Ch);
                     if not Is_Static (Idx) then
                        Error_Msg_Synth (+Ch, "choice is not static");
                     else
                        Off := Iir_Index32 (Get_Index_Offset (Idx, Bound, Ch));
                        Set_Elem (First_Pos + Off * Stride);
                     end if;
                  end;
               when Iir_Kind_Choice_By_Range =>
                  declare
                     Ch : constant Node := Get_Choice_Range (Assoc);
                     Rng : Discrete_Range_Type;
                     Val : Value_Acc;
                     W_Rng : Width;
                     Rng_Len : Width;
                     Off : Iir_Index32;
                  begin
                     Synth_Discrete_Range (Syn_Inst, Ch, Rng, W_Rng);
                     if Get_Element_Type_Flag (Assoc) then
                        Val := Create_Value_Discrete
                          (Rng.Left,
                           Get_Value_Type (Syn_Inst,
                                           Get_Base_Type (Get_Type (Ch))));
                        while In_Range (Rng, Val.Scal) loop
                           Off := Iir_Index32
                             (Get_Index_Offset (Val, Bound, Ch));
                           Set_Elem (First_Pos + Off * Stride);
                           Update_Index (Rng, Val.Scal);
                        end loop;
                     else
                        --  The direction must be the same.
                        if Rng.Dir /= Bound.Dir then
                           Error_Msg_Synth
                             (+Assoc, "direction of range does not match "
                                & "direction of array");
                        end if;
                        --  FIXME: can the expression be unbounded ?
                        Val := Synth_Expression (Syn_Inst, Value);
                        --  The length must match the range.
                        Rng_Len := Get_Range_Length (Rng);
                        if Get_Bound_Length (Val.Typ, 1) /= Rng_Len then
                           Error_Msg_Synth
                             (+Value, "length doesn't match range");
                        end if;
                        pragma Assert (Stride = 1);
                        Off := Iir_Index32
                          (Get_Index_Offset (Rng.Left, Bound, Ch));
                        Set_Vector (First_Pos + Off,
                                    Iir_Index32 (Rng_Len), Val);
                     end if;
                  end;
               when others =>
                  Error_Msg_Synth
                    (+Assoc, "unhandled association form");
            end case;
            Assoc := Get_Chain (Assoc);
            exit when Is_Null (Assoc);
            exit when not Get_Same_Alternative_Flag (Assoc);
         end loop;
      end loop;
   end Fill_Array_Aggregate;

   procedure Fill_Record_Aggregate (Syn_Inst : Synth_Instance_Acc;
                                    Aggr : Node;
                                    Rec : Value_Array_Acc;
                                    Const_P : out Boolean)
   is
      El_List : constant Node_Flist :=
        Get_Elements_Declaration_List (Get_Type (Aggr));
      Value : Node;
      Assoc : Node;
      Pos : Natural;

      procedure Set_Elem (Pos : Natural)
      is
         Val : Value_Acc;
         El_Type : Type_Acc;
      begin
         El_Type := Get_Value_Type
           (Syn_Inst, Get_Type (Get_Nth_Element (El_List, Pos)));
         Val := Synth_Expression_With_Type (Syn_Inst, Value, El_Type);
         Rec.V (Iir_Index32 (Pos + 1)) := Synth_Subtype_Conversion
           (Val, El_Type, False, Value);
         if Const_P and not Is_Static (Val) then
            Const_P := False;
         end if;
      end Set_Elem;
   begin
      Assoc := Get_Association_Choices_Chain (Aggr);
      Pos := 0;
      Const_P := True;
      Rec.V := (others => null);
      while Is_Valid (Assoc) loop
         Value := Get_Associated_Expr (Assoc);
         loop
            case Get_Kind (Assoc) is
               when Iir_Kind_Choice_By_None =>
                  Set_Elem (Pos);
                  Pos := Pos + 1;
               when Iir_Kind_Choice_By_Others =>
                  for I in Rec.V'Range loop
                     if Rec.V (I) = null then
                        Set_Elem (Natural (I - 1));
                     end if;
                  end loop;
               when Iir_Kind_Choice_By_Name =>
                  Pos := Natural (Get_Element_Position
                                    (Get_Named_Entity
                                       (Get_Choice_Name (Assoc))));
                  Set_Elem (Pos);
               when others =>
                  Error_Msg_Synth
                    (+Assoc, "unhandled association form");
            end case;
            Assoc := Get_Chain (Assoc);
            exit when Is_Null (Assoc);
            exit when not Get_Same_Alternative_Flag (Assoc);
         end loop;
      end loop;
   end Fill_Record_Aggregate;

   procedure Concat_Array (Arr : in out Net_Array)
   is
      Last : Int32;
      Idx, New_Idx : Int32;
   begin
      Last := Arr'Last;
      while Last > Arr'First loop
         Idx := Arr'First;
         New_Idx := Arr'First - 1;
         while Idx <= Last loop
            --  Gather at most 4 nets.
            New_Idx := New_Idx + 1;

            if Idx = Last then
               Arr (New_Idx) := Arr (Idx);
               Idx := Idx + 1;
            elsif Idx + 1 = Last then
               Arr (New_Idx) := Build_Concat2
                 (Build_Context, Arr (Idx), Arr (Idx + 1));
               Idx := Idx + 2;
            elsif Idx + 2 = Last then
               Arr (New_Idx) := Build_Concat3
                 (Build_Context, Arr (Idx), Arr (Idx + 1), Arr (Idx + 2));
               Idx := Idx + 3;
            else
               Arr (New_Idx) := Build_Concat4
                 (Build_Context,
                  Arr (Idx), Arr (Idx + 1), Arr (Idx + 2), Arr (Idx + 3));
               Idx := Idx + 4;
            end if;
         end loop;
         Last := New_Idx;
      end loop;
   end Concat_Array;

   function Concat_Array (Arr : Net_Array_Acc) return Net is
   begin
      Concat_Array (Arr.all);
      return Arr (Arr'First);
   end Concat_Array;

   function Synth_Discrete_Range_Expression
     (L : Int64; R : Int64; Dir : Iir_Direction) return Discrete_Range_Type is
   begin
      return (Dir => Dir,
              Left => L,
              Right => R,
              Is_Signed => L < 0 or R < 0);
   end Synth_Discrete_Range_Expression;

   function Synth_Discrete_Range_Expression
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Discrete_Range_Type
   is
      L, R : Value_Acc;
   begin
      L := Synth_Expression_With_Basetype (Syn_Inst, Get_Left_Limit (Rng));
      R := Synth_Expression_With_Basetype (Syn_Inst, Get_Right_Limit (Rng));
      Strip_Const (L);
      Strip_Const (R);

      if not (Is_Static (L) and Is_Static (R)) then
         Error_Msg_Synth (+Rng, "limits of range are not constant");
         raise Internal_Error;
      end if;

      return (Dir => Get_Direction (Rng),
              Left => L.Scal,
              Right => R.Scal,
              Is_Signed => L.Scal < 0 or R.Scal < 0);
   end Synth_Discrete_Range_Expression;

   function Synth_Float_Range_Expression
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Float_Range_Type
   is
      L, R : Value_Acc;
   begin
      L := Synth_Expression (Syn_Inst, Get_Left_Limit (Rng));
      R := Synth_Expression (Syn_Inst, Get_Right_Limit (Rng));
      return ((Get_Direction (Rng), L.Fp, R.Fp));
   end Synth_Float_Range_Expression;

   function Synth_Array_Attribute (Syn_Inst : Synth_Instance_Acc; Attr : Node)
                                  return Bound_Type
   is
      Prefix : constant Iir := Strip_Denoting_Name (Get_Prefix (Attr));
      Dim : constant Natural :=
        Vhdl.Evaluation.Eval_Attribute_Parameter_Or_1 (Attr);
      Res : Value_Acc;
   begin
      --  Prefix is an array object or an array subtype.
      Res := Synth_Name (Syn_Inst, Prefix);
      if Res.Typ.Kind = Type_Vector then
         if Dim /= 1 then
            raise Internal_Error;
         end if;
         return Res.Typ.Vbound;
      else
         return Res.Typ.Abounds.D (Iir_Index32 (Dim));
      end if;
   end Synth_Array_Attribute;

   procedure Synth_Discrete_Range (Syn_Inst : Synth_Instance_Acc;
                                   Bound : Node;
                                   Rng : out Discrete_Range_Type;
                                   W : out Width) is
   begin
      case Get_Kind (Bound) is
         when Iir_Kind_Range_Expression =>
            Rng := Synth_Discrete_Range_Expression (Syn_Inst, Bound);
            W := Discrete_Range_Width (Rng);
         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            if Get_Type_Declarator (Bound) /= Null_Node then
               declare
                  Typ : Type_Acc;
               begin
                  --  This is a named subtype, so it has been evaluated.
                  Typ := Get_Value_Type (Syn_Inst, Bound);
                  Rng := Typ.Drange;
                  W := Typ.W;
               end;
            else
               Synth_Discrete_Range
                 (Syn_Inst, Get_Range_Constraint (Bound), Rng, W);
            end if;
         when Iir_Kind_Range_Array_Attribute =>
            declare
               B : Bound_Type;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Bound);
               Rng := Discrete_Range_Type'(Dir => B.Dir,
                                           Is_Signed => True,
                                           Left => Int64 (B.Left),
                                           Right => Int64 (B.Right));
               W := B.Wbounds;
            end;
         when others =>
            Error_Kind ("synth_discrete_range", Bound);
      end case;
   end Synth_Discrete_Range;

   function Synth_Array_Bounds (Syn_Inst : Synth_Instance_Acc;
                                Atype : Node;
                                Dim : Natural) return Bound_Type
   is
      Info : constant Sim_Info_Acc := Get_Info (Atype);
   begin
      if Info = null then
         pragma Assert (Get_Type_Declarator (Atype) = Null_Node);
         declare
            Index_Type : constant Node := Get_Index_Type (Atype, Dim);
         begin
            return Synth_Bounds_From_Range (Syn_Inst, Index_Type);
         end;
      else
         declare
            Bnds : constant Value_Acc := Get_Value (Syn_Inst, Atype);
         begin
            case Bnds.Typ.Kind is
               when Type_Vector =>
                  pragma Assert (Dim = 0);
                  return Bnds.Typ.Vbound;
               when Type_Array =>
                  return Bnds.Typ.Abounds.D (Iir_Index32 (Dim + 1));
               when others =>
                  raise Internal_Error;
            end case;
         end;
      end if;
   end Synth_Array_Bounds;

   function Synth_Bounds_From_Range (Syn_Inst : Synth_Instance_Acc;
                                     Atype : Node) return Bound_Type
   is
      Rng : Discrete_Range_Type;
      W : Width;
   begin
      Synth_Discrete_Range (Syn_Inst, Atype, Rng, W);
      return (Dir => Rng.Dir,
              Wbounds => W,
              Left => Int32 (Rng.Left), Right => Int32 (Rng.Right),
              Len => Get_Range_Length (Rng));
   end Synth_Bounds_From_Range;

   function Synth_Aggregate_Array (Syn_Inst : Synth_Instance_Acc;
                                   Aggr : Node;
                                   Aggr_Type : Type_Acc) return Value_Acc
   is
      Strides : constant Stride_Array := Fill_Stride (Aggr_Type);
      Arr : Value_Array_Acc;
      Res : Value_Acc;
      Const_P : Boolean;
   begin
      Arr := Create_Value_Array
        (Iir_Index32 (Get_Array_Flat_Length (Aggr_Type)));

      Fill_Array_Aggregate
        (Syn_Inst, Aggr, Arr, Aggr_Type, 1, Strides, 1, Const_P);

      if Const_P then
         Res := Create_Value_Const_Array (Aggr_Type, Arr);
      else
         Res := Create_Value_Array (Aggr_Type, Arr);
      end if;

      return Res;
   end Synth_Aggregate_Array;

   function Synth_Aggregate_Record (Syn_Inst : Synth_Instance_Acc;
                                    Aggr : Node;
                                    Aggr_Type : Type_Acc) return Value_Acc
   is
      Arr : Value_Array_Acc;
      Res : Value_Acc;
      Const_P : Boolean;
   begin
      --  Allocate the result.
      Arr := Create_Value_Array (Aggr_Type.Rec.Len);

      Fill_Record_Aggregate (Syn_Inst, Aggr, Arr, Const_P);

      if Const_P then
         Res := Create_Value_Const_Record (Aggr_Type, Arr);
      else
         Res := Create_Value_Record (Aggr_Type, Arr);
      end if;

      return Res;
   end Synth_Aggregate_Record;

   --  Aggr_Type is the type from the context.
   function Synth_Aggregate (Syn_Inst : Synth_Instance_Acc;
                             Aggr : Node;
                             Aggr_Type : Type_Acc) return Value_Acc is
   begin
      case Aggr_Type.Kind is
         when Type_Unbounded_Array | Type_Unbounded_Vector =>
            declare
               Res_Type : Type_Acc;
            begin
               Res_Type := Decls.Synth_Array_Subtype_Indication
                 (Syn_Inst, Get_Type (Aggr));
               return Synth_Aggregate_Array (Syn_Inst, Aggr, Res_Type);
            end;
         when Type_Vector | Type_Array =>
            return Synth_Aggregate_Array (Syn_Inst, Aggr, Aggr_Type);
         when Type_Record =>
            return Synth_Aggregate_Record (Syn_Inst, Aggr, Aggr_Type);
         when others =>
            raise Internal_Error;
      end case;
   end Synth_Aggregate;

   function Synth_Simple_Aggregate (Syn_Inst : Synth_Instance_Acc;
                                    Aggr : Node) return Value_Acc
   is
      Aggr_Type : constant Node := Get_Type (Aggr);
      pragma Assert (Get_Nbr_Dimensions (Aggr_Type) = 1);
      El_Type : constant Node := Get_Element_Subtype (Aggr_Type);
      El_Typ : constant Type_Acc := Get_Value_Type (Syn_Inst, El_Type);
      Els : constant Iir_Flist := Get_Simple_Aggregate_List (Aggr);
      Last : constant Natural := Flist_Last (Els);
      Bnd : Bound_Type;
      Bnds : Bound_Array_Acc;
      Res_Type : Type_Acc;
      Arr : Value_Array_Acc;
      Val : Value_Acc;
   begin
      --  Allocate the result.
      Bnd := Synth_Array_Bounds (Syn_Inst, Aggr_Type, 0);
      pragma Assert (Bnd.Len = Uns32 (Last + 1));

      if El_Typ.Kind in Type_Nets then
         Res_Type := Create_Vector_Type (Bnd, El_Typ);
      else
         Bnds := Create_Bound_Array (1);
         Bnds.D (1) := Bnd;
         Res_Type := Create_Array_Type (Bnds, El_Typ);
      end if;

      Arr := Create_Value_Array (Iir_Index32 (Last + 1));

      for I in Flist_First .. Last loop
         Val := Synth_Expression_With_Type
           (Syn_Inst, Get_Nth_Element (Els, I), El_Typ);
         pragma Assert (Is_Static (Val));
         Arr.V (Iir_Index32 (I + 1)) := Val;
      end loop;

      return Create_Value_Const_Array (Res_Type, Arr);
   end Synth_Simple_Aggregate;

   --  Change the bounds of VAL.
   function Reshape_Value (Val : Value_Acc; Ntype : Type_Acc)
                          return Value_Acc is
   begin
      case Val.Kind is
         when Value_Array =>
            return Create_Value_Array (Ntype, Val.Arr);
         when Value_Const_Array =>
            return Create_Value_Const_Array (Ntype, Val.Arr);
         when Value_Wire =>
            return Create_Value_Wire (Val.W, Ntype);
         when Value_Net =>
            return Create_Value_Net (Val.N, Ntype);
         when Value_Alias =>
            return Create_Value_Alias (Val.A_Obj, Val.A_Off, Ntype);
         when Value_Const =>
            return Reshape_Value (Val.C_Val, Ntype);
         when others =>
            raise Internal_Error;
      end case;
   end Reshape_Value;

   function Synth_Subtype_Conversion (Val : Value_Acc;
                                      Dtype : Type_Acc;
                                      Bounds : Boolean;
                                      Loc : Source.Syn_Src)
                                     return Value_Acc
   is
      Vtype : constant Type_Acc := Val.Typ;
   begin
      case Dtype.Kind is
         when Type_Bit =>
            pragma Assert (Vtype.Kind = Type_Bit);
            return Val;
         when Type_Logic =>
            pragma Assert (Vtype.Kind = Type_Logic);
            return Val;
         when Type_Discrete =>
            pragma Assert (Vtype.Kind = Type_Discrete);
            declare
               N : Net;
            begin
               if Vtype.W /= Dtype.W then
                  --  Truncate.
                  --  TODO: check overflow.
                  case Val.Kind is
                     when Value_Net
                       | Value_Wire
                       | Value_Alias =>
                        N := Get_Net (Val);
                        if Vtype.Drange.Is_Signed then
                           N := Build2_Sresize
                             (Build_Context, N, Dtype.W, Get_Location (Loc));
                        else
                           N := Build2_Uresize
                             (Build_Context, N, Dtype.W, Get_Location (Loc));
                        end if;
                        return Create_Value_Net (N, Dtype);
                     when Value_Discrete =>
                        return Create_Value_Discrete (Val.Scal, Dtype);
                     when Value_Const =>
                        return Create_Value_Discrete (Val.C_Val.Scal, Dtype);
                     when others =>
                        raise Internal_Error;
                  end case;
               else
                  --  TODO: check overflow if sign differ.
                  return Val;
               end if;
            end;
         when Type_Float =>
            pragma Assert (Vtype.Kind = Type_Float);
            --  TODO: check range
            return Val;
         when Type_Vector =>
            pragma Assert (Vtype.Kind = Type_Vector
                             or Vtype.Kind = Type_Slice);
            if False and then Dtype.W /= Vtype.W then
               --  TODO: bad width.
               raise Internal_Error;
            end if;
            if Bounds then
               return Reshape_Value (Val, Dtype);
            else
               return Val;
            end if;
         when Type_Slice =>
            --  TODO: check width
            return Val;
         when Type_Array =>
            pragma Assert (Vtype.Kind = Type_Array);
            --  TODO: check bounds, handle elements
            return Val;
         when Type_Unbounded_Array =>
            pragma Assert (Vtype.Kind = Type_Array);
            return Val;
         when Type_Unbounded_Vector =>
            pragma Assert (Vtype.Kind = Type_Vector
                             or else Vtype.Kind = Type_Slice);
            return Val;
         when Type_Record =>
            --  TODO: handle elements.
            return Val;
         when Type_Access =>
            return Val;
         when Type_File =>
            pragma Assert (Vtype = Dtype);
            return Val;
      end case;
   end Synth_Subtype_Conversion;

   function Synth_Name (Syn_Inst : Synth_Instance_Acc; Name : Node)
                       return Value_Acc is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name =>
            return Synth_Name (Syn_Inst, Get_Named_Entity (Name));
         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Anonymous_Signal_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Interface_File_Declaration =>
            return Get_Value (Syn_Inst, Name);
         when Iir_Kind_Enumeration_Literal =>
            return Create_Value_Discrete
              (Int64 (Get_Enum_Pos (Name)),
               Get_Value_Type (Syn_Inst, Get_Type (Name)));
         when Iir_Kind_Unit_Declaration =>
            return Create_Value_Discrete
              (Vhdl.Evaluation.Get_Physical_Value (Name),
               Get_Value_Type (Syn_Inst, Get_Type (Name)));
         when Iir_Kind_Implicit_Dereference
           | Iir_Kind_Dereference =>
            declare
               Val : Value_Acc;
            begin
               Val := Synth_Expression (Syn_Inst, Get_Prefix (Name));
               return Heap.Synth_Dereference (Val.Acc);
            end;
         when others =>
            Error_Kind ("synth_name", Name);
      end case;
   end Synth_Name;

   function In_Bounds (Bnd : Bound_Type; V : Int32) return Boolean is
   begin
      case Bnd.Dir is
         when Iir_To =>
            return V >= Bnd.Left and then V <= Bnd.Right;
         when Iir_Downto =>
            return V <= Bnd.Left and then V >= Bnd.Right;
      end case;
   end In_Bounds;

   --  Convert index IDX in PFX to an offset.
   --  SYN_INST and LOC are used in case of error.
   function Index_To_Offset
     (Syn_Inst : Synth_Instance_Acc; Bnd : Bound_Type; Idx : Int64; Loc : Node)
     return Uns32 is
   begin
      if not In_Bounds (Bnd, Int32 (Idx)) then
         Error_Msg_Synth (+Loc, "index not within bounds");
         Synth.Debugger.Debug_Error (Syn_Inst, Loc);
         return 0;
      end if;

      --  The offset is from the LSB (bit 0).  Bit 0 is the rightmost one.
      case Bnd.Dir is
         when Iir_To =>
            return Uns32 (Bnd.Right - Int32 (Idx));
         when Iir_Downto =>
            return Uns32 (Int32 (Idx) - Bnd.Right);
      end case;
   end Index_To_Offset;

   function Dyn_Index_To_Offset
     (Bnd : Bound_Type; Idx_Val : Value_Acc; Loc : Node) return Net
   is
      Idx2 : Net;
      Off : Net;
      Right : Net;
   begin
      Idx2 := Synth_Resize (Idx_Val, Bnd.Wbounds, Loc);

      if Bnd.Right = 0 and then Bnd.Dir = Iir_Downto then
         --  Simple case without adjustments.
         return Idx2;
      end if;

      Right := Build_Const_UB32 (Build_Context, To_Uns32 (Bnd.Right),
                                 Bnd.Wbounds);
      Set_Location (Right, Loc);

      case Bnd.Dir is
         when Iir_To =>
            --  L <= I <= R    -->   off = R - I
            Off := Build_Dyadic (Build_Context, Id_Sub, Right, Idx2);
         when Iir_Downto =>
            --  L >= I >= R    -->   off = I - R
            Off := Build_Dyadic (Build_Context, Id_Sub, Idx2, Right);
      end case;
      Set_Location (Off, Loc);
      return Off;
   end Dyn_Index_To_Offset;

   --  Return the bounds of a one dimensional array/vector type and the
   --  width of the element.
   procedure Get_Onedimensional_Array_Bounds
     (Typ : Type_Acc; Bnd : out Bound_Type; El_Typ : out Type_Acc) is
   begin
      case Typ.Kind is
         when Type_Vector =>
            El_Typ := Typ.Vec_El;
            Bnd := Typ.Vbound;
         when Type_Array =>
            El_Typ := Typ.Arr_El;
            Bnd := Typ.Abounds.D (1);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Onedimensional_Array_Bounds;

   function Create_Onedimensional_Array_Subtype
     (Btyp : Type_Acc; Bnd : Bound_Type) return Type_Acc
   is
      Res : Type_Acc;
      Bnds : Bound_Array_Acc;
   begin
      case Btyp.Kind is
         when Type_Vector =>
            Res := Create_Vector_Type (Bnd, Btyp.Vec_El);
         when Type_Unbounded_Vector =>
            Res := Create_Vector_Type (Bnd, Btyp.Uvec_El);
         when Type_Array =>
            pragma Assert (Btyp.Abounds.Len = 1);
            Bnds := Create_Bound_Array (1);
            Bnds.D (1) := Bnd;
            Res := Create_Array_Type (Bnds, Btyp.Arr_El);
         when Type_Unbounded_Array =>
            pragma Assert (Btyp.Uarr_Ndim = 1);
            Bnds := Create_Bound_Array (1);
            Bnds.D (1) := Bnd;
            Res := Create_Array_Type (Bnds, Btyp.Uarr_El);
         when others =>
            raise Internal_Error;
      end case;
      return Res;
   end Create_Onedimensional_Array_Subtype;

   procedure Synth_Indexed_Name (Syn_Inst : Synth_Instance_Acc;
                                 Name : Node;
                                 Pfx_Type : Type_Acc;
                                 Voff : out Net;
                                 Off : out Uns32;
                                 W : out Width)
   is
      Indexes : constant Iir_Flist := Get_Index_List (Name);
      Idx_Expr : constant Node := Get_Nth_Element (Indexes, 0);
      Idx_Val : Value_Acc;
      Idx_Type : Type_Acc;
      Bnd : Bound_Type;
      El_Typ : Type_Acc;
   begin
      if Get_Nbr_Elements (Indexes) /= 1 then
         Error_Msg_Synth (+Name, "multi-dim arrays not yet supported");
         raise Internal_Error;
      end if;

      --  Use the base type as the subtype of the index is not synth-ed.
      Idx_Type := Get_Value_Type
        (Syn_Inst, Get_Base_Type (Get_Type (Idx_Expr)));
      Idx_Val := Synth_Expression_With_Type (Syn_Inst, Idx_Expr, Idx_Type);
      Strip_Const (Idx_Val);

      Get_Onedimensional_Array_Bounds (Pfx_Type, Bnd, El_Typ);
      W := El_Typ.W;

      if Idx_Val.Kind = Value_Discrete then
         Voff := No_Net;
         Off := Index_To_Offset (Syn_Inst, Bnd, Idx_Val.Scal, Name) * W;
      else
         Voff := Dyn_Index_To_Offset (Bnd, Idx_Val, Name);
         Voff := Build_Memidx (Get_Build (Syn_Inst), Voff, W, Bnd.Len - 1,
                               Width (Clog2 (Uns64 (W * Bnd.Len))));
         Set_Location (Voff, Name);
         Off := 0;
      end if;
   end Synth_Indexed_Name;

   function Is_Static (N : Net) return Boolean is
   begin
      case Get_Id (Get_Module (Get_Net_Parent (N))) is
         when Id_Const_UB32 =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Static;

   function Get_Const (N : Net) return Int32
   is
      Inst : constant Instance := Get_Net_Parent (N);
   begin
      case Get_Id (Get_Module (Inst)) is
         when Id_Const_UB32 =>
            return To_Int32 (Get_Param_Uns32 (Inst, 0));
         when others =>
            raise Internal_Error;
      end case;
   end Get_Const;

   procedure Decompose_Mul_Add (Val : Net;
                                Inp : out Net;
                                Factor : out Int32;
                                Addend : out Int32)
   is
      Inst : Instance;
      Val_I0, Val_I1 : Net;
   begin
      Factor := 1;
      Addend := 0;
      Inp := Val;

      loop
         Inst := Get_Net_Parent (Inp);
         case Get_Id (Get_Module (Inst)) is
            when Id_Add =>
               Val_I0 := Get_Input_Net (Inst, 0);
               Val_I1 := Get_Input_Net (Inst, 1);
               if Is_Static (Val_I0) then
                  Addend := Addend + Get_Const (Val_I0) * Factor;
                  Inp := Val_I1;
               elsif Is_Static (Val_I1) then
                  Addend := Addend + Get_Const (Val_I1) * Factor;
                  Inp := Val_I0;
               else
                  --  It's an addition, but without any constant value.
                  return;
               end if;
            when Id_Sub =>
               Val_I0 := Get_Input_Net (Inst, 0);
               Val_I1 := Get_Input_Net (Inst, 1);
               if Is_Static (Val_I1) then
                  Addend := Addend - Get_Const (Val_I1) * Factor;
                  Inp := Val_I0;
               else
                  --  It's a substraction, but without any constant value.
                  return;
               end if;
            when Id_Smul =>
               Val_I0 := Get_Input_Net (Inst, 0);
               Val_I1 := Get_Input_Net (Inst, 1);
               if Is_Static (Val_I0) then
                  Factor := Factor * Get_Const (Val_I0);
                  Inp := Val_I1;
               elsif Is_Static (Val_I1) then
                  Factor := Factor * Get_Const (Val_I1);
                  Inp := Val_I0;
               else
                  --  A mul but without any constant value.
                  return;
               end if;
            when Id_Utrunc
              | Id_Uextend =>
               Inp := Get_Input_Net (Inst, 0);
            when others =>
               --  Cannot decompose it.
               return;
         end case;
      end loop;
   end Decompose_Mul_Add;

   --  Identify LEFT to/downto RIGHT as:
   --  INP * STEP + WIDTH - 1 + OFF to/downto INP * STEP + OFF
   procedure Synth_Extract_Dyn_Suffix (Loc : Node;
                                       Pfx_Bnd : Bound_Type;
                                       Left : Net;
                                       Right : Net;
                                       Inp : out Net;
                                       Step : out Uns32;
                                       Off : out Uns32;
                                       Width : out Uns32)
   is
      L_Inp, R_Inp : Net;
      L_Fac, R_Fac : Int32;
      L_Add, R_Add : Int32;
   begin
      Inp := No_Net;
      Step := 0;
      Off := 0;
      Width := 0;

      if Left = Right then
         L_Inp := Left;
         R_Inp := Right;
         L_Fac := 1;
         R_Fac := 1;
         L_Add := 0;
         R_Add := 0;
      else
         Decompose_Mul_Add (Left, L_Inp, L_Fac, L_Add);
         Decompose_Mul_Add (Right, R_Inp, R_Fac, R_Add);
      end if;

      if not Same_Net (L_Inp, R_Inp) then
         Error_Msg_Synth
           (+Loc, "cannot extract same variable part for dynamic slice");
         return;
      end if;
      Inp := L_Inp;

      if L_Fac /= R_Fac then
         Error_Msg_Synth
           (+Loc, "cannot extract same constant factor for dynamic slice");
         return;
      end if;
      --  FIXME: what to do with negative values.
      Step := Uns32 (L_Fac);

      case Pfx_Bnd.Dir is
         when Iir_To =>
            Off := Uns32 (L_Add - Pfx_Bnd.Left);
            Width := Uns32 (R_Add - L_Add + 1);
         when Iir_Downto =>
            Off := Uns32 (R_Add - Pfx_Bnd.Right);
            Width := Uns32 (L_Add - R_Add + 1);
      end case;
   end Synth_Extract_Dyn_Suffix;

   procedure Synth_Slice_Const_Suffix (Syn_Inst: Synth_Instance_Acc;
                                       Expr : Node;
                                       Name : Node;
                                       Pfx_Bnd : Bound_Type;
                                       L, R : Int64;
                                       Dir : Iir_Direction;
                                       El_Wd : Width;
                                       Res_Bnd : out Bound_Type;
                                       Off : out Uns32;
                                       Wd : out Width)
   is
      Is_Null : Boolean;
      Len : Uns32;
   begin
      if Pfx_Bnd.Dir /= Dir then
         Error_Msg_Synth (+Name, "direction mismatch in slice");
         Off := 0;
         Wd := 0;
         return;
      end if;

      --  Might be a null slice.
      case Pfx_Bnd.Dir is
         when Iir_To =>
            Is_Null := L > R;
         when Iir_Downto =>
            Is_Null := L < R;
      end case;
      if Is_Null then
         Len := 0;
         Off := 0;
      else
         if not In_Bounds (Pfx_Bnd, Int32 (L))
           or else not In_Bounds (Pfx_Bnd, Int32 (R))
         then
            Error_Msg_Synth (+Name, "index not within bounds");
            Synth.Debugger.Debug_Error (Syn_Inst, Expr);
            Wd := 0;
            Off := 0;
            return;
         end if;

         case Pfx_Bnd.Dir is
            when Iir_To =>
               Len := Uns32 (R - L + 1);
               Off := Uns32 (Pfx_Bnd.Right - Int32 (R)) * El_Wd;
            when Iir_Downto =>
               Len := Uns32 (L - R + 1);
               Off := Uns32 (Int32 (R) - Pfx_Bnd.Right) * El_Wd;
         end case;
      end if;
      Res_Bnd := (Dir => Pfx_Bnd.Dir,
                  Wbounds => Pfx_Bnd.Wbounds,
                  Len => Len,
                  Left => Int32 (L),
                  Right => Int32 (R));
      Wd := Len * El_Wd;
   end Synth_Slice_Const_Suffix;

   procedure Synth_Slice_Suffix (Syn_Inst : Synth_Instance_Acc;
                                 Name : Node;
                                 Pfx_Bnd : Bound_Type;
                                 El_Wd : Width;
                                 Res_Bnd : out Bound_Type;
                                 Inp : out Net;
                                 Off : out Uns32;
                                 Wd : out Width)
   is
      Expr : constant Node := Get_Suffix (Name);
      Left, Right : Value_Acc;
      Dir : Iir_Direction;
      Step : Uns32;
      Max : Uns32;
      Inp_W : Width;
   begin
      Off := 0;

      case Get_Kind (Expr) is
         when Iir_Kind_Range_Expression =>
            Left := Synth_Expression_With_Basetype
              (Syn_Inst, Get_Left_Limit (Expr));
            Right := Synth_Expression_With_Basetype
              (Syn_Inst, Get_Right_Limit (Expr));
            Dir := Get_Direction (Expr);
         when Iir_Kind_Range_Array_Attribute =>
            declare
               Rng : Discrete_Range_Type;
               W : Width;
            begin
               Synth_Discrete_Range (Syn_Inst, Expr, Rng, W);
               Inp := No_Net;
               Synth_Slice_Const_Suffix (Syn_Inst, Expr,
                                         Name, Pfx_Bnd,
                                         Rng.Left, Rng.Right, Rng.Dir,
                                         El_Wd, Res_Bnd, Off, Wd);
               return;
            end;
         when others =>
            Error_Msg_Synth
              (+Expr, "only range expression supported for slices");
      end case;

      if Is_Static_Val (Left) and then Is_Static_Val (Right) then
         Inp := No_Net;
         Synth_Slice_Const_Suffix
           (Syn_Inst, Expr,
            Name, Pfx_Bnd,
            Get_Static_Discrete (Left), Get_Static_Discrete (Right), Dir,
            El_Wd, Res_Bnd, Off, Wd);
      else
         if Pfx_Bnd.Dir /= Dir then
            Error_Msg_Synth (+Name, "direction mismatch in slice");
            Inp := No_Net;
            Off := 0;
            Wd := 0;
            return;
         end if;

         if Is_Static (Left) or else Is_Static (Right) then
            Error_Msg_Synth
              (+Name, "left and right bounds of a slice must be "
                 & "either constant or dynamic");
            return;
         end if;
         Synth_Extract_Dyn_Suffix
           (Name, Pfx_Bnd, Get_Net (Left), Get_Net (Right),
            Inp, Step, Off, Wd);
         Inp_W := Get_Width (Inp);
         --  FIXME: convert range to offset.
         --  Extract max from the range.
         --  example: len=128  wd=8  step=8  => max=16
         --           len=8    wd=4  step=1  => max=4
         --  max so that max*step+wd <= len - off
         --              max <= (len - off - wd) / step
         Max := (Pfx_Bnd.Len - Off - Wd) / Step;
         if Clog2 (Uns64 (Max)) > Natural (Inp_W) then
            --  The width of Inp limits the max.
            Max := 2**Natural (Inp_W) - 1;
         end if;
         Inp := Build_Memidx
           (Get_Build (Syn_Inst),
            Inp, Step * El_Wd, Max,
            Inp_W + Width (Clog2 (Uns64 (Step * El_Wd))));
         Wd := Wd * El_Wd;
      end if;
   end Synth_Slice_Suffix;

   --  Match: clk_signal_name'event
   --  and return clk_signal_name.
   function Extract_Event_Expr_Prefix (Expr : Node) return Node is
   begin
      if Get_Kind (Expr) = Iir_Kind_Event_Attribute then
         return Get_Prefix (Expr);
      else
         return Null_Node;
      end if;
   end Extract_Event_Expr_Prefix;

   function Is_Same_Node (Left, Right : Node) return Boolean is
   begin
      if Get_Kind (Left) /= Get_Kind (Right) then
         return False;
      end if;
      case Get_Kind (Left) is
         when Iir_Kind_Simple_Name =>
            return Get_Named_Entity (Left) = Get_Named_Entity (Right);
         when others =>
            Error_Kind ("is_same_node", Left);
      end case;
   end Is_Same_Node;

   --  Match: clk_signal_name = '1' | clk_signal_name = '0'
   function Extract_Clock_Level
     (Syn_Inst : Synth_Instance_Acc; Expr : Node; Prefix : Node) return Net
   is
      Clk : Net;
      Imp : Node;
      Left, Right : Node;
      Lit : Node;
      Posedge : Boolean;
   begin
      Clk := Get_Net (Synth_Name (Syn_Inst, Prefix));
      if Get_Kind (Expr) /= Iir_Kind_Equality_Operator then
         Error_Msg_Synth (+Expr, "ill-formed clock-level, '=' expected");
         return Build_Edge (Build_Context, Clk);
      end if;
      Imp := Get_Implementation (Expr);
      if Get_Implicit_Definition (Imp) /= Iir_Predefined_Enum_Equality then
         Error_Msg_Synth (+Expr, "ill-formed clock-level, '=' expected");
         return Build_Edge (Build_Context, Clk);
      end if;
      Left := Get_Left (Expr);
      Right := Get_Right (Expr);
      if Get_Kind (Right) /= Iir_Kind_Character_Literal then
         Error_Msg_Synth
           (+Expr, "ill-formed clock-level, '0' or '1' expected");
         return Build_Edge (Build_Context, Clk);
      end if;
      Lit := Get_Named_Entity (Right);
      if Lit = Vhdl.Std_Package.Bit_0
        or else Lit = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_0
      then
         Posedge := False;
      elsif Lit = Vhdl.Std_Package.Bit_1
        or else Lit = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_1
      then
         Posedge := True;
      else
         Error_Msg_Synth
           (+Lit, "ill-formed clock-level, '0' or '1' expected");
         Posedge := True;
      end if;
      if not Is_Same_Node (Prefix, Left) then
         Error_Msg_Synth
           (+Left, "clock signal name doesn't match");
      end if;
      if not Posedge then
         Clk := Build_Monadic (Build_Context, Id_Not, Clk);
      end if;
      return Build_Edge (Build_Context, Clk);
   end Extract_Clock_Level;

   --  Try to match: clk'event and clk = X
   --            or: clk = X and clk'event
   --  where X is '0' or '1'.
   function Synth_Clock_Edge
     (Syn_Inst : Synth_Instance_Acc; Left, Right : Node) return Net
   is
      Prefix : Node;
   begin
      --  Try with left.
      Prefix := Extract_Event_Expr_Prefix (Left);
      if Is_Valid (Prefix) then
         return Extract_Clock_Level (Syn_Inst, Right, Prefix);
      end if;

      --  Try with right.
      Prefix := Extract_Event_Expr_Prefix (Right);
      if Is_Valid (Prefix) then
         return Extract_Clock_Level (Syn_Inst, Left, Prefix);
      end if;

      return No_Net;
   end Synth_Clock_Edge;

   function Synth_Type_Conversion (Syn_Inst : Synth_Instance_Acc; Conv : Node)
                                  return Value_Acc
   is
      Expr : constant Node := Get_Expression (Conv);
      Conv_Type : constant Node := Get_Type (Conv);
      Conv_Typ : constant Type_Acc := Get_Value_Type (Syn_Inst, Conv_Type);
      Val : Value_Acc;
   begin
      Val := Synth_Expression_With_Basetype (Syn_Inst, Expr);
      Strip_Const (Val);
      case Get_Kind (Conv_Type) is
         when Iir_Kind_Integer_Subtype_Definition =>
            if Val.Typ.Kind = Type_Discrete then
               --  Int to int.
               return Val;
            elsif Val.Typ.Kind = Type_Float then
               return Create_Value_Discrete (Int64 (Val.Fp), Conv_Typ);
            else
               Error_Msg_Synth (+Conv, "unhandled type conversion (to int)");
               return null;
            end if;
         when Iir_Kind_Floating_Subtype_Definition =>
            if Is_Static (Val) then
               return Create_Value_Float (Fp64 (Val.Scal), Conv_Typ);
            else
               Error_Msg_Synth (+Conv, "unhandled type conversion (to float)");
               return null;
            end if;
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            case Conv_Typ.Kind is
               when Type_Vector
                 | Type_Unbounded_Vector =>
                  return Val;
               when others =>
                  Error_Msg_Synth
                    (+Conv, "unhandled type conversion (to array)");
                  return Val;
            end case;
         when others =>
            Error_Msg_Synth (+Conv, "unhandled type conversion");
            return null;
      end case;
   end Synth_Type_Conversion;

   procedure Error_Unknown_Operator (Imp : Node; Loc : Node) is
   begin
      if Get_Kind (Get_Parent (Imp)) = Iir_Kind_Package_Declaration
        and then (Get_Identifier
                    (Get_Library
                       (Get_Design_File (Get_Design_Unit (Get_Parent (Imp)))))
                    = Std_Names.Name_Ieee)
      then
         Error_Msg_Synth (+Loc, "unhandled predefined IEEE operator %i", +Imp);
         Error_Msg_Synth (+Imp, " declared here");
      else
         Error_Msg_Synth (+Loc, "user defined operator %i not handled", +Imp);
      end if;
   end Error_Unknown_Operator;

   function Synth_String_Literal
     (Syn_Inst : Synth_Instance_Acc; Str : Node; Str_Typ : Type_Acc)
                                 return Value_Acc
   is
      pragma Assert (Get_Kind (Str) = Iir_Kind_String_Literal8);
      Id : constant String8_Id := Get_String8_Id (Str);

      Str_Type : constant Node := Get_Type (Str);
      El_Type : Type_Acc;
      Bounds : Bound_Type;
      Bnds : Bound_Array_Acc;
      Res_Type : Type_Acc;
      Res : Value_Acc;
      Arr : Value_Array_Acc;
      Pos : Nat8;
   begin
      case Str_Typ.Kind is
         when Type_Vector =>
            Bounds := Str_Typ.Vbound;
         when Type_Array =>
            Bounds := Str_Typ.Abounds.D (1);
         when Type_Unbounded_Vector
           | Type_Unbounded_Array =>
            Bounds := Synth_Array_Bounds (Syn_Inst, Str_Type, 0);
         when others =>
            raise Internal_Error;
      end case;

      El_Type := Get_Value_Type (Syn_Inst, Get_Element_Subtype (Str_Type));
      if El_Type.Kind in Type_Nets then
         Res_Type := Create_Vector_Type (Bounds, El_Type);
      else
         Bnds := Create_Bound_Array (1);
         Bnds.D (1) := Bounds;
         Res_Type := Create_Array_Type (Bnds, El_Type);
      end if;
      Arr := Create_Value_Array (Iir_Index32 (Bounds.Len));

      for I in Arr.V'Range loop
         -- FIXME: use literal from type ??
         Pos := Str_Table.Element_String8 (Id, Pos32 (I));
         Arr.V (I) := Create_Value_Discrete (Int64 (Pos), El_Type);
      end loop;

      Res := Create_Value_Const_Array (Res_Type, Arr);
      return Res;
   end Synth_String_Literal;

   subtype And_Or_Module_Id is Module_Id range Id_And .. Id_Or;

   function Synth_Short_Circuit (Syn_Inst : Synth_Instance_Acc;
                                 Id : And_Or_Module_Id;
                                 Left_Expr : Node;
                                 Right_Expr : Node;
                                 Typ : Type_Acc;
                                 Expr : Node) return Value_Acc
   is
      Left : Value_Acc;
      Right : Value_Acc;
      Val : Int64;
      N : Net;
   begin
      --  The short-circuit value.
      case Id is
         when Id_And =>
            Val := 0;
         when Id_Or =>
            Val := 1;
      end case;

      Left := Synth_Expression_With_Type (Syn_Inst, Left_Expr, Typ);
      if Is_Static_Val (Left) and then Get_Static_Discrete (Left) = Val then
         return Create_Value_Discrete (Val, Boolean_Type);
      end if;

      Strip_Const (Left);
      Right := Synth_Expression_With_Type (Syn_Inst, Right_Expr, Typ);
      Strip_Const (Right);

      --  Return a static value if both operands are static.
      --  Note: we know the value of left if it is not constant.
      if Is_Static_Val (Left) and then Is_Static_Val (Right) then
         Val := Get_Static_Discrete (Right);
         return Create_Value_Discrete (Val, Boolean_Type);
      end if;

      N := Build_Dyadic (Build_Context, Id,
                         Get_Net (Left), Get_Net (Right));
      Set_Location (N, Expr);
      return Create_Value_Net (N, Boolean_Type);
   end Synth_Short_Circuit;

   function Synth_Expression_With_Type
     (Syn_Inst : Synth_Instance_Acc; Expr : Node; Expr_Type : Type_Acc)
     return Value_Acc
   is
      Res : Value_Acc;
   begin
      case Get_Kind (Expr) is
         when Iir_Kinds_Dyadic_Operator =>
            declare
               Imp : constant Node := Get_Implementation (Expr);
               Def : constant Iir_Predefined_Functions :=
                 Get_Implicit_Definition (Imp);
               Edge : Net;
            begin
               --  Match clock-edge
               if Def = Iir_Predefined_Boolean_And then
                  Edge := Synth_Clock_Edge (Syn_Inst,
                                            Get_Left (Expr), Get_Right (Expr));
                  if Edge /= No_Net then
                     return Create_Value_Net (Edge, Boolean_Type);
                  end if;
               end if;

               --  Specially handle short-circuit operators.
               case Def is
                  when Iir_Predefined_Boolean_And =>
                     return Synth_Short_Circuit
                       (Syn_Inst, Id_And, Get_Left (Expr), Get_Right (Expr),
                        Boolean_Type, Expr);
                  when Iir_Predefined_Boolean_Or =>
                     return Synth_Short_Circuit
                       (Syn_Inst, Id_Or, Get_Left (Expr), Get_Right (Expr),
                        Boolean_Type, Expr);
                  when Iir_Predefined_Bit_And =>
                     return Synth_Short_Circuit
                       (Syn_Inst, Id_And, Get_Left (Expr), Get_Right (Expr),
                        Bit_Type, Expr);
                  when Iir_Predefined_Bit_Or =>
                     return Synth_Short_Circuit
                       (Syn_Inst, Id_Or, Get_Left (Expr), Get_Right (Expr),
                        Bit_Type, Expr);
                  when others =>
                     return Synth_Dyadic_Operation
                       (Syn_Inst, Imp,
                        Get_Left (Expr), Get_Right (Expr), Expr);
               end case;
            end;
         when Iir_Kinds_Monadic_Operator =>
            declare
               Imp : constant Node := Get_Implementation (Expr);
               Def : constant Iir_Predefined_Functions :=
                 Get_Implicit_Definition (Imp);
            begin
               if Def in Iir_Predefined_Implicit
                 or else Def in Iir_Predefined_IEEE_Explicit
               then
                  return Synth_Monadic_Operation
                    (Syn_Inst, Imp, Get_Operand (Expr), Expr);
               else
                  Error_Unknown_Operator (Imp, Expr);
                  raise Internal_Error;
               end if;
            end;
         when Iir_Kind_Simple_Name
           | Iir_Kind_Interface_Signal_Declaration --  For PSL.
           | Iir_Kind_Signal_Declaration =>  -- For PSL.
            return Synth_Name (Syn_Inst, Expr);
         when Iir_Kind_Reference_Name =>
            return Synth_Name (Syn_Inst, Get_Named_Entity (Expr));
         when Iir_Kind_Indexed_Name
           | Iir_Kind_Slice_Name =>
            declare
               Obj : Value_Acc;
               Off : Uns32;
               Typ : Type_Acc;

               Voff : Net;
               Rdwd : Width;
            begin
               Synth_Assignment_Prefix (Syn_Inst, Expr,
                                        Obj, Off, Voff, Rdwd, Typ);
               if Voff = No_Net and then Is_Static (Obj) then
                  pragma Assert (Off = 0);
                  return Obj;
               end if;
               return Synth_Read_Memory (Syn_Inst, Obj, Off, Voff, Typ, Expr);
            end;
         when Iir_Kind_Selected_Element =>
            declare
               Idx : constant Iir_Index32 :=
                 Get_Element_Position (Get_Named_Entity (Expr));
               Pfx : constant Node := Get_Prefix (Expr);
               Res_Typ : Type_Acc;
               N : Net;
            begin
               Res := Synth_Expression (Syn_Inst, Pfx);
               Strip_Const (Res);
               Res_Typ := Res.Typ.Rec.E (Idx + 1).Typ;
               if Res.Kind = Value_Const_Record then
                  return Res.Rec.V (Idx + 1);
               else
                  N := Build_Extract
                    (Build_Context, Get_Net (Res),
                     Res.Typ.Rec.E (Idx + 1).Off, Get_Type_Width (Res_Typ));
                  Set_Location (N, Expr);
                  return Create_Value_Net (N, Res_Typ);
               end if;
            end;
         when Iir_Kind_Character_Literal =>
            return Synth_Expression_With_Type
              (Syn_Inst, Get_Named_Entity (Expr), Expr_Type);
         when Iir_Kind_Integer_Literal =>
            return Create_Value_Discrete (Get_Value (Expr), Expr_Type);
         when Iir_Kind_Floating_Point_Literal =>
            return Create_Value_Float (Get_Fp_Value (Expr), Expr_Type);
         when Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal =>
            return Create_Value_Discrete
              (Get_Physical_Value (Expr), Expr_Type);
         when Iir_Kind_String_Literal8 =>
            return Synth_String_Literal (Syn_Inst, Expr, Expr_Type);
         when Iir_Kind_Enumeration_Literal =>
            return Synth_Name (Syn_Inst, Expr);
         when Iir_Kind_Type_Conversion =>
            return Synth_Type_Conversion (Syn_Inst, Expr);
         when Iir_Kind_Qualified_Expression =>
            return Synth_Expression_With_Type
              (Syn_Inst, Get_Expression (Expr),
               Get_Value_Type (Syn_Inst, Get_Type (Get_Type_Mark (Expr))));
         when Iir_Kind_Function_Call =>
            declare
               Imp : constant Node := Get_Implementation (Expr);
            begin
               if Get_Implicit_Definition (Imp) /= Iir_Predefined_None then
                  return Synth_Predefined_Function_Call (Syn_Inst, Expr);
               else
                  return Synth_User_Function_Call (Syn_Inst, Expr);
               end if;
            end;
         when Iir_Kind_Aggregate =>
            return Synth_Aggregate (Syn_Inst, Expr, Expr_Type);
         when Iir_Kind_Simple_Aggregate =>
            return Synth_Simple_Aggregate (Syn_Inst, Expr);
         when Iir_Kind_Left_Array_Attribute =>
            declare
               B : Bound_Type;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Expr);
               return Create_Value_Discrete (Int64 (B.Left), Expr_Type);
            end;
         when Iir_Kind_Right_Array_Attribute =>
            declare
               B : Bound_Type;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Expr);
               return Create_Value_Discrete (Int64 (B.Right), Expr_Type);
            end;
         when Iir_Kind_High_Array_Attribute =>
            declare
               B : Bound_Type;
               V : Int32;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Expr);
               case B.Dir is
                  when Iir_To =>
                     V := B.Right;
                  when Iir_Downto =>
                     V := B.Left;
               end case;
               return Create_Value_Discrete (Int64 (V), Expr_Type);
            end;
         when Iir_Kind_Low_Array_Attribute =>
            declare
               B : Bound_Type;
               V : Int32;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Expr);
               case B.Dir is
                  when Iir_To =>
                     V := B.Left;
                  when Iir_Downto =>
                     V := B.Right;
               end case;
               return Create_Value_Discrete (Int64 (V), Expr_Type);
            end;
         when Iir_Kind_Length_Array_Attribute =>
            declare
               B : Bound_Type;
            begin
               B := Synth_Array_Attribute (Syn_Inst, Expr);
               return Create_Value_Discrete (Int64 (B.Len), Expr_Type);
            end;
         when Iir_Kind_Pos_Attribute =>
            declare
               Param : constant Node := Get_Parameter (Expr);
               V : Value_Acc;
               Dtype : Type_Acc;
            begin
               V := Synth_Expression (Syn_Inst, Param);
               Dtype := Get_Value_Type (Syn_Inst, Get_Type (Expr));
               --  FIXME: to be generalized.  Not always as simple as a
               --  subtype conversion.
               return Synth_Subtype_Conversion (V, Dtype, False, Expr);
            end;
         when Iir_Kind_Null_Literal =>
            return Create_Value_Access (Expr_Type, Null_Heap_Index);
         when Iir_Kind_Allocator_By_Subtype =>
            declare
               T : Type_Acc;
               Acc : Heap_Index;
            begin
               T := Synth.Decls.Synth_Subtype_Indication
                 (Syn_Inst, Get_Subtype_Indication (Expr));
               Acc := Allocate_By_Type (T);
               return Create_Value_Access (Expr_Type, Acc);
            end;
         when Iir_Kind_Allocator_By_Expression =>
            declare
               V : Value_Acc;
               Acc : Heap_Index;
            begin
               V := Synth_Expression_With_Type
                 (Syn_Inst, Get_Expression (Expr), Expr_Type.Acc_Acc);
               Acc := Allocate_By_Value (V);
               return Create_Value_Access (Expr_Type, Acc);
            end;
         when Iir_Kind_Overflow_Literal =>
            declare
               N : Net;
            begin
               Error_Msg_Synth
                 (+Expr, "error detected during analysis injected");
               N := Build_Const_X (Get_Build (Syn_Inst), Expr_Type.W);
               return Create_Value_Net (N, Expr_Type);
            end;
         when others =>
            Error_Kind ("synth_expression_with_type", Expr);
      end case;
   end Synth_Expression_With_Type;

   function Synth_Expression (Syn_Inst : Synth_Instance_Acc; Expr : Node)
                             return Value_Acc is
   begin
      return Synth_Expression_With_Type
        (Syn_Inst, Expr, Get_Value_Type (Syn_Inst, Get_Type (Expr)));
   end Synth_Expression;

   function Synth_Expression_With_Basetype
     (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Value_Acc
   is
      Basetype : Type_Acc;
   begin
      Basetype := Get_Value_Type (Syn_Inst, Get_Base_Type (Get_Type (Expr)));
      return Synth_Expression_With_Type (Syn_Inst, Expr, Basetype);
   end Synth_Expression_With_Basetype;
end Synth.Expr;
