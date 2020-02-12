--  Operations synthesis.
--  Copyright (C) 2019 Tristan Gingold
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

with Types; use Types;

with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Ieee.Std_Logic_1164;

with Netlists; use Netlists;
with Netlists.Utils; use Netlists.Utils;

with Synth.Errors; use Synth.Errors;
with Synth.Source; use Synth.Source;
with Synth.Environment;
with Synth.Expr; use Synth.Expr;
with Synth.Oper;
with Synth.Ieee.Std_Logic_1164; use Synth.Ieee.Std_Logic_1164;
with Synth.Ieee.Numeric_Std; use Synth.Ieee.Numeric_Std;

package body Synth.Static_Oper is
   --  From openiee:

   type Static_Arr_Kind is (Sarr_Value, Sarr_Net);

   type Static_Arr_Type (Kind : Static_Arr_Kind) is record
      case Kind is
         when Sarr_Value =>
            Arr : Value_Array_Acc;
         when Sarr_Net =>
            N : Net;
      end case;
   end record;

   function Get_Static_Array (V : Value_Acc) return Static_Arr_Type
   is
      N : Net;
   begin
      case V.Kind is
         when Value_Const =>
            return (Kind => Sarr_Value, Arr => V.C_Val.Arr);
         when Value_Const_Array =>
            return (Kind => Sarr_Value, Arr => V.Arr);
         when Value_Net =>
            N := V.N;
         when Value_Wire =>
            N := Synth.Environment.Get_Const_Wire (V.W);
         when others =>
            raise Internal_Error;
      end case;
      return (Kind => Sarr_Net, N => N);
   end Get_Static_Array;

   function Logic_To_Std_Logic (Va : Uns32; Zx : Uns32) return Std_Ulogic
   is
      subtype Uns4 is Uns32 range 0 .. 3;
   begin
      case Uns4 (Va + 2 * Zx) is
         when 0 =>
            return Std_Ulogic'Val (Vhdl.Ieee.Std_Logic_1164.Std_Logic_0_Pos);
         when 1 =>
            return Std_Ulogic'Val (Vhdl.Ieee.Std_Logic_1164.Std_Logic_1_Pos);
         when 2 =>
            return Std_Ulogic'Val (Vhdl.Ieee.Std_Logic_1164.Std_Logic_Z_Pos);
         when 3 =>
            return Std_Ulogic'Val (Vhdl.Ieee.Std_Logic_1164.Std_Logic_X_Pos);
      end case;
   end Logic_To_Std_Logic;

   function Get_Static_Std_Logic (Sarr : Static_Arr_Type; Off : Uns32)
                                 return Std_Ulogic is
   begin
      case Sarr.Kind is
         when Sarr_Value =>
            return Std_Ulogic'Val (Sarr.Arr.V (Iir_Index32 (Off + 1)).Scal);
         when Sarr_Net =>
            declare
               Va : Uns32;
               Zx : Uns32;
            begin
               Get_Net_Element (Sarr.N, Off, Va, Zx);
               return Logic_To_Std_Logic (Va, Zx);
            end;
      end case;
   end Get_Static_Std_Logic;

   function Create_Res_Bound (Prev : Type_Acc) return Type_Acc is
   begin
      if Prev.Vbound.Dir = Iir_Downto
        and then Prev.Vbound.Right = 0
      then
         --  Normalized range
         return Prev;
      end if;

      return Create_Vec_Type_By_Length (Prev.W, Prev.Vec_El);
   end Create_Res_Bound;

   function Synth_Vector_Dyadic (Left, Right : Value_Acc;
                                 Op : Table_2d;
                                 Loc : Syn_Src) return Value_Acc
   is
      El_Typ : constant Type_Acc := Left.Typ.Vec_El;
      Larr : constant Static_Arr_Type := Get_Static_Array (Left);
      Rarr : constant Static_Arr_Type := Get_Static_Array (Right);
      Arr : Value_Array_Acc;
   begin
      if Left.Typ.W /= Right.Typ.W then
         Error_Msg_Synth (+Loc, "length of operands mismatch");
         return null;
      end if;

      Arr := Create_Value_Array (Iir_Index32 (Left.Typ.W));
      for I in Arr.V'Range loop
         declare
            Ls : constant Std_Ulogic :=
              Get_Static_Std_Logic (Larr, Uns32 (I - 1));
            Rs : constant Std_Ulogic :=
              Get_Static_Std_Logic (Rarr, Uns32 (I - 1));
            V : constant Std_Ulogic := Op (Ls, Rs);
         begin
            Arr.V (I) := Create_Value_Discrete (Std_Ulogic'Pos (V), El_Typ);
         end;
      end loop;

      return Create_Value_Const_Array (Create_Res_Bound (Left.Typ), Arr);
   end Synth_Vector_Dyadic;

   procedure To_Std_Logic_Vector
     (Val : Value_Acc; Arr : out Std_Logic_Vector) is
   begin
      for I in Val.Arr.V'Range loop
         Arr (Natural (I)) := Std_Ulogic'Val (Val.Arr.V (I).Scal);
      end loop;
   end To_Std_Logic_Vector;

   function To_Value_Acc (Vec : Std_Logic_Vector; El_Typ : Type_Acc)
                         return Value_Acc
   is
      pragma Assert (Vec'First = 1);
      Res_Typ : Type_Acc;
      Arr : Value_Array_Acc;
   begin
      Res_Typ := Create_Vec_Type_By_Length (Uns32 (Vec'Last), El_Typ);
      Arr := Create_Value_Array (Iir_Index32 (Vec'Last));
      for I in Vec'Range loop
         Arr.V (Iir_Index32 (I)) :=
           Create_Value_Discrete (Std_Ulogic'Pos (Vec (I)), El_Typ);
      end loop;
      return Create_Value_Const_Array (Res_Typ, Arr);
   end To_Value_Acc;

   function Synth_Add_Uns_Uns (L, R : Value_Acc; Loc : Syn_Src)
                              return Value_Acc
   is
      pragma Unreferenced (Loc);
      L_Arr : Std_Logic_Vector (1 .. Natural (L.Arr.Len));
      R_Arr : Std_Logic_Vector (1 .. Natural (R.Arr.Len));
   begin
      To_Std_Logic_Vector (L, L_Arr);
      To_Std_Logic_Vector (R, R_Arr);
      declare
         Res_Arr : constant Std_Logic_Vector := Add_Uns_Uns (L_Arr, R_Arr);
      begin
         return To_Value_Acc (Res_Arr, L.Typ.Vec_El);
      end;
   end Synth_Add_Uns_Uns;

   function Synth_Add_Sgn_Int (L, R : Value_Acc; Loc : Syn_Src)
                              return Value_Acc
   is
      pragma Unreferenced (Loc);
      L_Arr : Std_Logic_Vector (1 .. Natural (L.Arr.Len));
      R_Val : constant Int64 := R.Scal;
   begin
      To_Std_Logic_Vector (L, L_Arr);
      declare
         Res_Arr : constant Std_Logic_Vector := Add_Sgn_Int (L_Arr, R_Val);
      begin
         return To_Value_Acc (Res_Arr, L.Typ.Vec_El);
      end;
   end Synth_Add_Sgn_Int;

   function Synth_Add_Uns_Nat (L, R : Value_Acc; Loc : Syn_Src)
                              return Value_Acc
   is
      pragma Unreferenced (Loc);
      L_Arr : Std_Logic_Vector (1 .. Natural (L.Arr.Len));
      R_Val : constant Uns64 := Uns64 (R.Scal);
   begin
      To_Std_Logic_Vector (L, L_Arr);
      declare
         Res_Arr : constant Std_Logic_Vector := Add_Uns_Nat (L_Arr, R_Val);
      begin
         return To_Value_Acc (Res_Arr, L.Typ.Vec_El);
      end;
   end Synth_Add_Uns_Nat;

   function Synth_Sub_Uns_Uns (L, R : Value_Acc; Loc : Syn_Src)
                              return Value_Acc
   is
      pragma Unreferenced (Loc);
      L_Arr : Std_Logic_Vector (1 .. Natural (L.Arr.Len));
      R_Arr : Std_Logic_Vector (1 .. Natural (R.Arr.Len));
   begin
      To_Std_Logic_Vector (L, L_Arr);
      To_Std_Logic_Vector (R, R_Arr);
      declare
         Res_Arr : constant Std_Logic_Vector := Sub_Uns_Uns (L_Arr, R_Arr);
      begin
         return To_Value_Acc (Res_Arr, L.Typ.Vec_El);
      end;
   end Synth_Sub_Uns_Uns;

   function Synth_Mul_Uns_Uns (L, R : Value_Acc; Loc : Syn_Src)
                              return Value_Acc
   is
      pragma Unreferenced (Loc);
      L_Arr : Std_Logic_Vector (1 .. Natural (L.Arr.Len));
      R_Arr : Std_Logic_Vector (1 .. Natural (R.Arr.Len));
   begin
      To_Std_Logic_Vector (L, L_Arr);
      To_Std_Logic_Vector (R, R_Arr);
      declare
         Res_Arr : constant Std_Logic_Vector := Mul_Uns_Uns (L_Arr, R_Arr);
      begin
         return To_Value_Acc (Res_Arr, L.Typ.Vec_El);
      end;
   end Synth_Mul_Uns_Uns;

   function Synth_Mul_Sgn_Sgn (L, R : Value_Acc; Loc : Syn_Src)
                              return Value_Acc
   is
      pragma Unreferenced (Loc);
      L_Arr : Std_Logic_Vector (1 .. Natural (L.Arr.Len));
      R_Arr : Std_Logic_Vector (1 .. Natural (R.Arr.Len));
   begin
      To_Std_Logic_Vector (L, L_Arr);
      To_Std_Logic_Vector (R, R_Arr);
      declare
         Res_Arr : constant Std_Logic_Vector := Mul_Sgn_Sgn (L_Arr, R_Arr);
      begin
         return To_Value_Acc (Res_Arr, L.Typ.Vec_El);
      end;
   end Synth_Mul_Sgn_Sgn;

   function Synth_Shift (Val : Value_Acc;
                         Amt : Uns32;
                         Right : Boolean;
                         Arith : Boolean) return Value_Acc
   is
      Len : constant Uns32 := Uns32 (Val.Arr.Len);
      Arr : Std_Logic_Vector (1 .. Natural (Len));
      Pad : Std_Ulogic;
   begin
      if Len = 0 or Amt >= Len then
         Arr := (others => '0');
      else
         To_Std_Logic_Vector (Val, Arr);
         if Arith then
            Pad := Arr (1);
         else
            Pad := '0';
         end if;

         if Right then
            for I in reverse Amt + 1 .. Len loop
               Arr (Natural (I)) := Arr (Natural (I - Amt));
            end loop;
            for I in 1 .. Amt loop
               Arr (Natural (I)) := Pad;
            end loop;
         else
            for I in 1 .. Len - Amt loop
               Arr (Natural (I)) := Arr (Natural (I + Amt));
            end loop;
            for I in Len - Amt + 1 .. Len loop
               Arr (Natural (I)) := Pad;
            end loop;
         end if;
      end if;
      return To_Value_Acc (Arr, Val.Typ.Vec_El);
   end Synth_Shift;

   function Synth_Static_Dyadic_Predefined (Syn_Inst : Synth_Instance_Acc;
                                            Imp : Node;
                                            Left : Value_Acc;
                                            Right : Value_Acc;
                                            Expr : Node) return Value_Acc
   is
      Def : constant Iir_Predefined_Functions :=
        Get_Implicit_Definition (Imp);
      Res_Typ : constant Type_Acc :=
        Get_Value_Type (Syn_Inst, Get_Type (Expr));
   begin
      case Def is
         when Iir_Predefined_Error =>
            return null;

         when Iir_Predefined_Enum_Equality =>
            return Create_Value_Discrete
              (Boolean'Pos (Left.Scal = Right.Scal), Boolean_Type);

         when Iir_Predefined_Integer_Plus =>
            return Create_Value_Discrete
              (Get_Static_Discrete (Left) + Get_Static_Discrete (Right),
               Res_Typ);
         when Iir_Predefined_Integer_Minus =>
            return Create_Value_Discrete
              (Get_Static_Discrete (Left) - Get_Static_Discrete (Right),
               Res_Typ);
         when Iir_Predefined_Integer_Mul =>
            return Create_Value_Discrete
              (Get_Static_Discrete (Left) * Get_Static_Discrete (Right),
               Res_Typ);
         when Iir_Predefined_Integer_Div =>
            return Create_Value_Discrete
              (Get_Static_Discrete (Left) / Get_Static_Discrete (Right),
               Res_Typ);
         when Iir_Predefined_Integer_Mod =>
            return Create_Value_Discrete
              (Get_Static_Discrete (Left) mod Get_Static_Discrete (Right),
               Res_Typ);
         when Iir_Predefined_Integer_Rem =>
            return Create_Value_Discrete
              (Left.Scal rem Right.Scal, Res_Typ);
         when Iir_Predefined_Integer_Exp =>
            return Create_Value_Discrete
              (Left.Scal ** Natural (Right.Scal), Res_Typ);
         when Iir_Predefined_Integer_Less_Equal =>
            return Create_Value_Discrete
              (Boolean'Pos (Left.Scal <= Right.Scal), Boolean_Type);
         when Iir_Predefined_Integer_Less =>
            return Create_Value_Discrete
              (Boolean'Pos (Left.Scal < Right.Scal), Boolean_Type);
         when Iir_Predefined_Integer_Greater_Equal =>
            return Create_Value_Discrete
              (Boolean'Pos (Left.Scal >= Right.Scal), Boolean_Type);
         when Iir_Predefined_Integer_Greater =>
            return Create_Value_Discrete
              (Boolean'Pos (Left.Scal > Right.Scal), Boolean_Type);
         when Iir_Predefined_Integer_Equality =>
            return Create_Value_Discrete
              (Boolean'Pos (Get_Static_Discrete (Left)
                              = Get_Static_Discrete (Right)), Boolean_Type);
         when Iir_Predefined_Integer_Inequality =>
            return Create_Value_Discrete
              (Boolean'Pos (Get_Static_Discrete (Left)
                              /= Get_Static_Discrete (Right)), Boolean_Type);
         when Iir_Predefined_Physical_Physical_Div
           | Iir_Predefined_Physical_Integer_Div =>
            return Create_Value_Discrete
              (Left.Scal / Right.Scal, Res_Typ);

         when Iir_Predefined_Floating_Less =>
            return Create_Value_Discrete
              (Boolean'Pos (Left.Fp < Right.Fp), Boolean_Type);
         when Iir_Predefined_Floating_Less_Equal =>
            return Create_Value_Discrete
              (Boolean'Pos (Left.Fp <= Right.Fp), Boolean_Type);
         when Iir_Predefined_Floating_Equality =>
            return Create_Value_Discrete
              (Boolean'Pos (Left.Fp = Right.Fp), Boolean_Type);
         when Iir_Predefined_Floating_Inequality =>
            return Create_Value_Discrete
              (Boolean'Pos (Left.Fp /= Right.Fp), Boolean_Type);
         when Iir_Predefined_Floating_Greater =>
            return Create_Value_Discrete
              (Boolean'Pos (Left.Fp > Right.Fp), Boolean_Type);
         when Iir_Predefined_Floating_Greater_Equal =>
            return Create_Value_Discrete
              (Boolean'Pos (Left.Fp >= Right.Fp), Boolean_Type);

         when Iir_Predefined_Floating_Plus =>
            return Create_Value_Float (Left.Fp + Right.Fp, Res_Typ);
         when Iir_Predefined_Floating_Minus =>
            return Create_Value_Float (Left.Fp - Right.Fp, Res_Typ);
         when Iir_Predefined_Floating_Mul =>
            return Create_Value_Float (Left.Fp * Right.Fp, Res_Typ);
         when Iir_Predefined_Floating_Div =>
            return Create_Value_Float (Left.Fp / Right.Fp, Res_Typ);
         when Iir_Predefined_Floating_Exp =>
            return Create_Value_Float
              (Left.Fp ** Natural (Right.Scal), Res_Typ);

         when Iir_Predefined_Array_Array_Concat =>
            declare
               Ret_Typ : constant Type_Acc :=
                 Get_Value_Type (Syn_Inst, Get_Return_Type (Imp));
               L_Len : constant Iir_Index32 :=
                 Iir_Index32 (Get_Bound_Length (Left.Typ, 1));
               R_Len : constant Iir_Index32 :=
                 Iir_Index32 (Get_Bound_Length (Right.Typ, 1));
               Bnd : Bound_Type;
               Res_Typ : Type_Acc;
               Arr : Value_Array_Acc;
            begin
               Bnd := Oper.Create_Bounds_From_Length
                 (Syn_Inst, Get_Index_Type (Get_Type (Expr), 0),
                  L_Len + R_Len);
               Res_Typ := Create_Onedimensional_Array_Subtype
                 (Ret_Typ, Bnd);
               Arr := Create_Value_Array (L_Len + R_Len);
               for I in 1 .. L_Len loop
                  Arr.V (I) := Left.Arr.V (I);
               end loop;
               for I in 1 .. R_Len loop
                  Arr.V (L_Len + I) := Right.Arr.V (I);
               end loop;
               return Create_Value_Const_Array (Res_Typ, Arr);
            end;
         when Iir_Predefined_Element_Array_Concat =>
            declare
               Ret_Typ : constant Type_Acc :=
                 Get_Value_Type (Syn_Inst, Get_Return_Type (Imp));
               Bnd : Bound_Type;
               Res_Typ : Type_Acc;
               Arr : Value_Array_Acc;
            begin
               Bnd := Oper.Create_Bounds_From_Length
                 (Syn_Inst, Get_Index_Type (Get_Type (Expr), 0),
                  1 + Right.Arr.Len);
               Res_Typ := Create_Onedimensional_Array_Subtype
                 (Ret_Typ, Bnd);
               Arr := Create_Value_Array (1 + Right.Arr.Len);
               Arr.V (1) := Left;
               for I in Right.Arr.V'Range loop
                  Arr.V (1 + I) := Right.Arr.V (I);
               end loop;
               return Create_Value_Const_Array (Res_Typ, Arr);
            end;
         when Iir_Predefined_Array_Element_Concat =>
            declare
               Ret_Typ : constant Type_Acc :=
                 Get_Value_Type (Syn_Inst, Get_Return_Type (Imp));
               Bnd : Bound_Type;
               Res_Typ : Type_Acc;
               Arr : Value_Array_Acc;
            begin
               Bnd := Oper.Create_Bounds_From_Length
                 (Syn_Inst, Get_Index_Type (Get_Type (Expr), 0),
                  Left.Arr.Len + 1);
               Res_Typ := Create_Onedimensional_Array_Subtype
                 (Ret_Typ, Bnd);
               Arr := Create_Value_Array (Left.Arr.Len + 1);
               for I in Left.Arr.V'Range loop
                  Arr.V (I) := Left.Arr.V (I);
               end loop;
               Arr.V (Left.Arr.Len + 1) := Right;
               return Create_Value_Const_Array (Res_Typ, Arr);
            end;

         when Iir_Predefined_Array_Equality
           | Iir_Predefined_Record_Equality =>
            return Create_Value_Discrete
              (Boolean'Pos (Is_Equal (Left, Right)), Boolean_Type);
         when Iir_Predefined_Array_Inequality
            | Iir_Predefined_Record_Inequality =>
            return Create_Value_Discrete
              (Boolean'Pos (not Is_Equal (Left, Right)), Boolean_Type);

         when Iir_Predefined_Access_Equality =>
            return Create_Value_Discrete
              (Boolean'Pos (Left.Acc = Right.Acc), Boolean_Type);
         when Iir_Predefined_Access_Inequality =>
            return Create_Value_Discrete
              (Boolean'Pos (Left.Acc /= Right.Acc), Boolean_Type);

         when Iir_Predefined_Ieee_1164_Vector_And
           | Iir_Predefined_Ieee_Numeric_Std_And_Uns_Uns
           | Iir_Predefined_Ieee_Numeric_Std_And_Sgn_Sgn =>
            return Synth_Vector_Dyadic (Left, Right, And_Table, Expr);

         when Iir_Predefined_Ieee_1164_Vector_Or
           | Iir_Predefined_Ieee_Numeric_Std_Or_Uns_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Or_Sgn_Sgn =>
            return Synth_Vector_Dyadic (Left, Right, Or_Table, Expr);

         when Iir_Predefined_Ieee_1164_Vector_Xor
           | Iir_Predefined_Ieee_Numeric_Std_Xor_Uns_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Xor_Sgn_Sgn =>
            return Synth_Vector_Dyadic (Left, Right, Xor_Table, Expr);

         when Iir_Predefined_Ieee_1164_Scalar_Xor =>
            return Create_Value_Discrete
              (Std_Ulogic'Pos (Xor_Table (Std_Ulogic'Val (Left.Scal),
                                          Std_Ulogic'Val (Right.Scal))),
               Res_Typ);

         when Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Uns =>
            return Synth_Add_Uns_Uns (Left, Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Int =>
            return Synth_Add_Sgn_Int (Left, Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Nat =>
            return Synth_Add_Uns_Nat (Left, Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Uns =>
            return Synth_Sub_Uns_Uns (Left, Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Mul_Uns_Uns =>
            return Synth_Mul_Uns_Uns (Left, Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Mul_Sgn_Sgn =>
            return Synth_Mul_Sgn_Sgn (Left, Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Srl_Uns_Int =>
            declare
               Amt : Int64;
            begin
               Amt := Get_Static_Discrete (Right);
               if Amt >= 0 then
                  return Synth_Shift (Left, Uns32 (Amt), True, False);
               else
                  return Synth_Shift (Left, Uns32 (-Amt), False, False);
               end if;
            end;

         when others =>
            Error_Msg_Synth
              (+Expr, "synth_static_dyadic_predefined: unhandled "
                 & Iir_Predefined_Functions'Image (Def));
            raise Internal_Error;
      end case;
   end Synth_Static_Dyadic_Predefined;

   function Synth_Vector_Monadic
     (Vec : Value_Acc; Op : Table_1d) return Value_Acc
   is
      El_Typ : constant Type_Acc := Vec.Typ.Vec_El;
      Arr : Value_Array_Acc;
   begin
      Arr := Create_Value_Array (Vec.Arr.Len);
      for I in Arr.V'Range loop
         declare
            V : constant Std_Ulogic := Std_Ulogic'Val (Vec.Arr.V (I).Scal);
         begin
            Arr.V (I) :=
              Create_Value_Discrete (Std_Ulogic'Pos (Op (V)), El_Typ);
         end;
      end loop;

      return Create_Value_Const_Array (Create_Res_Bound (Vec.Typ), Arr);
   end Synth_Vector_Monadic;

   function Synth_Vector_Reduce
     (Init : Std_Ulogic; Vec : Value_Acc; Op : Table_2d) return Value_Acc
   is
      El_Typ : constant Type_Acc := Vec.Typ.Vec_El;
      Res : Std_Ulogic;
   begin
      Res := Init;
      for I in Vec.Arr.V'Range loop
         declare
            V : constant Std_Ulogic :=
              Std_Ulogic'Val (Vec.Arr.V (I).Scal);
         begin
            Res := Op (Res, V);
         end;
      end loop;

      return Create_Value_Discrete (Std_Ulogic'Pos (Res), El_Typ);
   end Synth_Vector_Reduce;

   function Synth_Static_Monadic_Predefined (Syn_Inst : Synth_Instance_Acc;
                                             Imp : Node;
                                             Operand : Value_Acc;
                                             Expr : Node) return Value_Acc
   is
      Def : constant Iir_Predefined_Functions :=
        Get_Implicit_Definition (Imp);
      Inter_Chain : constant Node :=
        Get_Interface_Declaration_Chain (Imp);
      Oper_Type : constant Node := Get_Type (Inter_Chain);
      Oper_Typ : constant Type_Acc := Get_Value_Type (Syn_Inst, Oper_Type);
      --  Res_Typ : constant Type_Acc :=
      --    Get_Value_Type (Syn_Inst, Get_Type (Expr));
   begin
      case Def is
         when Iir_Predefined_Boolean_Not
           | Iir_Predefined_Bit_Not =>
            return Create_Value_Discrete (1 - Operand.Scal, Oper_Typ);

         when Iir_Predefined_Integer_Negation =>
            return Create_Value_Discrete (-Operand.Scal, Oper_Typ);
         when Iir_Predefined_Integer_Absolute =>
            return Create_Value_Discrete (abs Operand.Scal, Oper_Typ);

         when Iir_Predefined_Floating_Negation =>
            return Create_Value_Float (-Operand.Fp, Oper_Typ);
         when Iir_Predefined_Floating_Identity =>
            return Operand;
         when Iir_Predefined_Floating_Absolute =>
            return Create_Value_Float (abs Operand.Fp, Oper_Typ);

         when Iir_Predefined_Ieee_1164_Condition_Operator =>
            --  Constant std_logic: need to convert.
            declare
               Val : Uns32;
               Zx : Uns32;
            begin
               From_Std_Logic (Operand.Scal, Val, Zx);
               return Create_Value_Discrete
                 (Boolean'Pos (Val = 1 and Zx = 0), Boolean_Type);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Neg_Sgn =>
            declare
               Op_Arr : Std_Logic_Vector (1 .. Natural (Operand.Arr.Len));
            begin
               To_Std_Logic_Vector (Operand, Op_Arr);
               declare
                  Res_Arr : constant Std_Logic_Vector := Neg_Sgn (Op_Arr);
               begin
                  return To_Value_Acc (Res_Arr, Operand.Typ.Vec_El);
               end;
            end;

         when Iir_Predefined_Ieee_1164_Vector_Not =>
            return Synth_Vector_Monadic (Operand, Not_Table);

         when Iir_Predefined_Ieee_1164_Scalar_Not =>
            return Create_Value_Discrete
              (Std_Ulogic'Pos (Not_Table (Std_Ulogic'Val (Operand.Scal))),
               Oper_Typ);

         when Iir_Predefined_Ieee_1164_Vector_Or_Reduce =>
            return Synth_Vector_Reduce ('0', Operand, Or_Table);

         when others =>
            Error_Msg_Synth
              (+Expr, "synth_static_monadic_predefined: unhandled "
                 & Iir_Predefined_Functions'Image (Def));
            raise Internal_Error;
      end case;
   end Synth_Static_Monadic_Predefined;
end Synth.Static_Oper;
