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

with Synth.Errors; use Synth.Errors;
with Synth.Source; use Synth.Source;
with Synth.Expr; use Synth.Expr;
with Synth.Ieee.Std_Logic_1164;

package body Synth.Static_Oper is
   --  From openiee:

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

   function Synth_Vector_And (L, R : Value_Acc; Loc : Syn_Src)
                             return Value_Acc
   is
      use Synth.Ieee.Std_Logic_1164;
      El_Typ : constant Type_Acc := L.Typ.Vec_El;
      Arr : Value_Array_Acc;
   begin
      if L.Arr.Len /= R.Arr.Len then
         Error_Msg_Synth (+Loc, "length of operands mismatch");
         return null;
      end if;

      Arr := Create_Value_Array (L.Arr.Len);
      for I in Arr.V'Range loop
         declare
            Ls : constant Std_Ulogic :=
              Std_Ulogic'Val (L.Arr.V (I).Scal);
            Rs : constant Std_Ulogic :=
              Std_Ulogic'Val (R.Arr.V (I).Scal);
            V : constant Std_Ulogic := And_Table (Ls, Rs);
         begin
            Arr.V (I) := Create_Value_Discrete (Std_Ulogic'Pos (V), El_Typ);
         end;
      end loop;

      return Create_Value_Array (Create_Res_Bound (L.Typ), Arr);
   end Synth_Vector_And;

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
              (Get_Const_Discrete (Left) + Get_Const_Discrete (Right),
               Res_Typ);
         when Iir_Predefined_Integer_Minus =>
            return Create_Value_Discrete
              (Get_Const_Discrete (Left) - Get_Const_Discrete (Right),
               Res_Typ);
         when Iir_Predefined_Integer_Mul =>
            return Create_Value_Discrete
              (Get_Const_Discrete (Left) * Get_Const_Discrete (Right),
               Res_Typ);
         when Iir_Predefined_Integer_Div =>
            return Create_Value_Discrete
              (Left.Scal / Right.Scal, Res_Typ);
         when Iir_Predefined_Integer_Mod =>
            return Create_Value_Discrete
              (Left.Scal mod Right.Scal, Res_Typ);
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
              (Boolean'Pos (Left.Scal = Right.Scal), Boolean_Type);
         when Iir_Predefined_Integer_Inequality =>
            return Create_Value_Discrete
              (Boolean'Pos (Left.Scal /= Right.Scal), Boolean_Type);
         when Iir_Predefined_Physical_Physical_Div =>
            return Create_Value_Discrete
              (Left.Scal / Right.Scal, Res_Typ);

         when Iir_Predefined_Floating_Div =>
            return Create_Value_Float
              (Left.Fp / Right.Fp, Res_Typ);

         when Iir_Predefined_Array_Equality
           | Iir_Predefined_Record_Equality =>
            return Create_Value_Discrete
              (Boolean'Pos (Is_Equal (Left, Right)), Boolean_Type);
         when Iir_Predefined_Array_Inequality
            | Iir_Predefined_Record_Inequality =>
            return Create_Value_Discrete
              (Boolean'Pos (not Is_Equal (Left, Right)), Boolean_Type);

         when Iir_Predefined_Ieee_1164_Vector_And =>
            return Synth_Vector_And (Left, Right, Expr);

         when others =>
            Error_Msg_Synth
              (+Expr, "synth_static_dyadic_predefined: unhandled "
                 & Iir_Predefined_Functions'Image (Def));
            raise Internal_Error;
      end case;
   end Synth_Static_Dyadic_Predefined;
end Synth.Static_Oper;
