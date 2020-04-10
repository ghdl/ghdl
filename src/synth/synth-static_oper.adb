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
with Types_Utils; use Types_Utils;

with Grt.Types;

with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Ieee.Std_Logic_1164; use Vhdl.Ieee.Std_Logic_1164;

with Netlists; use Netlists;

with Synth.Errors; use Synth.Errors;
with Synth.Source; use Synth.Source;
with Synth.Expr; use Synth.Expr;
with Synth.Oper;
with Synth.Ieee.Std_Logic_1164; use Synth.Ieee.Std_Logic_1164;
with Synth.Ieee.Numeric_Std; use Synth.Ieee.Numeric_Std;
with Synth.Files_Operations;

package body Synth.Static_Oper is
   --  As log2(3m) is directly referenced, the program must be linked with -lm
   --  (math library) on unix systems.
   pragma Linker_Options ("-lm");

   function Read_Std_Logic (M : Memory_Ptr; Off : Uns32) return Std_Ulogic is
   begin
      return Std_Ulogic'Val (Read_U8 (M + Size_Type (Off)));
   end Read_Std_Logic;

   procedure Write_Std_Logic (M : Memory_Ptr; Off : Uns32; Val : Std_Ulogic) is
   begin
      Write_U8 (M + Size_Type (Off), Std_Ulogic'Pos (Val));
   end Write_Std_Logic;

   procedure Warn_Compare_Null (Loc : Node) is
   begin
      Warning_Msg_Synth (+Loc, "null argument detected, returning false");
   end Warn_Compare_Null;

   procedure Warn_Compare_Meta (Loc : Node) is
   begin
      Warning_Msg_Synth (+Loc, "metavalue detected, returning false");
   end Warn_Compare_Meta;

   function Synth_Compare_Uns_Uns
     (Left, Right : Memtyp; Err : Order_Type; Loc : Node) return Order_Type
   is
      Lw : constant Uns32 := Left.Typ.W;
      Rw : constant Uns32 := Right.Typ.W;
      Len : constant Uns32 := Uns32'Min (Left.Typ.W, Right.Typ.W);
      L, R : Std_Ulogic;
   begin
      if Len = 0 then
         Warn_Compare_Null (Loc);
         return Err;
      end if;

      if Lw > Rw then
         for I in 0 .. Lw - Rw - 1 loop
            case To_X01 (Read_Std_Logic (Left.Mem, I)) is
               when '0' =>
                  null;
               when '1' =>
                  return Greater;
               when 'X' =>
                  Warn_Compare_Meta (Loc);
                  return Err;
            end case;
         end loop;
      elsif Lw < Rw then
         for I in 0 .. Rw - Lw - 1 loop
            case To_X01 (Read_Std_Logic (Right.Mem, I)) is
               when '0' =>
                  null;
               when '1' =>
                  return Less;
               when 'X' =>
                  Warn_Compare_Meta (Loc);
                  return Err;
            end case;
         end loop;
      end if;

      for I in 0 .. Len - 1 loop
         L := To_X01 (Read_Std_Logic (Left.Mem, Lw - Len + I));
         R := To_X01 (Read_Std_Logic (Right.Mem, Rw - Len + I));
         if L = 'X' or R = 'X' then
            Warn_Compare_Meta (Loc);
            return Err;
         elsif L = '1' and R = '0' then
            return Greater;
         elsif L = '0' and R = '1' then
            return Less;
         end if;
      end loop;
      return Equal;
   end Synth_Compare_Uns_Uns;

   function Synth_Compare_Uns_Nat
     (Left, Right : Memtyp; Err : Order_Type; Loc : Node) return Order_Type
   is
      Lw : constant Uns32 := Left.Typ.W;
      Rval : constant Uns64 := To_Uns64 (Read_Discrete (Right));
      L : Std_Ulogic;
      Cnt : Uns32;
   begin
      if Lw = 0 then
         Warn_Compare_Null (Loc);
         return Err;
      end if;

      if Lw > 64 then
         for I in 0 .. Lw - 64 - 1 loop
            case To_X01 (Read_Std_Logic (Left.Mem, I)) is
               when '0' =>
                  null;
               when '1' =>
                  return Greater;
               when 'X' =>
                  Warn_Compare_Meta (Loc);
                  return Err;
            end case;
         end loop;
         Cnt := 64;
      elsif Lw < 64 then
         if Shift_Right (Rval, Natural (Lw)) /= 0 then
            return Less;
         end if;
         Cnt := Lw;
      else
         Cnt := 64;
      end if;

      for I in reverse 0 .. Cnt - 1 loop
         L := To_X01 (Read_Std_Logic (Left.Mem, Lw - I - 1));
         if L = 'X' then
            Warn_Compare_Meta (Loc);
            return Err;
         end if;
         if (Shift_Right (Rval, Natural (I)) and 1) = 1 then
            if L = '0' then
               return Less;
            end if;
         else
            if L = '1' then
               return Greater;
            end if;
         end if;
      end loop;
      return Equal;
   end Synth_Compare_Uns_Nat;

   function Synth_Compare_Nat_Uns
     (Left, Right : Memtyp; Err : Order_Type; Loc : Node) return Order_Type
   is
      Rw : constant Uns32 := Right.Typ.W;
      Lval : constant Uns64 := To_Uns64 (Read_Discrete (Left));
      R : Std_Ulogic;
      Cnt : Uns32;
   begin
      if Rw = 0 then
         Warn_Compare_Null (Loc);
         return Err;
      end if;

      if Rw > 64 then
         for I in 0 .. Rw - 64 - 1 loop
            case To_X01 (Read_Std_Logic (Right.Mem, I)) is
               when '0' =>
                  null;
               when '1' =>
                  return Less;
               when 'X' =>
                  Warn_Compare_Meta (Loc);
                  return Err;
            end case;
         end loop;
         Cnt := 64;
      elsif Rw < 64 then
         if Shift_Right (Lval, Natural (Rw)) /= 0 then
            return Greater;
         end if;
         Cnt := Rw;
      else
         Cnt := 64;
      end if;

      for I in reverse 0 .. Cnt - 1 loop
         R := To_X01 (Read_Std_Logic (Right.Mem, Rw - I - 1));
         if R = 'X' then
            Warn_Compare_Meta (Loc);
            return Err;
         end if;
         if (Shift_Right (Lval, Natural (I)) and 1) = 1 then
            if R = '0' then
               return Greater;
            end if;
         else
            if R = '1' then
               return Less;
            end if;
         end if;
      end loop;
      return Equal;
   end Synth_Compare_Nat_Uns;

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

   function Synth_Vector_Dyadic (Left, Right : Memtyp;
                                 Op : Table_2d;
                                 Loc : Syn_Src) return Valtyp
   is
      Res : Valtyp;
   begin
      if Left.Typ.W /= Right.Typ.W then
         Error_Msg_Synth (+Loc, "length of operands mismatch");
         return No_Valtyp;
      end if;

      Res := Create_Value_Memory (Create_Res_Bound (Left.Typ));
      for I in 1 .. Uns32 (Vec_Length (Res.Typ)) loop
         declare
            Ls : constant Std_Ulogic := Read_Std_Logic (Left.Mem, I - 1);
            Rs : constant Std_Ulogic := Read_Std_Logic (Right.Mem, I - 1);
            V : constant Std_Ulogic := Op (Ls, Rs);
         begin
            Write_Std_Logic (Res.Val.Mem, I - 1, V);
         end;
      end loop;

      return Res;
   end Synth_Vector_Dyadic;

   procedure To_Std_Logic_Vector (Val : Memtyp; Arr : out Std_Logic_Vector) is
   begin
      for I in 1 .. Uns32 (Vec_Length (Val.Typ)) loop
         Arr (Natural (I)) := Read_Std_Logic (Val.Mem, I - 1);
      end loop;
   end To_Std_Logic_Vector;

   function To_Valtyp (Vec : Std_Logic_Vector; El_Typ : Type_Acc) return Valtyp
   is
      pragma Assert (Vec'First = 1);
      Res_Typ : Type_Acc;
      Res : Valtyp;
   begin
      Res_Typ := Create_Vec_Type_By_Length (Uns32 (Vec'Last), El_Typ);
      Res := Create_Value_Memory (Res_Typ);
      for I in 1 .. Vec'Last loop
         Write_Std_Logic (Res.Val.Mem, Uns32 (I - 1), Vec (I));
      end loop;
      return Res;
   end To_Valtyp;

   function Synth_Add_Uns_Uns (L, R : Memtyp; Loc : Syn_Src) return Valtyp
   is
      pragma Unreferenced (Loc);
      L_Arr : Std_Logic_Vector (1 .. Natural (Vec_Length (L.Typ)));
      R_Arr : Std_Logic_Vector (1 .. Natural (Vec_Length (R.Typ)));
   begin
      To_Std_Logic_Vector (L, L_Arr);
      To_Std_Logic_Vector (R, R_Arr);
      declare
         Res_Arr : constant Std_Logic_Vector := Add_Uns_Uns (L_Arr, R_Arr);
      begin
         return To_Valtyp (Res_Arr, L.Typ.Vec_El);
      end;
   end Synth_Add_Uns_Uns;

   function Synth_Add_Sgn_Int (L, R : Memtyp; Loc : Syn_Src) return Valtyp
   is
      pragma Unreferenced (Loc);
      L_Arr : Std_Logic_Vector (1 .. Natural (Vec_Length (L.Typ)));
      R_Val : constant Int64 := Read_Discrete (R);
   begin
      To_Std_Logic_Vector (L, L_Arr);
      declare
         Res_Arr : constant Std_Logic_Vector := Add_Sgn_Int (L_Arr, R_Val);
      begin
         return To_Valtyp (Res_Arr, L.Typ.Vec_El);
      end;
   end Synth_Add_Sgn_Int;

   function Synth_Add_Uns_Nat (L, R : Memtyp; Loc : Syn_Src) return Valtyp
   is
      pragma Unreferenced (Loc);
      L_Arr : Std_Logic_Vector (1 .. Natural (L.Typ.W));
      R_Val : constant Uns64 := Uns64 (Read_Discrete (R));
   begin
      To_Std_Logic_Vector (L, L_Arr);
      declare
         Res_Arr : constant Std_Logic_Vector := Add_Uns_Nat (L_Arr, R_Val);
      begin
         return To_Valtyp (Res_Arr, L.Typ.Vec_El);
      end;
   end Synth_Add_Uns_Nat;

   function Synth_Sub_Uns_Uns (L, R : Memtyp; Loc : Syn_Src) return Valtyp
   is
      pragma Unreferenced (Loc);
      L_Arr : Std_Logic_Vector (1 .. Natural (Vec_Length (L.Typ)));
      R_Arr : Std_Logic_Vector (1 .. Natural (Vec_Length (R.Typ)));
   begin
      To_Std_Logic_Vector (L, L_Arr);
      To_Std_Logic_Vector (R, R_Arr);
      declare
         Res_Arr : constant Std_Logic_Vector := Sub_Uns_Uns (L_Arr, R_Arr);
      begin
         return To_Valtyp (Res_Arr, L.Typ.Vec_El);
      end;
   end Synth_Sub_Uns_Uns;

   function Synth_Sub_Uns_Nat (L, R : Memtyp; Loc : Syn_Src) return Valtyp
   is
      pragma Unreferenced (Loc);
      L_Arr : Std_Logic_Vector (1 .. Natural (Vec_Length (L.Typ)));
      R_Val : constant Uns64 := Uns64 (Read_Discrete (R));
   begin
      To_Std_Logic_Vector (L, L_Arr);
      declare
         Res_Arr : constant Std_Logic_Vector := Sub_Uns_Nat (L_Arr, R_Val);
      begin
         return To_Valtyp (Res_Arr, L.Typ.Vec_El);
      end;
   end Synth_Sub_Uns_Nat;

   function Synth_Mul_Uns_Uns (L, R : Memtyp; Loc : Syn_Src) return Valtyp
   is
      pragma Unreferenced (Loc);
      L_Arr : Std_Logic_Vector (1 .. Natural (Vec_Length (L.Typ)));
      R_Arr : Std_Logic_Vector (1 .. Natural (Vec_Length (R.Typ)));
   begin
      To_Std_Logic_Vector (L, L_Arr);
      To_Std_Logic_Vector (R, R_Arr);
      declare
         Res_Arr : constant Std_Logic_Vector := Mul_Uns_Uns (L_Arr, R_Arr);
      begin
         return To_Valtyp (Res_Arr, L.Typ.Vec_El);
      end;
   end Synth_Mul_Uns_Uns;

   function Synth_Mul_Nat_Uns (L, R : Memtyp; Loc : Syn_Src) return Valtyp
   is
      pragma Unreferenced (Loc);
      R_Arr : Std_Logic_Vector (1 .. Natural (Vec_Length (R.Typ)));
      L_Val : constant Uns64 := Uns64 (Read_Discrete (L));
   begin
      To_Std_Logic_Vector (R, R_Arr);
      declare
         Res_Arr : constant Std_Logic_Vector := Mul_Nat_Uns (L_Val, R_Arr);
      begin
         return To_Valtyp (Res_Arr, R.Typ.Vec_El);
      end;
   end Synth_Mul_Nat_Uns;

   function Synth_Mul_Uns_Nat (L, R : Memtyp; Loc : Syn_Src) return Valtyp
   is
      pragma Unreferenced (Loc);
      L_Arr : Std_Logic_Vector (1 .. Natural (Vec_Length (L.Typ)));
      R_Val : constant Uns64 := Uns64 (Read_Discrete (R));
   begin
      To_Std_Logic_Vector (L, L_Arr);
      declare
         Res_Arr : constant Std_Logic_Vector := Mul_Uns_Nat (L_Arr, R_Val);
      begin
         return To_Valtyp (Res_Arr, L.Typ.Vec_El);
      end;
   end Synth_Mul_Uns_Nat;

   function Synth_Mul_Sgn_Sgn (L, R : Memtyp; Loc : Syn_Src) return Valtyp
   is
      pragma Unreferenced (Loc);
      L_Arr : Std_Logic_Vector (1 .. Natural (Vec_Length (L.Typ)));
      R_Arr : Std_Logic_Vector (1 .. Natural (Vec_Length (R.Typ)));
   begin
      To_Std_Logic_Vector (L, L_Arr);
      To_Std_Logic_Vector (R, R_Arr);
      declare
         Res_Arr : constant Std_Logic_Vector := Mul_Sgn_Sgn (L_Arr, R_Arr);
      begin
         return To_Valtyp (Res_Arr, L.Typ.Vec_El);
      end;
   end Synth_Mul_Sgn_Sgn;

   function Synth_Shift (Val : Memtyp;
                         Amt : Uns32;
                         Right : Boolean;
                         Arith : Boolean) return Valtyp
   is
      Len : constant Uns32 := Uns32 (Vec_Length (Val.Typ));
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
      return To_Valtyp (Arr, Val.Typ.Vec_El);
   end Synth_Shift;

   function Get_Static_Ulogic (Op : Memtyp) return Std_Ulogic is
   begin
      pragma Assert (Op.Typ.Kind = Type_Logic);
      return Std_Ulogic'Val (Read_U8 (Op.Mem));
   end Get_Static_Ulogic;

   function Synth_Static_Dyadic_Predefined (Syn_Inst : Synth_Instance_Acc;
                                            Imp : Node;
                                            Left : Memtyp;
                                            Right : Memtyp;
                                            Expr : Node) return Valtyp
   is
      Def : constant Iir_Predefined_Functions :=
        Get_Implicit_Definition (Imp);
      Res_Typ : constant Type_Acc :=
        Get_Subtype_Object (Syn_Inst, Get_Type (Expr));
   begin
      case Def is
         when Iir_Predefined_Error =>
            return No_Valtyp;

         when Iir_Predefined_Boolean_Xor =>
            return Create_Value_Discrete
              (Boolean'Pos (Boolean'Val (Read_Discrete (Left))
                              xor Boolean'Val (Read_Discrete (Right))),
               Res_Typ);

         when Iir_Predefined_Enum_Equality =>
            return Create_Value_Discrete
              (Boolean'Pos (Read_Discrete (Left) = Read_Discrete (Right)),
               Boolean_Type);
         when Iir_Predefined_Enum_Inequality =>
            return Create_Value_Discrete
              (Boolean'Pos (Read_Discrete (Left) /= Read_Discrete (Right)),
               Boolean_Type);
         when Iir_Predefined_Integer_Plus
           | Iir_Predefined_Physical_Plus =>
            return Create_Value_Discrete
              (Read_Discrete (Left) + Read_Discrete (Right), Res_Typ);
         when Iir_Predefined_Integer_Minus
           | Iir_Predefined_Physical_Minus =>
            return Create_Value_Discrete
              (Read_Discrete (Left) - Read_Discrete (Right), Res_Typ);
         when Iir_Predefined_Integer_Mul
           | Iir_Predefined_Physical_Integer_Mul
           | Iir_Predefined_Integer_Physical_Mul =>
            return Create_Value_Discrete
              (Read_Discrete (Left) * Read_Discrete (Right), Res_Typ);
         when Iir_Predefined_Integer_Div
           | Iir_Predefined_Physical_Physical_Div
           | Iir_Predefined_Physical_Integer_Div =>
            return Create_Value_Discrete
              (Read_Discrete (Left) / Read_Discrete (Right), Res_Typ);
         when Iir_Predefined_Integer_Mod =>
            return Create_Value_Discrete
              (Read_Discrete (Left) mod Read_Discrete (Right), Res_Typ);
         when Iir_Predefined_Integer_Rem =>
            return Create_Value_Discrete
              (Read_Discrete (Left) rem Read_Discrete (Right), Res_Typ);
         when Iir_Predefined_Integer_Exp =>
            return Create_Value_Discrete
              (Read_Discrete (Left) ** Natural (Read_Discrete (Right)),
               Res_Typ);
         when Iir_Predefined_Physical_Minimum
           | Iir_Predefined_Integer_Minimum =>
            return Create_Value_Discrete
              (Int64'Min (Read_Discrete (Left), Read_Discrete (Right)),
               Res_Typ);
         when Iir_Predefined_Physical_Maximum
           | Iir_Predefined_Integer_Maximum =>
            return Create_Value_Discrete
              (Int64'Max (Read_Discrete (Left), Read_Discrete (Right)),
               Res_Typ);
         when Iir_Predefined_Integer_Less_Equal
           | Iir_Predefined_Physical_Less_Equal =>
            return Create_Value_Discrete
              (Boolean'Pos (Read_Discrete (Left) <= Read_Discrete (Right)),
               Boolean_Type);
         when Iir_Predefined_Integer_Less
           | Iir_Predefined_Physical_Less =>
            return Create_Value_Discrete
              (Boolean'Pos (Read_Discrete (Left) < Read_Discrete (Right)),
               Boolean_Type);
         when Iir_Predefined_Integer_Greater_Equal
           | Iir_Predefined_Physical_Greater_Equal =>
            return Create_Value_Discrete
              (Boolean'Pos (Read_Discrete (Left) >= Read_Discrete (Right)),
               Boolean_Type);
         when Iir_Predefined_Integer_Greater
           | Iir_Predefined_Physical_Greater =>
            return Create_Value_Discrete
              (Boolean'Pos (Read_Discrete (Left) > Read_Discrete (Right)),
               Boolean_Type);
         when Iir_Predefined_Integer_Equality
           | Iir_Predefined_Physical_Equality =>
            return Create_Value_Discrete
              (Boolean'Pos (Read_Discrete (Left) = Read_Discrete (Right)),
               Boolean_Type);
         when Iir_Predefined_Integer_Inequality
           | Iir_Predefined_Physical_Inequality =>
            return Create_Value_Discrete
              (Boolean'Pos (Read_Discrete (Left) /= Read_Discrete (Right)),
               Boolean_Type);

         when Iir_Predefined_Physical_Real_Mul =>
            return Create_Value_Discrete
              (Int64 (Fp64 (Read_Discrete (Left)) * Read_Fp64 (Right)),
               Res_Typ);
         when Iir_Predefined_Real_Physical_Mul =>
            return Create_Value_Discrete
              (Int64 (Read_Fp64 (Left) * Fp64 (Read_Discrete (Right))),
               Res_Typ);
         when Iir_Predefined_Physical_Real_Div =>
            return Create_Value_Discrete
              (Int64 (Fp64 (Read_Discrete (Left)) / Read_Fp64 (Right)),
               Res_Typ);

         when Iir_Predefined_Floating_Less =>
            return Create_Value_Discrete
              (Boolean'Pos (Read_Fp64 (Left) < Read_Fp64 (Right)),
               Boolean_Type);
         when Iir_Predefined_Floating_Less_Equal =>
            return Create_Value_Discrete
              (Boolean'Pos (Read_Fp64 (Left) <= Read_Fp64 (Right)),
               Boolean_Type);
         when Iir_Predefined_Floating_Equality =>
            return Create_Value_Discrete
              (Boolean'Pos (Read_Fp64 (Left) = Read_Fp64 (Right)),
               Boolean_Type);
         when Iir_Predefined_Floating_Inequality =>
            return Create_Value_Discrete
              (Boolean'Pos (Read_Fp64 (Left) /= Read_Fp64 (Right)),
               Boolean_Type);
         when Iir_Predefined_Floating_Greater =>
            return Create_Value_Discrete
              (Boolean'Pos (Read_Fp64 (Left) > Read_Fp64 (Right)),
               Boolean_Type);
         when Iir_Predefined_Floating_Greater_Equal =>
            return Create_Value_Discrete
              (Boolean'Pos (Read_Fp64 (Left) >= Read_Fp64 (Right)),
               Boolean_Type);

         when Iir_Predefined_Floating_Plus =>
            return Create_Value_Float (Read_Fp64 (Left) + Read_Fp64 (Right),
                                       Res_Typ);
         when Iir_Predefined_Floating_Minus =>
            return Create_Value_Float (Read_Fp64 (Left) - Read_Fp64 (Right),
                                       Res_Typ);
         when Iir_Predefined_Floating_Mul =>
            return Create_Value_Float (Read_Fp64 (Left) * Read_Fp64 (Right),
                                       Res_Typ);
         when Iir_Predefined_Floating_Div =>
            return Create_Value_Float (Read_Fp64 (Left) / Read_Fp64 (Right),
                                       Res_Typ);
         when Iir_Predefined_Floating_Exp =>
            return Create_Value_Float
              (Read_Fp64 (Left) ** Natural (Read_Discrete (Right)), Res_Typ);

         when Iir_Predefined_Array_Array_Concat =>
            declare
               Ret_Typ : constant Type_Acc :=
                 Get_Subtype_Object (Syn_Inst, Get_Return_Type (Imp));
               L_Len : constant Iir_Index32 :=
                 Iir_Index32 (Get_Bound_Length (Left.Typ, 1));
               R_Len : constant Iir_Index32 :=
                 Iir_Index32 (Get_Bound_Length (Right.Typ, 1));
               Bnd : Bound_Type;
               Res_Typ : Type_Acc;
               Res : Valtyp;
            begin
               Bnd := Oper.Create_Bounds_From_Length
                 (Syn_Inst, Get_Index_Type (Get_Type (Expr), 0),
                  L_Len + R_Len);
               Res_Typ := Create_Onedimensional_Array_Subtype
                 (Ret_Typ, Bnd);
               Res := Create_Value_Memory (Res_Typ);
               if Left.Typ.Sz > 0 then
                  Copy_Memory (Res.Val.Mem, Left.Mem, Left.Typ.Sz);
               end if;
               if Right.Typ.Sz > 0 then
                  Copy_Memory (Res.Val.Mem + Left.Typ.Sz,
                               Right.Mem, Right.Typ.Sz);
               end if;
               return Res;
            end;
         when Iir_Predefined_Element_Array_Concat =>
            declare
               Ret_Typ : constant Type_Acc :=
                 Get_Subtype_Object (Syn_Inst, Get_Return_Type (Imp));
               Rlen : constant Iir_Index32 :=
                 Get_Array_Flat_Length (Right.Typ);
               Bnd : Bound_Type;
               Res_Typ : Type_Acc;
               Res : Valtyp;
            begin
               Bnd := Oper.Create_Bounds_From_Length
                 (Syn_Inst, Get_Index_Type (Get_Type (Expr), 0), 1 + Rlen);
               Res_Typ := Create_Onedimensional_Array_Subtype
                 (Ret_Typ, Bnd);
               Res := Create_Value_Memory (Res_Typ);
               Copy_Memory (Res.Val.Mem, Left.Mem, Left.Typ.Sz);
               Copy_Memory (Res.Val.Mem + Left.Typ.Sz,
                            Right.Mem, Right.Typ.Sz);
               return Res;
            end;
         when Iir_Predefined_Array_Element_Concat =>
            declare
               Ret_Typ : constant Type_Acc :=
                 Get_Subtype_Object (Syn_Inst, Get_Return_Type (Imp));
               Llen : constant Iir_Index32 := Get_Array_Flat_Length (Left.Typ);
               Bnd : Bound_Type;
               Res_Typ : Type_Acc;
               Res : Valtyp;
            begin
               Bnd := Oper.Create_Bounds_From_Length
                 (Syn_Inst, Get_Index_Type (Get_Type (Expr), 0), Llen + 1);
               Res_Typ := Create_Onedimensional_Array_Subtype
                 (Ret_Typ, Bnd);
               Res := Create_Value_Memory (Res_Typ);
               Copy_Memory (Res.Val.Mem, Left.Mem, Left.Typ.Sz);
               Copy_Memory (Res.Val.Mem + Left.Typ.Sz,
                            Right.Mem, Right.Typ.Sz);
               return Res;
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
              (Boolean'Pos (Read_Access (Left) = Read_Access (Right)),
               Boolean_Type);
         when Iir_Predefined_Access_Inequality =>
            return Create_Value_Discrete
              (Boolean'Pos (Read_Access (Left) /= Read_Access (Right)),
               Boolean_Type);

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

         when Iir_Predefined_Ieee_1164_Scalar_Or =>
            return Create_Value_Discrete
              (Std_Ulogic'Pos (Or_Table (Get_Static_Ulogic (Left),
                                         Get_Static_Ulogic (Right))),
               Res_Typ);

         when Iir_Predefined_Ieee_1164_Scalar_And =>
            return Create_Value_Discrete
              (Std_Ulogic'Pos (And_Table (Get_Static_Ulogic (Left),
                                          Get_Static_Ulogic (Right))),
               Res_Typ);

         when Iir_Predefined_Ieee_1164_Scalar_Xor =>
            return Create_Value_Discrete
              (Std_Ulogic'Pos (Xor_Table (Get_Static_Ulogic (Left),
                                          Get_Static_Ulogic (Right))),
               Res_Typ);

         when Iir_Predefined_Ieee_Numeric_Std_Eq_Uns_Uns =>
            declare
               Res : Boolean;
            begin
               Res :=
                 Synth_Compare_Uns_Uns (Left, Right, Greater, Expr) = Equal;
               return Create_Value_Discrete (Boolean'Pos (Res), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Uns_Nat =>
            declare
               Res : Boolean;
            begin
               Res :=
                 Synth_Compare_Uns_Nat (Left, Right, Greater, Expr) = Equal;
               return Create_Value_Discrete (Boolean'Pos (Res), Res_Typ);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Gt_Uns_Uns =>
            declare
               Res : Boolean;
            begin
               Res :=
                 Synth_Compare_Uns_Uns (Left, Right, Less, Expr) = Greater;
               return Create_Value_Discrete (Boolean'Pos (Res), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Nat_Uns =>
            declare
               Res : Boolean;
            begin
               Res :=
                 Synth_Compare_Nat_Uns (Left, Right, Less, Expr) = Greater;
               return Create_Value_Discrete (Boolean'Pos (Res), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Uns_Nat =>
            declare
               Res : Boolean;
            begin
               Res :=
                 Synth_Compare_Uns_Nat (Left, Right, Less, Expr) = Greater;
               return Create_Value_Discrete (Boolean'Pos (Res), Res_Typ);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Le_Uns_Uns =>
            declare
               Res : Boolean;
            begin
               Res :=
                 Synth_Compare_Uns_Uns (Left, Right, Greater, Expr) <= Equal;
               return Create_Value_Discrete (Boolean'Pos (Res), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Le_Uns_Nat =>
            declare
               Res : Boolean;
            begin
               Res :=
                 Synth_Compare_Uns_Nat (Left, Right, Greater, Expr) <= Equal;
               return Create_Value_Discrete (Boolean'Pos (Res), Res_Typ);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Lt_Uns_Uns =>
            declare
               Res : Boolean;
            begin
               Res :=
                 Synth_Compare_Uns_Uns (Left, Right, Greater, Expr) < Equal;
               return Create_Value_Discrete (Boolean'Pos (Res), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Uns_Nat =>
            declare
               Res : Boolean;
            begin
               Res :=
                 Synth_Compare_Uns_Nat (Left, Right, Greater, Expr) < Equal;
               return Create_Value_Discrete (Boolean'Pos (Res), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Nat_Uns =>
            declare
               Res : Boolean;
            begin
               Res :=
                 Synth_Compare_Nat_Uns (Left, Right, Greater, Expr) < Equal;
               return Create_Value_Discrete (Boolean'Pos (Res), Res_Typ);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Uns =>
            return Synth_Add_Uns_Uns (Left, Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Int =>
            return Synth_Add_Sgn_Int (Left, Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Nat
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Slv_Int =>
            return Synth_Add_Uns_Nat (Left, Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Uns =>
            return Synth_Sub_Uns_Uns (Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Nat =>
            return Synth_Sub_Uns_Nat (Left, Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Mul_Uns_Uns =>
            return Synth_Mul_Uns_Uns (Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Mul_Nat_Uns =>
            return Synth_Mul_Nat_Uns (Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Mul_Uns_Nat =>
            return Synth_Mul_Uns_Nat (Left, Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Mul_Sgn_Sgn =>
            return Synth_Mul_Sgn_Sgn (Left, Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Srl_Uns_Int =>
            declare
               Amt : Int64;
            begin
               Amt := Read_Discrete (Right);
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
            return No_Valtyp;
      end case;
   end Synth_Static_Dyadic_Predefined;

   function Synth_Vector_Monadic (Vec : Memtyp; Op : Table_1d) return Valtyp
   is
      Len : constant Iir_Index32 := Vec_Length (Vec.Typ);
      Res : Valtyp;
   begin
      Res := Create_Value_Memory (Create_Res_Bound (Vec.Typ));
      for I in 1 .. Uns32 (Len) loop
         declare
            V : constant Std_Ulogic := Read_Std_Logic (Vec.Mem, I - 1);
         begin
            Write_Std_Logic (Res.Val.Mem, I - 1, Op (V));
         end;
      end loop;
      return Res;
   end Synth_Vector_Monadic;

   function Synth_Vector_Reduce
     (Init : Std_Ulogic; Vec : Memtyp; Op : Table_2d) return Valtyp
   is
      El_Typ : constant Type_Acc := Vec.Typ.Vec_El;
      Res : Std_Ulogic;
   begin
      Res := Init;
      for I in 1 .. Uns32 (Vec_Length (Vec.Typ)) loop
         declare
            V : constant Std_Ulogic := Read_Std_Logic (Vec.Mem, I - 1);
         begin
            Res := Op (Res, V);
         end;
      end loop;

      return Create_Value_Discrete (Std_Ulogic'Pos (Res), El_Typ);
   end Synth_Vector_Reduce;

   function Synth_Static_Monadic_Predefined (Syn_Inst : Synth_Instance_Acc;
                                             Imp : Node;
                                             Operand : Memtyp;
                                             Expr : Node) return Valtyp
   is
      Def : constant Iir_Predefined_Functions :=
        Get_Implicit_Definition (Imp);
      Inter_Chain : constant Node :=
        Get_Interface_Declaration_Chain (Imp);
      Oper_Type : constant Node := Get_Type (Inter_Chain);
      Oper_Typ : constant Type_Acc := Get_Subtype_Object (Syn_Inst, Oper_Type);
   begin
      case Def is
         when Iir_Predefined_Boolean_Not
           | Iir_Predefined_Bit_Not =>
            return Create_Value_Discrete
              (1 - Read_Discrete (Operand), Oper_Typ);

         when Iir_Predefined_Integer_Negation
           | Iir_Predefined_Physical_Negation =>
            return Create_Value_Discrete (-Read_Discrete (Operand), Oper_Typ);
         when Iir_Predefined_Integer_Absolute
           | Iir_Predefined_Physical_Absolute =>
            return Create_Value_Discrete
              (abs Read_Discrete(Operand), Oper_Typ);
         when Iir_Predefined_Integer_Identity
           | Iir_Predefined_Physical_Identity =>
            return Create_Value_Memory (Operand);

         when Iir_Predefined_Floating_Negation =>
            return Create_Value_Float (-Read_Fp64 (Operand), Oper_Typ);
         when Iir_Predefined_Floating_Identity =>
            return Create_Value_Memory (Operand);
         when Iir_Predefined_Floating_Absolute =>
            return Create_Value_Float (abs Read_Fp64 (Operand), Oper_Typ);

         when Iir_Predefined_Ieee_1164_Condition_Operator =>
            --  Constant std_logic: need to convert.
            declare
               Val : Uns32;
               Zx : Uns32;
            begin
               From_Std_Logic (Read_Discrete (Operand), Val, Zx);
               return Create_Value_Discrete
                 (Boolean'Pos (Val = 1 and Zx = 0), Boolean_Type);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Neg_Sgn =>
            declare
               Op_Arr : Std_Logic_Vector
                 (1 .. Natural (Vec_Length (Operand.Typ)));
            begin
               To_Std_Logic_Vector (Operand, Op_Arr);
               declare
                  Res_Arr : constant Std_Logic_Vector := Neg_Sgn (Op_Arr);
               begin
                  return To_Valtyp (Res_Arr, Operand.Typ.Vec_El);
               end;
            end;

         when Iir_Predefined_Ieee_1164_Vector_Not
           | Iir_Predefined_Ieee_Numeric_Std_Not_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Not_Sgn =>
            return Synth_Vector_Monadic (Operand, Not_Table);

         when Iir_Predefined_Ieee_1164_Scalar_Not =>
            return Create_Value_Discrete
              (Std_Ulogic'Pos (Not_Table (Read_Std_Logic (Operand.Mem, 0))),
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

   function Eval_To_Vector (Arg : Uns64; Sz : Int64; Res_Type : Type_Acc)
                           return Valtyp
   is
      Len : constant Iir_Index32 := Iir_Index32 (Sz);
      El_Type : constant Type_Acc := Get_Array_Element (Res_Type);
      Res : Valtyp;
      Bnd : Type_Acc;
      B : Uns64;
   begin
      Bnd := Create_Vec_Type_By_Length (Width (Len), El_Type);
      Res := Create_Value_Memory (Bnd);
      for I in 1 .. Len loop
         B := Shift_Right_Arithmetic (Arg, Natural (I - 1)) and 1;
         Write_Std_Logic (Res.Val.Mem, Uns32 (Len - I),
                          Std_Ulogic'Val (Std_Logic_0_Pos + B));
      end loop;
      return Res;
   end Eval_To_Vector;

   function Eval_Unsigned_To_Integer (Arg : Valtyp; Loc : Node) return Int64
   is
      Res : Uns64;
      V : Std_Ulogic;
   begin
      Res := 0;
      for I in 1 .. Vec_Length (Arg.Typ) loop
         V := Std_Ulogic'Val (Read_U8 (Arg.Val.Mem + Size_Type (I - 1)));
         case To_X01 (V) is
            when '0' =>
               Res := Res * 2;
            when '1' =>
               Res := Res * 2 + 1;
            when 'X' =>
               Warning_Msg_Synth
                 (+Loc, "metavalue detected, returning 0");
               Res := 0;
               exit;
         end case;
      end loop;
      return To_Int64 (Res);
   end Eval_Unsigned_To_Integer;

   function Eval_Signed_To_Integer (Arg : Valtyp; Loc : Node) return Int64
   is
      Len : constant Iir_Index32 := Vec_Length (Arg.Typ);
      Res : Uns64;
      E : Std_Ulogic;
   begin
      if Len = 0 then
         Warning_Msg_Synth
           (+Loc, "numeric_std.to_integer: null detected, returning 0");
         return 0;
      end if;

      E := Std_Ulogic'Val (Read_U8 (Arg.Val.Mem));
      case To_X01 (E) is
         when '0' =>
            Res := 0;
         when '1' =>
            Res := not 0;
         when 'X' =>
            Warning_Msg_Synth (+Loc, "metavalue detected, returning 0");
            return 0;
      end case;
      for I in 2 .. Len loop
         E := Std_Ulogic'Val (Read_U8 (Arg.Val.Mem + Size_Type (I - 1)));
         case To_X01 (E) is
            when '0' =>
               Res := Res * 2;
            when '1' =>
               Res := Res * 2 + 1;
            when 'X' =>
               Warning_Msg_Synth (+Loc, "metavalue detected, returning 0");
               return 0;
         end case;
      end loop;
      return To_Int64 (Res);
   end Eval_Signed_To_Integer;

   function Synth_Static_Predefined_Function_Call
     (Subprg_Inst : Synth_Instance_Acc; Expr : Node) return Valtyp
   is
      Imp  : constant Node := Get_Implementation (Expr);
      Def : constant Iir_Predefined_Functions :=
        Get_Implicit_Definition (Imp);
      Inter_Chain : constant Node := Get_Interface_Declaration_Chain (Imp);
      Param1 : Valtyp;
      Param2 : Valtyp;
      Res_Typ : Type_Acc;
      Inter : Node;
   begin
      Inter := Inter_Chain;
      if Inter /= Null_Node then
         Param1 := Get_Value (Subprg_Inst, Inter);
         Strip_Const (Param1);
         Inter := Get_Chain (Inter);
      else
         Param1 := No_Valtyp;
      end if;
      if Inter /= Null_Node then
         Param2 := Get_Value (Subprg_Inst, Inter);
         Strip_Const (Param2);
         Inter := Get_Chain (Inter);
      else
         Param2 := No_Valtyp;
      end if;

      Res_Typ := Get_Subtype_Object (Subprg_Inst, Get_Type (Imp));

      case Def is
         when Iir_Predefined_Endfile =>
            declare
               Res : Boolean;
            begin
               Res := Synth.Files_Operations.Endfile (Param1.Val.File, Expr);
               return Create_Value_Discrete (Boolean'Pos (Res), Boolean_Type);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Touns_Nat_Nat_Uns
           | Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Unsigned_Int =>
            return Eval_To_Vector
              (Uns64 (Read_Discrete (Param1)), Read_Discrete (Param2),
               Res_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Tosgn_Int_Nat_Sgn
           | Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Vector_Int =>
            return Eval_To_Vector
              (To_Uns64 (Read_Discrete (Param1)), Read_Discrete (Param2),
               Res_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Toint_Uns_Nat
           | Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Integer_Uns
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Conv_Integer =>
            --  UNSIGNED to Natural.
            return Create_Value_Discrete
              (Eval_Unsigned_To_Integer (Param1, Expr), Res_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Toint_Sgn_Int =>
            --  SIGNED to Integer
            return Create_Value_Discrete
              (Eval_Signed_To_Integer (Param1, Expr), Res_Typ);

         when Iir_Predefined_Ieee_1164_To_Stdlogicvector_Bv =>
            declare
               use Grt.Types;
               El_Type : constant Type_Acc := Get_Array_Element (Res_Typ);
               Res : Valtyp;
               Bnd : Type_Acc;
               B : Std_Ulogic;
            begin
               Bnd := Create_Vec_Type_By_Length
                 (Uns32 (Vec_Length (Param1.Typ)), El_Type);
               Res := Create_Value_Memory (Bnd);
               for I in 1 .. Vec_Length (Param1.Typ) loop
                  if Read_U8 (Param1.Val.Mem + Size_Type (I - 1)) = 0 then
                     B := '0';
                  else
                     B := '1';
                  end if;
                  Write_Std_Logic (Res.Val.Mem, Uns32 (I - 1), B);
               end loop;
               return Res;
            end;
         when Iir_Predefined_Ieee_Math_Real_Log2 =>
            declare
               function Log2 (Arg : Fp64) return Fp64;
               pragma Import (C, Log2);
            begin
               return Create_Value_Float (Log2 (Read_Fp64 (Param1)), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Math_Real_Ceil =>
            declare
               function Ceil (Arg : Fp64) return Fp64;
               pragma Import (C, Ceil);
            begin
               return Create_Value_Float (Ceil (Read_Fp64 (Param1)), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Math_Real_Round =>
            declare
               function Round (Arg : Fp64) return Fp64;
               pragma Import (C, Round);
            begin
               return Create_Value_Float (Round (Read_Fp64 (Param1)), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Math_Real_Sin =>
            declare
               function Sin (Arg : Fp64) return Fp64;
               pragma Import (C, Sin);
            begin
               return Create_Value_Float (Sin (Read_Fp64 (Param1)), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Math_Real_Cos =>
            declare
               function Cos (Arg : Fp64) return Fp64;
               pragma Import (C, Cos);
            begin
               return Create_Value_Float (Cos (Read_Fp64 (Param1)), Res_Typ);
            end;
         when others =>
            Error_Msg_Synth
              (+Expr, "unhandled (static) function: "
                 & Iir_Predefined_Functions'Image (Def));
            return No_Valtyp;
      end case;
   end Synth_Static_Predefined_Function_Call;

end Synth.Static_Oper;
