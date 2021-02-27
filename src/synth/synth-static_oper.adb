--  Operations synthesis.
--  Copyright (C) 2019 Tristan Gingold
--
--  This file is part of GHDL.
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
with Types_Utils; use Types_Utils;

with Grt.Types; use Grt.Types;

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
with Synth.Values; use Synth.Values;

package body Synth.Static_Oper is
   --  As log2(3m) is directly referenced, the program must be linked with -lm
   --  (math library) on unix systems.
   pragma Linker_Options ("-lm");

   function Create_Res_Bound (Prev : Type_Acc) return Type_Acc is
   begin
      if Prev.Vbound.Dir = Dir_Downto
        and then Prev.Vbound.Right = 0
      then
         --  Normalized range
         return Prev;
      end if;

      return Create_Vec_Type_By_Length (Prev.W, Prev.Vec_El);
   end Create_Res_Bound;

   function Synth_Vector_Dyadic (Left, Right : Memtyp;
                                 Op : Table_2d;
                                 Loc : Syn_Src) return Memtyp
   is
      Res : Memtyp;
   begin
      if Left.Typ.W /= Right.Typ.W then
         Error_Msg_Synth (+Loc, "length of operands mismatch");
         return Null_Memtyp;
      end if;

      Res := Create_Memory (Create_Res_Bound (Left.Typ));
      for I in 1 .. Uns32 (Vec_Length (Res.Typ)) loop
         declare
            Ls : constant Std_Ulogic := Read_Std_Logic (Left.Mem, I - 1);
            Rs : constant Std_Ulogic := Read_Std_Logic (Right.Mem, I - 1);
            V : constant Std_Ulogic := Op (Ls, Rs);
         begin
            Write_Std_Logic (Res.Mem, I - 1, V);
         end;
      end loop;

      return Res;
   end Synth_Vector_Dyadic;

   function Get_Static_Ulogic (Op : Memtyp) return Std_Ulogic is
   begin
      pragma Assert (Op.Typ.Kind = Type_Logic);
      return Std_Ulogic'Val (Read_U8 (Op.Mem));
   end Get_Static_Ulogic;

   procedure Check_Integer_Overflow
     (Val : in out Int64; Typ : Type_Acc; Loc : Syn_Src) is
   begin
      pragma Assert (Typ.Kind = Type_Discrete);
      case Typ.Sz is
         when 4 =>
            if Val < -2**31 or Val >= 2**31 then
               Error_Msg_Synth (+Loc, "integer overflow");
               --  Just keep the lower 32bit (and sign extend).
               Val := Int64
                 (To_Int32 (Uns32 (To_Uns64 (Val) and 16#ffff_ffff#)));
            end if;
         when 8 =>
            null;
         when others =>
            raise Internal_Error;
      end case;
   end Check_Integer_Overflow;

   function Synth_Static_Dyadic_Predefined (Syn_Inst : Synth_Instance_Acc;
                                            Imp : Node;
                                            Left : Memtyp;
                                            Right : Memtyp;
                                            Expr : Node) return Memtyp
   is
      Def : constant Iir_Predefined_Functions :=
        Get_Implicit_Definition (Imp);
      Res_Typ : constant Type_Acc :=
        Get_Subtype_Object (Syn_Inst, Get_Type (Expr));
   begin
      case Def is
         when Iir_Predefined_Error =>
            return Null_Memtyp;

         when Iir_Predefined_Boolean_Xor
            | Iir_Predefined_Bit_Xor =>
            return Create_Memory_U8
              (Boolean'Pos (Boolean'Val (Read_Discrete (Left))
                              xor Boolean'Val (Read_Discrete (Right))),
               Res_Typ);

         when Iir_Predefined_Enum_Equality =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Discrete (Left) = Read_Discrete (Right)),
               Boolean_Type);
         when Iir_Predefined_Enum_Inequality =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Discrete (Left) /= Read_Discrete (Right)),
               Boolean_Type);

         when Iir_Predefined_Integer_Plus
           | Iir_Predefined_Physical_Plus =>
            declare
               Res : Int64;
            begin
               Res := Read_Discrete (Left) + Read_Discrete (Right);
               Check_Integer_Overflow (Res, Res_Typ, Expr);
               return Create_Memory_Discrete (Res, Res_Typ);
            end;
         when Iir_Predefined_Integer_Minus
            | Iir_Predefined_Physical_Minus =>
            declare
               Res : Int64;
            begin
               Res := Read_Discrete (Left) - Read_Discrete (Right);
               Check_Integer_Overflow (Res, Res_Typ, Expr);
               return Create_Memory_Discrete (Res, Res_Typ);
            end;
         when Iir_Predefined_Integer_Mul
           | Iir_Predefined_Physical_Integer_Mul
           | Iir_Predefined_Integer_Physical_Mul =>
            declare
               Res : Int64;
            begin
               Res := Read_Discrete (Left) * Read_Discrete (Right);
               Check_Integer_Overflow (Res, Res_Typ, Expr);
               return Create_Memory_Discrete (Res, Res_Typ);
            end;
         when Iir_Predefined_Integer_Div
           | Iir_Predefined_Physical_Physical_Div
           | Iir_Predefined_Physical_Integer_Div =>
            declare
               Res : Int64;
            begin
               Res := Read_Discrete (Left) / Read_Discrete (Right);
               Check_Integer_Overflow (Res, Res_Typ, Expr);
               return Create_Memory_Discrete (Res, Res_Typ);
            end;
         when Iir_Predefined_Integer_Mod =>
            declare
               Res : Int64;
            begin
               Res := Read_Discrete (Left) mod Read_Discrete (Right);
               Check_Integer_Overflow (Res, Res_Typ, Expr);
               return Create_Memory_Discrete (Res, Res_Typ);
            end;
         when Iir_Predefined_Integer_Rem =>
            declare
               Res : Int64;
            begin
               Res := Read_Discrete (Left) rem Read_Discrete (Right);
               Check_Integer_Overflow (Res, Res_Typ, Expr);
               return Create_Memory_Discrete (Res, Res_Typ);
            end;

         when Iir_Predefined_Integer_Exp =>
            return Create_Memory_Discrete
              (Read_Discrete (Left) ** Natural (Read_Discrete (Right)),
               Res_Typ);

         when Iir_Predefined_Physical_Minimum
           | Iir_Predefined_Integer_Minimum =>
            return Create_Memory_Discrete
              (Int64'Min (Read_Discrete (Left), Read_Discrete (Right)),
               Res_Typ);
         when Iir_Predefined_Physical_Maximum
           | Iir_Predefined_Integer_Maximum =>
            return Create_Memory_Discrete
              (Int64'Max (Read_Discrete (Left), Read_Discrete (Right)),
               Res_Typ);

         when Iir_Predefined_Integer_Less_Equal
           | Iir_Predefined_Physical_Less_Equal =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Discrete (Left) <= Read_Discrete (Right)),
               Boolean_Type);
         when Iir_Predefined_Integer_Less
           | Iir_Predefined_Physical_Less =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Discrete (Left) < Read_Discrete (Right)),
               Boolean_Type);
         when Iir_Predefined_Integer_Greater_Equal
           | Iir_Predefined_Physical_Greater_Equal =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Discrete (Left) >= Read_Discrete (Right)),
               Boolean_Type);
         when Iir_Predefined_Integer_Greater
           | Iir_Predefined_Physical_Greater =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Discrete (Left) > Read_Discrete (Right)),
               Boolean_Type);
         when Iir_Predefined_Integer_Equality
           | Iir_Predefined_Physical_Equality =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Discrete (Left) = Read_Discrete (Right)),
               Boolean_Type);
         when Iir_Predefined_Integer_Inequality
           | Iir_Predefined_Physical_Inequality =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Discrete (Left) /= Read_Discrete (Right)),
               Boolean_Type);

         when Iir_Predefined_Physical_Real_Mul =>
            return Create_Memory_Discrete
              (Int64 (Fp64 (Read_Discrete (Left)) * Read_Fp64 (Right)),
               Res_Typ);
         when Iir_Predefined_Real_Physical_Mul =>
            return Create_Memory_Discrete
              (Int64 (Read_Fp64 (Left) * Fp64 (Read_Discrete (Right))),
               Res_Typ);
         when Iir_Predefined_Physical_Real_Div =>
            return Create_Memory_Discrete
              (Int64 (Fp64 (Read_Discrete (Left)) / Read_Fp64 (Right)),
               Res_Typ);

         when Iir_Predefined_Floating_Less =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Fp64 (Left) < Read_Fp64 (Right)),
               Boolean_Type);
         when Iir_Predefined_Floating_Less_Equal =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Fp64 (Left) <= Read_Fp64 (Right)),
               Boolean_Type);
         when Iir_Predefined_Floating_Equality =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Fp64 (Left) = Read_Fp64 (Right)),
               Boolean_Type);
         when Iir_Predefined_Floating_Inequality =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Fp64 (Left) /= Read_Fp64 (Right)),
               Boolean_Type);
         when Iir_Predefined_Floating_Greater =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Fp64 (Left) > Read_Fp64 (Right)),
               Boolean_Type);
         when Iir_Predefined_Floating_Greater_Equal =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Fp64 (Left) >= Read_Fp64 (Right)),
               Boolean_Type);

         when Iir_Predefined_Floating_Plus =>
            return Create_Memory_Fp64 (Read_Fp64 (Left) + Read_Fp64 (Right),
                                       Res_Typ);
         when Iir_Predefined_Floating_Minus =>
            return Create_Memory_Fp64 (Read_Fp64 (Left) - Read_Fp64 (Right),
                                       Res_Typ);
         when Iir_Predefined_Floating_Mul =>
            return Create_Memory_Fp64 (Read_Fp64 (Left) * Read_Fp64 (Right),
                                       Res_Typ);
         when Iir_Predefined_Floating_Div =>
            return Create_Memory_Fp64 (Read_Fp64 (Left) / Read_Fp64 (Right),
                                       Res_Typ);
         when Iir_Predefined_Floating_Exp =>
            return Create_Memory_Fp64
              (Read_Fp64 (Left) ** Integer (Read_Discrete (Right)), Res_Typ);

         when Iir_Predefined_Array_Array_Concat =>
            declare
               L_Len : constant Iir_Index32 :=
                 Iir_Index32 (Get_Bound_Length (Left.Typ, 1));
               R_Len : constant Iir_Index32 :=
                 Iir_Index32 (Get_Bound_Length (Right.Typ, 1));
               Bnd : Bound_Type;
               Res_St : Type_Acc;
               Res : Memtyp;
            begin
               Bnd := Oper.Create_Bounds_From_Length
                 (Syn_Inst, Get_Index_Type (Get_Type (Expr), 0),
                  L_Len + R_Len);
               Res_St := Create_Onedimensional_Array_Subtype (Res_Typ, Bnd);
               Res := Create_Memory (Res_St);
               if Left.Typ.Sz > 0 then
                  Copy_Memory (Res.Mem, Left.Mem, Left.Typ.Sz);
               end if;
               if Right.Typ.Sz > 0 then
                  Copy_Memory (Res.Mem + Left.Typ.Sz, Right.Mem, Right.Typ.Sz);
               end if;
               return Res;
            end;
         when Iir_Predefined_Element_Array_Concat =>
            declare
               Rlen : constant Iir_Index32 :=
                 Get_Array_Flat_Length (Right.Typ);
               Bnd : Bound_Type;
               Res_St : Type_Acc;
               Res : Memtyp;
            begin
               Bnd := Oper.Create_Bounds_From_Length
                 (Syn_Inst, Get_Index_Type (Get_Type (Expr), 0), 1 + Rlen);
               Res_St := Create_Onedimensional_Array_Subtype (Res_Typ, Bnd);
               Res := Create_Memory (Res_St);
               Copy_Memory (Res.Mem, Left.Mem, Left.Typ.Sz);
               Copy_Memory (Res.Mem + Left.Typ.Sz,
                            Right.Mem, Right.Typ.Sz);
               return Res;
            end;
         when Iir_Predefined_Array_Element_Concat =>
            declare
               Llen : constant Iir_Index32 := Get_Array_Flat_Length (Left.Typ);
               Bnd : Bound_Type;
               Res_St : Type_Acc;
               Res : Memtyp;
            begin
               Bnd := Oper.Create_Bounds_From_Length
                 (Syn_Inst, Get_Index_Type (Get_Type (Expr), 0), Llen + 1);
               Res_St := Create_Onedimensional_Array_Subtype (Res_Typ, Bnd);
               Res := Create_Memory (Res_St);
               Copy_Memory (Res.Mem, Left.Mem, Left.Typ.Sz);
               Copy_Memory (Res.Mem + Left.Typ.Sz,
                            Right.Mem, Right.Typ.Sz);
               return Res;
            end;

         when Iir_Predefined_Array_Equality
           | Iir_Predefined_Record_Equality =>
            return Create_Memory_U8
              (Boolean'Pos (Is_Equal (Left, Right)), Boolean_Type);
         when Iir_Predefined_Array_Inequality
            | Iir_Predefined_Record_Inequality =>
            return Create_Memory_U8
              (Boolean'Pos (not Is_Equal (Left, Right)), Boolean_Type);

         when Iir_Predefined_Access_Equality =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Access (Left) = Read_Access (Right)),
               Boolean_Type);
         when Iir_Predefined_Access_Inequality =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Access (Left) /= Read_Access (Right)),
               Boolean_Type);

         when Iir_Predefined_TF_Array_Xor =>
            if Left.Typ.Sz /= Right.Typ.Sz then
               Error_Msg_Synth (+Expr, "length mismatch");
               return Left;
            else
               declare
                  Res : Memtyp;
                  L, R : Boolean;
               begin
                  Res := Create_Memory (Left.Typ);
                  for I in 1 .. Left.Typ.Sz loop
                     L := Boolean'Val (Read_U8 (Left.Mem + (I - 1)));
                     R := Boolean'Val (Read_U8 (Right.Mem + (I - 1)));
                     Write_U8 (Res.Mem + (I - 1), Boolean'Pos (L xor R));
                  end loop;
                  return Res;
               end;
            end if;

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
            return Create_Memory_U8
              (Std_Ulogic'Pos (Or_Table (Get_Static_Ulogic (Left),
                                         Get_Static_Ulogic (Right))),
               Res_Typ);

         when Iir_Predefined_Ieee_1164_Scalar_And =>
            return Create_Memory_U8
              (Std_Ulogic'Pos (And_Table (Get_Static_Ulogic (Left),
                                          Get_Static_Ulogic (Right))),
               Res_Typ);

         when Iir_Predefined_Ieee_1164_Scalar_Xor =>
            return Create_Memory_U8
              (Std_Ulogic'Pos (Xor_Table (Get_Static_Ulogic (Left),
                                          Get_Static_Ulogic (Right))),
               Res_Typ);

         when Iir_Predefined_Ieee_Numeric_Std_Eq_Uns_Uns =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Uns_Uns (Left, Right, Greater, Expr) = Equal;
               return Create_Memory_U8 (Boolean'Pos (Res), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Sgn_Sgn =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Sgn_Sgn (Left, Right, Greater, Expr) = Equal;
               return Create_Memory_U8 (Boolean'Pos (Res), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Uns_Nat =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Uns_Nat (Left, Right, Greater, Expr) = Equal;
               return Create_Memory_U8 (Boolean'Pos (Res), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Sgn_Int =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Sgn_Int (Left, Right, Greater, Expr) = Equal;
               return Create_Memory_U8 (Boolean'Pos (Res), Res_Typ);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Gt_Uns_Uns =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Uns_Uns (Left, Right, Less, Expr) = Greater;
               return Create_Memory_U8 (Boolean'Pos (Res), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Sgn_Sgn =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Sgn_Sgn (Left, Right, Less, Expr) = Greater;
               return Create_Memory_U8 (Boolean'Pos (Res), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Nat_Uns =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Nat_Uns (Left, Right, Less, Expr) = Greater;
               return Create_Memory_U8 (Boolean'Pos (Res), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Uns_Nat =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Uns_Nat (Left, Right, Less, Expr) = Greater;
               return Create_Memory_U8 (Boolean'Pos (Res), Res_Typ);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Ge_Uns_Uns =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Uns_Uns (Left, Right, Greater, Expr) >= Equal;
               return Create_Memory_U8 (Boolean'Pos (Res), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Ge_Sgn_Sgn =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Sgn_Sgn (Left, Right, Less, Expr) >= Equal;
               return Create_Memory_U8 (Boolean'Pos (Res), Res_Typ);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Le_Uns_Uns =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Uns_Uns (Left, Right, Greater, Expr) <= Equal;
               return Create_Memory_U8 (Boolean'Pos (Res), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Le_Uns_Nat =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Uns_Nat (Left, Right, Greater, Expr) <= Equal;
               return Create_Memory_U8 (Boolean'Pos (Res), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Le_Sgn_Sgn =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Sgn_Sgn (Left, Right, Less, Expr) <= Equal;
               return Create_Memory_U8 (Boolean'Pos (Res), Res_Typ);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Lt_Uns_Uns =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Uns_Uns (Left, Right, Greater, Expr) < Equal;
               return Create_Memory_U8 (Boolean'Pos (Res), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Uns_Nat =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Uns_Nat (Left, Right, Greater, Expr) < Equal;
               return Create_Memory_U8 (Boolean'Pos (Res), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Nat_Uns =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Nat_Uns (Left, Right, Greater, Expr) < Equal;
               return Create_Memory_U8 (Boolean'Pos (Res), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Sgn_Sgn =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Sgn_Sgn (Left, Right, Less, Expr) < Equal;
               return Create_Memory_U8 (Boolean'Pos (Res), Res_Typ);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Log
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Slv_Log
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Slv_Slv
           | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Uns_Slv =>
            return Add_Uns_Uns (Left, Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Int =>
            return Add_Sgn_Int (Left, Read_Discrete (Right), Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Nat
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Slv_Int =>
            return Add_Uns_Nat (Left, To_Uns64 (Read_Discrete (Right)), Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Sgn =>
            return Add_Sgn_Sgn (Left, Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Uns =>
            return Sub_Uns_Uns (Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Nat =>
            return Sub_Uns_Nat (Left, To_Uns64 (Read_Discrete (Right)), Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Sub_Sgn_Int =>
            return Sub_Sgn_Int (Left, Read_Discrete (Right), Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Sgn_Sgn =>
            return Sub_Sgn_Sgn (Left, Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Mul_Uns_Uns =>
            return Mul_Uns_Uns (Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Mul_Nat_Uns =>
            return Mul_Nat_Uns (To_Uns64 (Read_Discrete (Left)), Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Mul_Uns_Nat =>
            return Mul_Uns_Nat (Left, To_Uns64 (Read_Discrete (Right)), Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Mul_Sgn_Sgn =>
            return Mul_Sgn_Sgn (Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Mul_Sgn_Int =>
            return Mul_Sgn_Int (Left, Read_Discrete (Right), Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Mul_Int_Sgn =>
            return Mul_Int_Sgn (Read_Discrete (Left), Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Div_Uns_Uns =>
            return Div_Uns_Uns (Left, Right, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Div_Sgn_Sgn =>
            return Div_Sgn_Sgn (Left, Right, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Srl_Uns_Int
           |  Iir_Predefined_Ieee_Numeric_Std_Srl_Sgn_Int =>
            declare
               Amt : Int64;
            begin
               Amt := Read_Discrete (Right);
               if Amt >= 0 then
                  return Shift_Vec (Left, Uns32 (Amt), True, False);
               else
                  return Shift_Vec (Left, Uns32 (-Amt), False, False);
               end if;
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Sll_Uns_Int
           |  Iir_Predefined_Ieee_Numeric_Std_Sll_Sgn_Int =>
            declare
               Amt : Int64;
            begin
               Amt := Read_Discrete (Right);
               if Amt >= 0 then
                  return Shift_Vec (Left, Uns32 (Amt), False, False);
               else
                  return Shift_Vec (Left, Uns32 (-Amt), True, False);
               end if;
            end;

         when Iir_Predefined_Ieee_Math_Real_Pow =>
            declare
               function Pow (L, R : Fp64) return Fp64;
               pragma Import (C, Pow);
            begin
               return Create_Memory_Fp64
                 (Pow (Read_Fp64 (Left), Read_Fp64 (Right)), Res_Typ);
            end;

         when others =>
            Error_Msg_Synth
              (+Expr, "synth_static_dyadic_predefined: unhandled "
                 & Iir_Predefined_Functions'Image (Def));
            return Null_Memtyp;
      end case;
   end Synth_Static_Dyadic_Predefined;

   function Synth_Vector_Monadic (Vec : Memtyp; Op : Table_1d) return Memtyp
   is
      Len : constant Iir_Index32 := Vec_Length (Vec.Typ);
      Res : Memtyp;
   begin
      Res := Create_Memory (Create_Res_Bound (Vec.Typ));
      for I in 1 .. Uns32 (Len) loop
         declare
            V : constant Std_Ulogic := Read_Std_Logic (Vec.Mem, I - 1);
         begin
            Write_Std_Logic (Res.Mem, I - 1, Op (V));
         end;
      end loop;
      return Res;
   end Synth_Vector_Monadic;

   function Synth_Vector_Reduce
     (Init : Std_Ulogic; Vec : Memtyp; Op : Table_2d) return Memtyp
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

      return Create_Memory_U8 (Std_Ulogic'Pos (Res), El_Typ);
   end Synth_Vector_Reduce;

   function Synth_Static_Monadic_Predefined (Syn_Inst : Synth_Instance_Acc;
                                             Imp : Node;
                                             Operand : Memtyp;
                                             Expr : Node) return Memtyp
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
            return Create_Memory_U8 (1 - Read_U8 (Operand), Oper_Typ);

         when Iir_Predefined_Integer_Negation
           | Iir_Predefined_Physical_Negation =>
            return Create_Memory_Discrete (-Read_Discrete (Operand), Oper_Typ);
         when Iir_Predefined_Integer_Absolute
           | Iir_Predefined_Physical_Absolute =>
            return Create_Memory_Discrete
              (abs Read_Discrete(Operand), Oper_Typ);
         when Iir_Predefined_Integer_Identity
           | Iir_Predefined_Physical_Identity =>
            return Operand;

         when Iir_Predefined_Floating_Negation =>
            return Create_Memory_Fp64 (-Read_Fp64 (Operand), Oper_Typ);
         when Iir_Predefined_Floating_Identity =>
            return Operand;
         when Iir_Predefined_Floating_Absolute =>
            return Create_Memory_Fp64 (abs Read_Fp64 (Operand), Oper_Typ);

         when Iir_Predefined_Ieee_1164_Condition_Operator =>
            --  Constant std_logic: need to convert.
            declare
               Val : Uns32;
               Zx : Uns32;
            begin
               From_Std_Logic (Int64 (Read_U8 (Operand)), Val, Zx);
               return Create_Memory_U8
                 (Boolean'Pos (Val = 1 and Zx = 0), Boolean_Type);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Neg_Sgn =>
            return Neg_Vec (Operand, Expr);

         when Iir_Predefined_Ieee_1164_Vector_Not
           | Iir_Predefined_Ieee_Numeric_Std_Not_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Not_Sgn =>
            return Synth_Vector_Monadic (Operand, Not_Table);

         when Iir_Predefined_Ieee_1164_Scalar_Not =>
            return Create_Memory_U8
              (Std_Ulogic'Pos (Not_Table (Read_Std_Logic (Operand.Mem, 0))),
               Oper_Typ);

         when Iir_Predefined_Ieee_1164_Or_Suv =>
            return Synth_Vector_Reduce ('0', Operand, Or_Table);
         when Iir_Predefined_Ieee_1164_Xor_Suv =>
            return Synth_Vector_Reduce ('0', Operand, Xor_Table);

         when others =>
            Error_Msg_Synth
              (+Expr, "synth_static_monadic_predefined: unhandled "
                 & Iir_Predefined_Functions'Image (Def));
            raise Internal_Error;
      end case;
   end Synth_Static_Monadic_Predefined;

   function Eval_To_Vector (Arg : Uns64; Sz : Int64; Res_Type : Type_Acc)
                           return Memtyp
   is
      Len : constant Iir_Index32 := Iir_Index32 (Sz);
      El_Type : constant Type_Acc := Get_Array_Element (Res_Type);
      Res : Memtyp;
      Bnd : Type_Acc;
      B : Uns64;
   begin
      Bnd := Create_Vec_Type_By_Length (Width (Len), El_Type);
      Res := Create_Memory (Bnd);
      for I in 1 .. Len loop
         B := Shift_Right_Arithmetic (Arg, Natural (I - 1)) and 1;
         Write_Std_Logic (Res.Mem, Uns32 (Len - I),
                          Std_Ulogic'Val (Std_Logic_0_Pos + B));
      end loop;
      return Res;
   end Eval_To_Vector;

   function Eval_Unsigned_To_Integer (Arg : Memtyp; Loc : Node) return Int64
   is
      Res : Uns64;
      V : Std_Ulogic;
   begin
      Res := 0;
      for I in 1 .. Vec_Length (Arg.Typ) loop
         V := Std_Ulogic'Val (Read_U8 (Arg.Mem + Size_Type (I - 1)));
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

   function Eval_Signed_To_Integer (Arg : Memtyp; Loc : Node) return Int64
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

      E := Std_Ulogic'Val (Read_U8 (Arg.Mem));
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
         E := Std_Ulogic'Val (Read_U8 (Arg.Mem + Size_Type (I - 1)));
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
     (Subprg_Inst : Synth_Instance_Acc; Expr : Node) return Memtyp
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
               return Create_Memory_U8 (Boolean'Pos (Res), Boolean_Type);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Touns_Nat_Nat_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Unsigned_Int
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_To_Slv_Nat_Nat_Slv =>
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
            return Create_Memory_Discrete
              (Eval_Unsigned_To_Integer (Get_Memtyp (Param1), Expr), Res_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Toint_Sgn_Int =>
            --  SIGNED to Integer
            return Create_Memory_Discrete
              (Eval_Signed_To_Integer (Get_Memtyp (Param1), Expr), Res_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Integer_Int =>
            return Get_Memtyp (Param1);

         when Iir_Predefined_Ieee_Numeric_Std_Shf_Left_Uns_Nat
            | Iir_Predefined_Ieee_Numeric_Std_Shf_Left_Sgn_Nat =>
            return Shift_Vec
              (Get_Memtyp (Param1), Uns32 (Read_Discrete (Param2)),
               False, False);
         when Iir_Predefined_Ieee_Numeric_Std_Shf_Right_Uns_Nat =>
            return Shift_Vec
              (Get_Memtyp (Param1), Uns32 (Read_Discrete (Param2)),
               True, False);
         when Iir_Predefined_Ieee_Numeric_Std_Shf_Right_Sgn_Nat =>
            return Shift_Vec
              (Get_Memtyp (Param1), Uns32 (Read_Discrete (Param2)),
               True, True);
         when Iir_Predefined_Ieee_Numeric_Std_Resize_Sgn_Nat =>
            return Resize_Vec
              (Get_Memtyp (Param1), Uns32 (Read_Discrete (Param2)), True);
         when Iir_Predefined_Ieee_Numeric_Std_Resize_Uns_Nat =>
            return Resize_Vec
              (Get_Memtyp (Param1), Uns32 (Read_Discrete (Param2)), False);

         when Iir_Predefined_Ieee_1164_To_Stdulogic =>
            declare
               B : Std_Ulogic;
            begin
               B := Read_Bit_To_Std_Logic (Param1.Val.Mem, 0);
               return Create_Memory_U8 (Std_Ulogic'Pos (B), Res_Typ);
            end;

         when Iir_Predefined_Ieee_1164_To_X01_Log =>
            declare
               B : Std_Ulogic;
            begin
               B := Read_Std_Logic (Param1.Val.Mem, 0);
               B := To_X01 (B);
               return Create_Memory_U8 (Std_Ulogic'Pos (B), Res_Typ);
            end;
         when Iir_Predefined_Ieee_1164_To_X01_Slv =>
            declare
               El_Type : constant Type_Acc := Get_Array_Element (Res_Typ);
               Res : Memtyp;
               Bnd : Type_Acc;
               B : Std_Ulogic;
            begin
               Bnd := Create_Vec_Type_By_Length
                 (Uns32 (Vec_Length (Param1.Typ)), El_Type);
               Res := Create_Memory (Bnd);
               for I in 1 .. Uns32 (Vec_Length (Param1.Typ)) loop
                  B := Read_Std_Logic (Param1.Val.Mem, I - 1);
                  B := To_X01 (B);
                  Write_Std_Logic (Res.Mem, I - 1, B);
               end loop;
               return Res;
            end;

         when Iir_Predefined_Ieee_1164_To_Stdlogicvector_Bv
            | Iir_Predefined_Ieee_1164_To_Stdulogicvector_Bv =>
            declare
               El_Type : constant Type_Acc := Get_Array_Element (Res_Typ);
               Res : Memtyp;
               Bnd : Type_Acc;
               B : Std_Ulogic;
            begin
               Bnd := Create_Vec_Type_By_Length
                 (Uns32 (Vec_Length (Param1.Typ)), El_Type);
               Res := Create_Memory (Bnd);
               for I in 1 .. Uns32 (Vec_Length (Param1.Typ)) loop
                  B := Read_Bit_To_Std_Logic (Param1.Val.Mem, I - 1);
                  Write_Std_Logic (Res.Mem, I - 1, B);
               end loop;
               return Res;
            end;

         when Iir_Predefined_Ieee_1164_To_Bit =>
            declare
               V : Std_Ulogic;
               X : Bit;
               R : Bit;
            begin
               V := Read_Std_Logic (Param1.Val.Mem, 0);
               X := Read_Bit (Param2.Val.Mem, 0);
               R := To_Bit (V, X);
               return Create_Memory_U8 (Bit'Pos(R), Res_Typ);
            end;
         when Iir_Predefined_Ieee_1164_To_Bitvector =>
            declare
               El_Type : constant Type_Acc := Get_Array_Element (Res_Typ);
               Res     : Memtyp;
               Bnd     : Type_Acc;
               S       : Std_Ulogic;
               X       : Bit;
               R       : Bit;
            begin
               X := Read_Bit (Param2.Val.Mem, 0);
               Bnd := Create_Vec_Type_By_Length
                 (Uns32 (Vec_Length (Param1.Typ)), El_Type);
               Res := Create_Memory (Bnd);
               for I in 1 .. Uns32 (Vec_Length (Param1.Typ)) loop
                  S := Read_Std_Logic (Param1.Val.Mem, I - 1);
                  R := To_Bit (S, X);
                  Write_Bit (Res.Mem, I - 1, R);
               end loop;
               return Res;
            end;

         when Iir_Predefined_Ieee_Math_Real_Log2 =>
            declare
               function Log2 (Arg : Fp64) return Fp64;
               pragma Import (C, Log2);
            begin
               return Create_Memory_Fp64 (Log2 (Read_Fp64 (Param1)), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Math_Real_Ceil =>
            declare
               function Ceil (Arg : Fp64) return Fp64;
               pragma Import (C, Ceil);
            begin
               return Create_Memory_Fp64 (Ceil (Read_Fp64 (Param1)), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Math_Real_Floor =>
            declare
               function Floor (Arg : Fp64) return Fp64;
               pragma Import (C, Floor);
            begin
               return Create_Memory_Fp64 (Floor (Read_Fp64 (Param1)), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Math_Real_Round =>
            declare
               function Round (Arg : Fp64) return Fp64;
               pragma Import (C, Round);
            begin
               return Create_Memory_Fp64 (Round (Read_Fp64 (Param1)), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Math_Real_Sin =>
            declare
               function Sin (Arg : Fp64) return Fp64;
               pragma Import (C, Sin);
            begin
               return Create_Memory_Fp64 (Sin (Read_Fp64 (Param1)), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Math_Real_Cos =>
            declare
               function Cos (Arg : Fp64) return Fp64;
               pragma Import (C, Cos);
            begin
               return Create_Memory_Fp64 (Cos (Read_Fp64 (Param1)), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Math_Real_Arctan =>
            declare
               function Atan (Arg : Fp64) return Fp64;
               pragma Import (C, Atan);
            begin
               return Create_Memory_Fp64 (Atan (Read_Fp64 (Param1)), Res_Typ);
            end;
         when others =>
            Error_Msg_Synth
              (+Expr, "unhandled (static) function: "
                 & Iir_Predefined_Functions'Image (Def));
            return Null_Memtyp;
      end case;
   end Synth_Static_Predefined_Function_Call;

end Synth.Static_Oper;
