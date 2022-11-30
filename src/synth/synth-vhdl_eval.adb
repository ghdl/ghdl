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
with Flags;
with Name_Table;

with Grt.Types; use Grt.Types;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.To_Strings;
with Grt.Arith;
with Grt.Fcvt;

with Vhdl.Utils;
with Vhdl.Evaluation;
with Vhdl.Ieee.Std_Logic_1164; use Vhdl.Ieee.Std_Logic_1164;

with Elab.Memtype; use Elab.Memtype;
with Elab.Vhdl_Expr; use Elab.Vhdl_Expr;
with Elab.Vhdl_Types;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;

with Netlists; use Netlists;

with Synth.Errors; use Synth.Errors;
with Synth.Source; use Synth.Source;
with Synth.Vhdl_Expr; use Synth.Vhdl_Expr;
with Synth.Ieee.Std_Logic_1164; use Synth.Ieee.Std_Logic_1164;
with Synth.Ieee.Numeric_Std; use Synth.Ieee.Numeric_Std;
with Synth.Ieee.Std_Logic_Arith; use Synth.Ieee.Std_Logic_Arith;

package body Synth.Vhdl_Eval is
   --  As log2(3m) is directly referenced, the program must be linked with -lm
   --  (math library) on unix systems.
   pragma Linker_Options ("-lm");

   type Tf_Table_2d is array (Boolean, Boolean) of Boolean;

   Tf_2d_And : constant Tf_Table_2d :=
     (False => (others => False),
      True => (True => True, False => False));

   Tf_2d_Nand : constant Tf_Table_2d :=
     (False => (others => True),
      True => (True => False, False => True));

   Tf_2d_Or : constant Tf_Table_2d :=
     (False => (True => True, False => False),
      True => (True => True, False => True));

   Tf_2d_Nor : constant Tf_Table_2d :=
     (False => (True => False, False => True),
      True => (True => False, False => False));

   Tf_2d_Xor : constant Tf_Table_2d :=
     (False => (False => False, True => True),
      True  => (False => True,  True => False));

   Tf_2d_Xnor : constant Tf_Table_2d :=
     (False => (False => True, True => False),
      True  => (False => False,  True => True));

   function Create_Res_Bound (Prev : Type_Acc) return Type_Acc is
   begin
      if Prev.Abound.Dir = Dir_Downto
        and then Prev.Abound.Right = 0
      then
         --  Normalized range
         return Prev;
      end if;

      return Create_Vec_Type_By_Length (Prev.W, Prev.Arr_El);
   end Create_Res_Bound;

   function Eval_Vector_Dyadic (Inst : Synth_Instance_Acc;
                                Left, Right : Memtyp;
                                Op : Table_2d;
                                Loc : Syn_Src) return Memtyp
   is
      Res : Memtyp;
   begin
      if Left.Typ.W /= Right.Typ.W then
         Error_Msg_Synth (Inst, Loc, "length of operands mismatch");
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
   end Eval_Vector_Dyadic;

   function Eval_Logic_Vector_Scalar (Vect, Scal : Memtyp;
                                      Op : Table_2d) return Memtyp
   is
      Res : Memtyp;
      Vs, Vv, Vr : Std_Ulogic;
   begin
      Res := Create_Memory (Create_Res_Bound (Vect.Typ));
      Vs := Read_Std_Logic (Scal.Mem, 0);
      for I in 1 .. Vect.Typ.Abound.Len loop
         Vv := Read_Std_Logic (Vect.Mem, I - 1);
         Vr := Op (Vs, Vv);
         Write_Std_Logic (Res.Mem, I - 1, Vr);
      end loop;
      return Res;
   end Eval_Logic_Vector_Scalar;

   function Eval_Logic_Scalar (Left, Right : Memtyp;
                               Op : Table_2d;
                               Neg : Boolean := False) return Memtyp
   is
      Res : Std_Ulogic;
   begin
      Res := Op (Read_Std_Logic (Left.Mem, 0), Read_Std_Logic (Right.Mem, 0));
      if Neg then
         Res := Not_Table (Res);
      end if;
      return Create_Memory_U8 (Std_Ulogic'Pos (Res), Left.Typ);
   end Eval_Logic_Scalar;

   function Eval_Vector_Match (Inst : Synth_Instance_Acc;
                               Left, Right : Memtyp;
                               Neg : Boolean;
                               Loc : Syn_Src) return Memtyp
   is
      Res : Std_Ulogic;
   begin
      if Left.Typ.W /= Right.Typ.W then
         Error_Msg_Synth (Inst, Loc, "length of operands mismatch");
         return Null_Memtyp;
      end if;

      Res := '1';
      for I in 1 .. Left.Typ.Abound.Len loop
         declare
            Ls : constant Std_Ulogic := Read_Std_Logic (Left.Mem, I - 1);
            Rs : constant Std_Ulogic := Read_Std_Logic (Right.Mem, I - 1);
         begin
            Res := And_Table (Res, Match_Eq_Table (Ls, Rs));
         end;
      end loop;

      if Neg then
         Res := Not_Table (Res);
      end if;
      return Create_Memory_U8 (Std_Ulogic'Pos (Res), Left.Typ.Arr_El);
   end Eval_Vector_Match;

   function Eval_TF_Vector_Dyadic (Inst : Synth_Instance_Acc;
                                   Left, Right : Memtyp;
                                   Op : Tf_Table_2d;
                                   Loc : Syn_Src) return Memtyp
   is
      Res : Memtyp;
      L, R : Boolean;
   begin
      if Left.Typ.Sz /= Right.Typ.Sz then
         Error_Msg_Synth (Inst, Loc, "length mismatch");
         return Null_Memtyp;
      end if;

      Res := Create_Memory (Left.Typ);
      for I in 1 .. Left.Typ.Sz loop
         L := Boolean'Val (Read_U8 (Left.Mem + (I - 1)));
         R := Boolean'Val (Read_U8 (Right.Mem + (I - 1)));
         Write_U8 (Res.Mem + (I - 1), Boolean'Pos (Op (L, R)));
      end loop;
      return Res;
   end Eval_TF_Vector_Dyadic;

   function Eval_TF_Array_Element (El, Arr : Memtyp;
                                   Op : Tf_Table_2d) return Memtyp
   is
      Res : Memtyp;
      Ve, Va : Boolean;
   begin
      Res := Create_Memory (Arr.Typ);
      Ve := Boolean'Val (Read_U8 (El.Mem));
      for I in 1 .. Arr.Typ.Sz loop
         Va := Boolean'Val (Read_U8 (Arr.Mem + (I - 1)));
         Write_U8 (Res.Mem + (I - 1), Boolean'Pos (Op (Ve, Va)));
      end loop;
      return Res;
   end Eval_TF_Array_Element;

   function Compare (L, R : Memtyp) return Order_Type is
   begin
      case L.Typ.Kind is
         when Type_Bit
           | Type_Logic =>
            declare
               Lv : constant Ghdl_U8 := Read_U8 (L.Mem);
               Rv : constant Ghdl_U8 := Read_U8 (R.Mem);
            begin
               if Lv < Rv then
                  return Less;
               elsif Lv > Rv then
                  return Greater;
               else
                  return Equal;
               end if;
            end;
         when Type_Discrete =>
            pragma Assert (L.Typ.Sz = R.Typ.Sz);
            if L.Typ.Sz = 1 then
               declare
                  Lv : constant Ghdl_U8 := Read_U8 (L.Mem);
                  Rv : constant Ghdl_U8 := Read_U8 (R.Mem);
               begin
                  if Lv < Rv then
                     return Less;
                  elsif Lv > Rv then
                     return Greater;
                  else
                     return Equal;
                  end if;
               end;
            elsif L.Typ.Sz = 4 then
               declare
                  Lv : constant Ghdl_I32 := Read_I32 (L.Mem);
                  Rv : constant Ghdl_I32 := Read_I32 (R.Mem);
               begin
                  if Lv < Rv then
                     return Less;
                  elsif Lv > Rv then
                     return Greater;
                  else
                     return Equal;
                  end if;
               end;
            else
               raise Internal_Error;
            end if;
         when others =>
            raise Internal_Error;
      end case;
   end Compare;

   function Compare_Array (L, R : Memtyp) return Order_Type
   is
      Len : Uns32;
      Res : Order_Type;
   begin
      Len := Uns32'Min (L.Typ.Abound.Len, R.Typ.Abound.Len);
      for I in 1 .. Size_Type (Len) loop
         Res := Compare
           ((L.Typ.Arr_El, L.Mem + (I - 1) * L.Typ.Arr_El.Sz),
            (R.Typ.Arr_El, R.Mem + (I - 1) * R.Typ.Arr_El.Sz));
         if Res /= Equal then
            return Res;
         end if;
      end loop;
      if L.Typ.Abound.Len > Len then
         return Greater;
      end if;
      if R.Typ.Abound.Len > Len then
         return Less;
      end if;
      return Equal;
   end Compare_Array;

   --  Execute shift and rot.
   --  ZERO is the value to be used for '0' (for shifts).
   --  It is 0 for bit, 2 for std_logic.
   function Execute_Shift_Operator (Left : Memtyp;
                                    Count : Int64;
                                    Zero : Ghdl_U8;
                                    Op : Iir_Predefined_Shift_Functions)
                                   return Memtyp
   is
      Cnt : Uns32;
      Len : constant Uns32 := Left.Typ.Abound.Len;
      Dir_Left : Boolean;
      P : Size_Type;
      Res : Memtyp;
      E : Ghdl_U8;
   begin
      --  LRM93 7.2.3
      --  That is, if R is 0 or if L is a null array, the return value is L.
      if Count = 0 or else Len = 0 then
         return Left;
      end if;

      case Op is
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
         Cnt := Uns32 (-Count);
         Dir_Left := not Dir_Left;
      else
         Cnt := Uns32 (Count);
      end if;

      case Op is
         when Iir_Predefined_Array_Sll
           | Iir_Predefined_Array_Srl =>
            E := Zero;
         when Iir_Predefined_Array_Sla
           | Iir_Predefined_Array_Sra =>
            if Dir_Left then
               E := Read_U8 (Left.Mem + Size_Type (Len - 1));
            else
               E := Read_U8 (Left.Mem);
            end if;
         when Iir_Predefined_Array_Rol
           | Iir_Predefined_Array_Ror =>
            Cnt := Cnt mod Len;
            if not Dir_Left then
               Cnt := (Len - Cnt) mod Len;
            end if;
      end case;

      Res := Create_Memory (Left.Typ);
      P := 0;

      case Op is
         when Iir_Predefined_Array_Sll
           | Iir_Predefined_Array_Srl
           | Iir_Predefined_Array_Sla
           | Iir_Predefined_Array_Sra =>
            if Dir_Left then
               if Cnt < Len then
                  for I in Cnt .. Len - 1 loop
                     Write_U8 (Res.Mem + P,
                               Read_U8 (Left.Mem + Size_Type (I)));
                     P := P + 1;
                  end loop;
               else
                  Cnt := Len;
               end if;
               for I in 0 .. Cnt - 1 loop
                  Write_U8 (Res.Mem + P, E);
                  P := P + 1;
               end loop;
            else
               if Cnt > Len then
                  Cnt := Len;
               end if;
               for I in 0 .. Cnt - 1 loop
                  Write_U8 (Res.Mem + P, E);
                  P := P + 1;
               end loop;
               for I in Cnt .. Len - 1 loop
                  Write_U8 (Res.Mem + P,
                            Read_U8 (Left.Mem + Size_Type (I - Cnt)));
                  P := P + 1;
               end loop;
            end if;
         when Iir_Predefined_Array_Rol
           | Iir_Predefined_Array_Ror =>
            for I in 1 .. Len loop
               Write_U8 (Res.Mem + P,
                         Read_U8 (Left.Mem + Size_Type (Cnt)));
               P := P + 1;
               Cnt := Cnt + 1;
               if Cnt = Len then
                  Cnt := 0;
               end if;
            end loop;
      end case;
      return Res;
   end Execute_Shift_Operator;

   procedure Check_Integer_Overflow (Inst : Synth_Instance_Acc;
                                     Val : in out Int64;
                                     Typ : Type_Acc;
                                     Loc : Syn_Src) is
   begin
      pragma Assert (Typ.Kind = Type_Discrete);
      case Typ.Sz is
         when 4 =>
            if Val < -2**31 or Val >= 2**31 then
               Error_Msg_Synth (Inst, Loc, "integer overflow");
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

   function Create_Memory_Boolean (V : Boolean) return Memtyp is
   begin
      return Create_Memory_U8 (Boolean'Pos (V), Boolean_Type);
   end Create_Memory_Boolean;

   function Eval_Vector_Monadic (Vec : Memtyp; Op : Table_1d) return Memtyp
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
   end Eval_Vector_Monadic;

   function Eval_Vector_Reduce (Init : Std_Ulogic;
                                Vec : Memtyp;
                                Op : Table_2d;
                                Neg : Boolean) return Memtyp
   is
      El_Typ : constant Type_Acc := Vec.Typ.Arr_El;
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

      if Neg then
         Res := Not_Table (Res);
      end if;

      return Create_Memory_U8 (Std_Ulogic'Pos (Res), El_Typ);
   end Eval_Vector_Reduce;

   function Eval_TF_Vector_Monadic (Vec : Memtyp) return Memtyp
   is
      Len : constant Iir_Index32 := Vec_Length (Vec.Typ);
      Res : Memtyp;
   begin
      Res := Create_Memory (Create_Res_Bound (Vec.Typ));
      for I in 1 .. Uns32 (Len) loop
         declare
            V : constant Boolean :=
              Boolean'Val (Read_U8 (Vec.Mem + Size_Type (I - 1)));
         begin
            Write_U8 (Res.Mem + Size_Type (I - 1), Boolean'Pos (not V));
         end;
      end loop;
      return Res;
   end Eval_TF_Vector_Monadic;

   function Eval_TF_Vector_Reduce (Init : Boolean;
                                   Neg : Boolean;
                                   Vec : Memtyp;
                                   Op : Tf_Table_2d) return Memtyp
   is
      El_Typ : constant Type_Acc := Vec.Typ.Arr_El;
      Res : Boolean;
   begin
      Res := Init;
      for I in 1 .. Size_Type (Vec.Typ.Abound.Len) loop
         declare
            V : constant Boolean := Boolean'Val (Read_U8 (Vec.Mem + (I - 1)));
         begin
            Res := Op (Res, V);
         end;
      end loop;

      return Create_Memory_U8 (Boolean'Pos (Res xor Neg), El_Typ);
   end Eval_TF_Vector_Reduce;

   function Eval_Vector_Maximum (Vec : Memtyp) return Memtyp
   is
      Etyp : constant Type_Acc := Vec.Typ.Arr_El;
      Len : constant Uns32 := Vec.Typ.Abound.Len;
   begin
      case Etyp.Kind is
         when Type_Logic
           | Type_Bit
           | Type_Discrete =>
            declare
               Res : Int64;
               V : Int64;
            begin
               case Etyp.Drange.Dir is
                  when Dir_To =>
                     Res := Etyp.Drange.Left;
                  when Dir_Downto =>
                     Res := Etyp.Drange.Right;
               end case;

               for I in 1 .. Len loop
                  V := Read_Discrete
                    (Vec.Mem + Size_Type (I - 1) * Etyp.Sz, Etyp);
                  if V > Res then
                     Res := V;
                  end if;
               end loop;
               return Create_Memory_Discrete (Res, Etyp);
            end;
         when Type_Float =>
            declare
               Res : Fp64;
               V : Fp64;
            begin
               case Etyp.Frange.Dir is
                  when Dir_To =>
                     Res := Etyp.Frange.Left;
                  when Dir_Downto =>
                     Res := Etyp.Frange.Right;
               end case;

               for I in 1 .. Len loop
                  V := Read_Fp64
                    (Vec.Mem + Size_Type (I - 1) * Etyp.Sz);
                  if V > Res then
                     Res := V;
                  end if;
               end loop;
               return Create_Memory_Fp64 (Res, Etyp);
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Eval_Vector_Maximum;

   function Eval_Vector_Minimum (Vec : Memtyp) return Memtyp
   is
      Etyp : constant Type_Acc := Vec.Typ.Arr_El;
      Len : constant Uns32 := Vec.Typ.Abound.Len;
   begin
      case Etyp.Kind is
         when Type_Logic
           | Type_Bit
           | Type_Discrete =>
            declare
               Res : Int64;
               V : Int64;
            begin
               case Etyp.Drange.Dir is
                  when Dir_To =>
                     Res := Etyp.Drange.Right;
                  when Dir_Downto =>
                     Res := Etyp.Drange.Left;
               end case;

               for I in 1 .. Len loop
                  V := Read_Discrete
                    (Vec.Mem + Size_Type (I - 1) * Etyp.Sz, Etyp);
                  if V < Res then
                     Res := V;
                  end if;
               end loop;
               return Create_Memory_Discrete (Res, Etyp);
            end;
         when Type_Float =>
            declare
               Res : Fp64;
               V : Fp64;
            begin
               case Etyp.Frange.Dir is
                  when Dir_To =>
                     Res := Etyp.Frange.Right;
                  when Dir_Downto =>
                     Res := Etyp.Frange.Left;
               end case;

               for I in 1 .. Len loop
                  V := Read_Fp64
                    (Vec.Mem + Size_Type (I - 1) * Etyp.Sz);
                  if V < Res then
                     Res := V;
                  end if;
               end loop;
               return Create_Memory_Fp64 (Res, Etyp);
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Eval_Vector_Minimum;

   function Eval_To_Log_Vector (Arg : Uns64; Sz : Int64; Res_Type : Type_Acc)
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
   end Eval_To_Log_Vector;

   function Eval_To_Bit_Vector (Arg : Uns64; Sz : Int64; Res_Type : Type_Acc)
                               return Memtyp
   is
      Len : constant Size_Type := Size_Type (Sz);
      El_Type : constant Type_Acc := Get_Array_Element (Res_Type);
      Res : Memtyp;
      Bnd : Type_Acc;
      B : Uns64;
   begin
      Bnd := Create_Vec_Type_By_Length (Width (Sz), El_Type);
      Res := Create_Memory (Bnd);
      for I in 1 .. Len loop
         B := Shift_Right_Arithmetic (Arg, Natural (I - 1)) and 1;
         Write_U8 (Res.Mem + (Len - I), Ghdl_U8 (B));
      end loop;
      return Res;
   end Eval_To_Bit_Vector;

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

   function Eval_Array_Char_To_String (Param : Memtyp;
                                       Res_Typ : Type_Acc;
                                       Imp : Node) return Memtyp
   is
      use Vhdl.Utils;
      use Name_Table;
      Len : constant Uns32 := Param.Typ.Abound.Len;
      Elt : constant Type_Acc := Param.Typ.Arr_El;
      Etype : constant Node := Get_Base_Type
        (Get_Element_Subtype
           (Get_Type (Get_Interface_Declaration_Chain (Imp))));
      pragma Assert (Get_Kind (Etype) = Iir_Kind_Enumeration_Type_Definition);
      Enums : constant Iir_Flist := Get_Enumeration_Literal_List (Etype);
      Lit : Node;
      Lit_Id : Name_Id;
      Bnd : Bound_Type;
      Res_St : Type_Acc;
      Res : Memtyp;
      V : Int64;
   begin
      Bnd := Elab.Vhdl_Types.Create_Bounds_From_Length
        (Res_Typ.Uarr_Idx.Drange, Iir_Index32 (Len));
      Res_St := Create_Onedimensional_Array_Subtype
        (Res_Typ, Bnd, Res_Typ.Uarr_El);
      Res := Create_Memory (Res_St);
      for I in 1 .. Len loop
         V := Read_Discrete (Param.Mem + Size_Type (I - 1) * Elt.Sz, Elt);
         Lit := Get_Nth_Element (Enums, Natural (V));
         Lit_Id := Get_Identifier (Lit);
         pragma Assert (Is_Character (Lit_Id));
         Write_U8 (Res.Mem + Size_Type (I - 1),
                   Character'Pos (Get_Character (Lit_Id)));
      end loop;
      return Res;
   end Eval_Array_Char_To_String;

   function String_To_Memtyp (Str : String; Styp : Type_Acc) return Memtyp
   is
      Len : constant Natural := Str'Length;
      Bnd : Bound_Type;
      Typ : Type_Acc;
      Res : Memtyp;
   begin
      Bnd := (Dir => Dir_To, Left => 1, Right => Int32 (Len),
              Len => Uns32 (Len));
      Typ := Create_Array_Type (Bnd, True, Styp.Uarr_El);

      Res := Create_Memory (Typ);
      for I in Str'Range loop
         Write_U8 (Res.Mem + Size_Type (I - Str'First),
                   Character'Pos (Str (I)));
      end loop;
      return Res;
   end String_To_Memtyp;

   function Eval_Enum_To_String (Param : Memtyp;
                                 Res_Typ : Type_Acc;
                                 Imp : Node) return Memtyp
   is
      use Vhdl.Utils;
      use Name_Table;
      Etype : constant Node := Get_Base_Type
        (Get_Type (Get_Interface_Declaration_Chain (Imp)));
      pragma Assert (Get_Kind (Etype) = Iir_Kind_Enumeration_Type_Definition);
      Enums : constant Iir_Flist := Get_Enumeration_Literal_List (Etype);
      Lit : Node;
      Lit_Id : Name_Id;
      V : Int64;
      C : String (1 .. 1);
   begin
      V := Read_Discrete (Param.Mem, Param.Typ);
      Lit := Get_Nth_Element (Enums, Natural (V));
      Lit_Id := Get_Identifier (Lit);
      if Is_Character (Lit_Id) then
         C (1) := Get_Character (Lit_Id);
         return String_To_Memtyp (C, Res_Typ);
      else
         return String_To_Memtyp (Image (Lit_Id), Res_Typ);
      end if;
   end Eval_Enum_To_String;

   Hex_Chars : constant array (Natural range 0 .. 15) of Character :=
     "0123456789ABCDEF";

   function Eval_Bit_Vector_To_String (Val : Memtyp;
                                       Res_Typ : Type_Acc;
                                       Log_Base : Natural) return Memtyp
   is
      Base : constant Natural := 2 ** Log_Base;
      Blen : constant Natural := Natural (Val.Typ.Abound.Len);
      Str : String (1 .. (Blen + Log_Base - 1) / Log_Base);
      Pos : Natural;
      V : Natural;
      N : Natural;
   begin
      V := 0;
      N := 1;
      Pos := Str'Last;
      for I in 1 .. Blen loop
         V := V + Natural (Read_U8 (Val.Mem + Size_Type (Blen - I))) * N;
         N := N * 2;
         if N = Base or else I = Blen then
            Str (Pos) := Hex_Chars (V);
            Pos := Pos - 1;
            N := 1;
            V := 0;
         end if;
      end loop;
      return String_To_Memtyp (Str, Res_Typ);
   end Eval_Bit_Vector_To_String;

   function Eval_Logic_Vector_To_String (Val : Memtyp;
                                         Res_Typ : Type_Acc;
                                         Is_Signed : Boolean;
                                         Log_Base : Natural) return Memtyp
   is
      Base : constant Natural := 2 ** Log_Base;
      Blen : constant Uns32 := Val.Typ.Abound.Len;
      Str : String (1 .. (Natural (Blen) + Log_Base - 1) / Log_Base);
      Pos : Natural;
      D : Std_Ulogic;
      V : Natural;
      N : Natural;
      Has_X, Has_Z, Has_D : Boolean;
   begin
      V := 0;
      N := 1;
      Has_X := False;
      Has_Z := False;
      Has_D := False;
      Pos := Str'Last;
      for I in 1 .. Blen loop
         D := Read_Std_Logic (Val.Mem, Blen - I);
         case D is
            when '0' | 'L' =>
               Has_D := True;
            when '1' | 'H' =>
               Has_D := True;
               V := V + N;
            when 'Z' | 'W' =>
               Has_Z := True;
            when 'X' | 'U' | '-' =>
               Has_X := True;
         end case;
         N := N * 2;
         if N = Base or else I = Blen then
            if Has_X or (Has_Z and Has_D) then
               Str (Pos) := 'X';
            elsif Has_Z then
               Str (Pos) := 'Z';
            else
               if Is_Signed and N < Base and (D = '1' or D = 'H') then
                  --  Sign extend.
                  loop
                     V := V + N;
                     N := N * 2;
                     exit when N = Base;
                  end loop;
               end if;
               Str (Pos) := Hex_Chars (V);
            end if;
            Pos := Pos - 1;
            N := 1;
            V := 0;
            Has_X := False;
            Has_Z := False;
            Has_D := False;
         end if;
      end loop;
      return String_To_Memtyp (Str, Res_Typ);
   end Eval_Logic_Vector_To_String;

   function Eval_To_X01 (Val : Memtyp; Map : Table_1d) return Memtyp
   is
      Len : constant Uns32 := Val.Typ.Abound.Len;
      Res : Memtyp;
      B : Std_Ulogic;
   begin
      Res := Create_Memory (Create_Res_Bound (Val.Typ));
      for I in 1 .. Len loop
         B := Read_Std_Logic (Val.Mem, I - 1);
         B := Map (B);
         Write_Std_Logic (Res.Mem, I - 1, B);
      end loop;
      return Res;
   end Eval_To_X01;

   function Eval_Static_Predefined_Function_Call (Inst : Synth_Instance_Acc;
                                                  Param1 : Memtyp;
                                                  Param2 : Memtyp;
                                                  Res_Typ : Type_Acc;
                                                  Expr : Node) return Memtyp
   is
      Imp  : constant Node := Get_Implementation (Expr);
      Def : constant Iir_Predefined_Functions :=
        Get_Implicit_Definition (Imp);
   begin
      case Def is
         when Iir_Predefined_Error =>
            return Null_Memtyp;

         when Iir_Predefined_Boolean_Not
           | Iir_Predefined_Bit_Not =>
            return Create_Memory_U8 (1 - Read_U8 (Param1), Param1.Typ);

         when Iir_Predefined_Boolean_Or
            | Iir_Predefined_Bit_Or
            | Iir_Predefined_Boolean_And
            | Iir_Predefined_Bit_And =>
            --  Short-circuit operators.
            raise Internal_Error;

         when Iir_Predefined_Boolean_Rising_Edge
            | Iir_Predefined_Boolean_Falling_Edge
            | Iir_Predefined_Bit_Rising_Edge
            | Iir_Predefined_Bit_Falling_Edge =>
            --  Cannot be static
            raise Internal_Error;

         when Iir_Predefined_Boolean_Xor
            | Iir_Predefined_Bit_Xor =>
            return Create_Memory_U8
              (Boolean'Pos (Boolean'Val (Read_Discrete (Param1))
                              xor Boolean'Val (Read_Discrete (Param2))),
               Res_Typ);

         when Iir_Predefined_Boolean_Nand
            | Iir_Predefined_Bit_Nand =>
            return Create_Memory_U8
              (Boolean'Pos (not (Boolean'Val (Read_Discrete (Param1))
                                   and Boolean'Val (Read_Discrete (Param2)))),
               Res_Typ);

         when Iir_Predefined_Boolean_Nor
            | Iir_Predefined_Bit_Nor =>
            return Create_Memory_U8
              (Boolean'Pos (not (Boolean'Val (Read_Discrete (Param1))
                                   or Boolean'Val (Read_Discrete (Param2)))),
               Res_Typ);

         when Iir_Predefined_Bit_Condition =>
            return Create_Memory_U8 (Read_U8 (Param1), Param1.Typ);

         when Iir_Predefined_Integer_Negation
           | Iir_Predefined_Physical_Negation =>
            return Create_Memory_Discrete
              (-Read_Discrete (Param1), Param1.Typ);
         when Iir_Predefined_Integer_Absolute
           | Iir_Predefined_Physical_Absolute =>
            return Create_Memory_Discrete
              (abs Read_Discrete (Param1), Param1.Typ);
         when Iir_Predefined_Integer_Identity
           | Iir_Predefined_Physical_Identity =>
            return Param1;

         when Iir_Predefined_Floating_Negation =>
            return Create_Memory_Fp64 (-Read_Fp64 (Param1), Param1.Typ);
         when Iir_Predefined_Floating_Identity =>
            return Param1;
         when Iir_Predefined_Floating_Absolute =>
            return Create_Memory_Fp64 (abs Read_Fp64 (Param1), Param1.Typ);

         when Iir_Predefined_Vector_Maximum =>
            return Eval_Vector_Maximum (Param1);
         when Iir_Predefined_Vector_Minimum =>
            return Eval_Vector_Minimum (Param1);

         when Iir_Predefined_TF_Array_Not =>
            return Eval_TF_Vector_Monadic (Param1);

         when Iir_Predefined_TF_Reduction_Or =>
            return Eval_TF_Vector_Reduce (False, False, Param1, Tf_2d_Or);
         when Iir_Predefined_TF_Reduction_And =>
            return Eval_TF_Vector_Reduce (True, False, Param1, Tf_2d_And);
         when Iir_Predefined_TF_Reduction_Xor =>
            return Eval_TF_Vector_Reduce (False, False, Param1, Tf_2d_Xor);
         when Iir_Predefined_TF_Reduction_Nor =>
            return Eval_TF_Vector_Reduce (False, True, Param1, Tf_2d_Or);
         when Iir_Predefined_TF_Reduction_Nand =>
            return Eval_TF_Vector_Reduce (True, True, Param1, Tf_2d_And);
         when Iir_Predefined_TF_Reduction_Xnor =>
            return Eval_TF_Vector_Reduce (False, True, Param1, Tf_2d_Xor);

         when Iir_Predefined_Ieee_1164_Condition_Operator =>
            --  Constant std_logic: need to convert.
            declare
               Val : Uns32;
               Zx : Uns32;
            begin
               From_Std_Logic (Int64 (Read_U8 (Param1)), Val, Zx);
               return Create_Memory_U8
                 (Boolean'Pos (Val = 1 and Zx = 0), Boolean_Type);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Neg_Sgn =>
            return Neg_Vec (Param1, +Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Abs_Sgn =>
            return Abs_Vec (Param1, +Expr);

         when Iir_Predefined_Ieee_1164_Vector_Not
           | Iir_Predefined_Ieee_Numeric_Std_Not_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Not_Sgn =>
            return Eval_Vector_Monadic (Param1, Not_Table);

         when Iir_Predefined_Ieee_1164_Scalar_Not =>
            return Create_Memory_U8
              (Std_Ulogic'Pos (Not_Table (Read_Std_Logic (Param1.Mem, 0))),
               Param1.Typ);

         when Iir_Predefined_Ieee_1164_And_Suv
            | Iir_Predefined_Ieee_Numeric_Std_And_Uns
            | Iir_Predefined_Ieee_Numeric_Std_And_Sgn =>
            return Eval_Vector_Reduce ('1', Param1, And_Table, False);
         when Iir_Predefined_Ieee_1164_Nand_Suv
            | Iir_Predefined_Ieee_Numeric_Std_Nand_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Nand_Sgn =>
            return Eval_Vector_Reduce ('1', Param1, And_Table, True);

         when Iir_Predefined_Ieee_1164_Or_Suv
            | Iir_Predefined_Ieee_Numeric_Std_Or_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Or_Sgn =>
            return Eval_Vector_Reduce ('0', Param1, Or_Table, False);
         when Iir_Predefined_Ieee_1164_Nor_Suv
            | Iir_Predefined_Ieee_Numeric_Std_Nor_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Nor_Sgn =>
            return Eval_Vector_Reduce ('0', Param1, Or_Table, True);

         when Iir_Predefined_Ieee_1164_Xor_Suv
            | Iir_Predefined_Ieee_Numeric_Std_Xor_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Xor_Sgn =>
            return Eval_Vector_Reduce ('0', Param1, Xor_Table, False);
         when Iir_Predefined_Ieee_1164_Xnor_Suv
            | Iir_Predefined_Ieee_Numeric_Std_Xnor_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Xnor_Sgn =>
            return Eval_Vector_Reduce ('0', Param1, Xor_Table, True);

         when Iir_Predefined_Ieee_Std_Logic_Arith_Id_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Id_Uns_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Id_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Id_Sgn_Slv
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Id_Slv
            | Iir_Predefined_Ieee_Std_Logic_Signed_Id_Slv =>
            return Param1;

         when Iir_Predefined_Ieee_Std_Logic_Arith_Neg_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Neg_Sgn_Slv
            | Iir_Predefined_Ieee_Std_Logic_Signed_Neg_Slv =>
            return Neg_Sgn_Sgn (Param1, +Expr);

         when Iir_Predefined_Ieee_Std_Logic_Arith_Abs_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Abs_Sgn_Slv
            | Iir_Predefined_Ieee_Std_Logic_Signed_Abs_Slv =>
            return Abs_Sgn_Sgn (Param1, +Expr);

         when Iir_Predefined_Integer_Plus
           | Iir_Predefined_Physical_Plus =>
            declare
               Res : Int64;
            begin
               Res := Read_Discrete (Param1) + Read_Discrete (Param2);
               Check_Integer_Overflow (Inst, Res, Res_Typ, Expr);
               return Create_Memory_Discrete (Res, Res_Typ);
            end;
         when Iir_Predefined_Integer_Minus
            | Iir_Predefined_Physical_Minus =>
            declare
               Res : Int64;
            begin
               Res := Read_Discrete (Param1) - Read_Discrete (Param2);
               Check_Integer_Overflow (Inst, Res, Res_Typ, Expr);
               return Create_Memory_Discrete (Res, Res_Typ);
            end;
         when Iir_Predefined_Integer_Mul
           | Iir_Predefined_Physical_Integer_Mul
           | Iir_Predefined_Integer_Physical_Mul =>
            declare
               Res : Int64;
            begin
               Res := Read_Discrete (Param1) * Read_Discrete (Param2);
               Check_Integer_Overflow (Inst, Res, Res_Typ, Expr);
               return Create_Memory_Discrete (Res, Res_Typ);
            end;
         when Iir_Predefined_Integer_Div
            | Iir_Predefined_Physical_Physical_Div
            | Iir_Predefined_Physical_Integer_Div =>
            declare
               Rv : Int64;
               Res : Int64;
            begin
               Rv := Read_Discrete (Param2);
               if Rv = 0 then
                  Error_Msg_Synth (Inst, Expr, "division by zero");
                  return Null_Memtyp;
               end if;
               Res := Read_Discrete (Param1) / Rv;
               Check_Integer_Overflow (Inst, Res, Res_Typ, Expr);
               return Create_Memory_Discrete (Res, Res_Typ);
            end;
         when Iir_Predefined_Integer_Mod =>
            declare
               Res : Int64;
            begin
               Res := Read_Discrete (Param1) mod Read_Discrete (Param2);
               Check_Integer_Overflow (Inst, Res, Res_Typ, Expr);
               return Create_Memory_Discrete (Res, Res_Typ);
            end;
         when Iir_Predefined_Integer_Rem =>
            declare
               Res : Int64;
            begin
               Res := Read_Discrete (Param1) rem Read_Discrete (Param2);
               Check_Integer_Overflow (Inst, Res, Res_Typ, Expr);
               return Create_Memory_Discrete (Res, Res_Typ);
            end;

         when Iir_Predefined_Integer_Exp =>
            declare
               Lv : Ghdl_I64;
               Rv : Std_Integer;
               Res : Ghdl_I64;
               Ovf : Boolean;
            begin
               Lv := Ghdl_I64 (Read_Discrete (Param1));
               Rv := Std_Integer (Read_Discrete (Param2));
               Grt.Arith.Exp_I64 (Lv, Rv, Res, Ovf);
               if Ovf then
                  Error_Msg_Synth (Inst, Expr, "exponentiation overflow");
                  Res := 0;
               end if;
               return Create_Memory_Discrete (Int64 (Res), Res_Typ);
            end;

         when Iir_Predefined_Integer_Less_Equal
            | Iir_Predefined_Physical_Less_Equal
            | Iir_Predefined_Enum_Less_Equal =>
            return Create_Memory_Boolean
              (Read_Discrete (Param1) <= Read_Discrete (Param2));
         when Iir_Predefined_Integer_Less
            | Iir_Predefined_Physical_Less
            | Iir_Predefined_Enum_Less =>
            return Create_Memory_Boolean
              (Read_Discrete (Param1) < Read_Discrete (Param2));
         when Iir_Predefined_Integer_Greater_Equal
            | Iir_Predefined_Physical_Greater_Equal
            | Iir_Predefined_Enum_Greater_Equal =>
            return Create_Memory_Boolean
              (Read_Discrete (Param1) >= Read_Discrete (Param2));
         when Iir_Predefined_Integer_Greater
            | Iir_Predefined_Physical_Greater
            | Iir_Predefined_Enum_Greater =>
            return Create_Memory_Boolean
              (Read_Discrete (Param1) > Read_Discrete (Param2));
         when Iir_Predefined_Integer_Equality
            | Iir_Predefined_Physical_Equality
            | Iir_Predefined_Enum_Equality
            | Iir_Predefined_Bit_Match_Equality =>
            return Create_Memory_Boolean
              (Read_Discrete (Param1) = Read_Discrete (Param2));
         when Iir_Predefined_Integer_Inequality
            | Iir_Predefined_Physical_Inequality
            | Iir_Predefined_Enum_Inequality
            | Iir_Predefined_Bit_Match_Inequality =>
            return Create_Memory_Boolean
              (Read_Discrete (Param1) /= Read_Discrete (Param2));

         when Iir_Predefined_Physical_Real_Mul =>
            return Create_Memory_Discrete
              (Int64 (Fp64 (Read_Discrete (Param1)) * Read_Fp64 (Param2)),
               Res_Typ);
         when Iir_Predefined_Real_Physical_Mul =>
            return Create_Memory_Discrete
              (Int64 (Read_Fp64 (Param1) * Fp64 (Read_Discrete (Param2))),
               Res_Typ);
         when Iir_Predefined_Physical_Real_Div =>
            return Create_Memory_Discrete
              (Int64 (Fp64 (Read_Discrete (Param1)) / Read_Fp64 (Param2)),
               Res_Typ);

         when Iir_Predefined_Floating_Less =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Fp64 (Param1) < Read_Fp64 (Param2)),
               Boolean_Type);
         when Iir_Predefined_Floating_Less_Equal =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Fp64 (Param1) <= Read_Fp64 (Param2)),
               Boolean_Type);
         when Iir_Predefined_Floating_Equality =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Fp64 (Param1) = Read_Fp64 (Param2)),
               Boolean_Type);
         when Iir_Predefined_Floating_Inequality =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Fp64 (Param1) /= Read_Fp64 (Param2)),
               Boolean_Type);
         when Iir_Predefined_Floating_Greater =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Fp64 (Param1) > Read_Fp64 (Param2)),
               Boolean_Type);
         when Iir_Predefined_Floating_Greater_Equal =>
            return Create_Memory_U8
              (Boolean'Pos (Read_Fp64 (Param1) >= Read_Fp64 (Param2)),
               Boolean_Type);

         when Iir_Predefined_Floating_Plus =>
            return Create_Memory_Fp64 (Read_Fp64 (Param1) + Read_Fp64 (Param2),
                                       Res_Typ);
         when Iir_Predefined_Floating_Minus =>
            return Create_Memory_Fp64 (Read_Fp64 (Param1) - Read_Fp64 (Param2),
                                       Res_Typ);
         when Iir_Predefined_Floating_Mul =>
            return Create_Memory_Fp64 (Read_Fp64 (Param1) * Read_Fp64 (Param2),
                                       Res_Typ);
         when Iir_Predefined_Floating_Div =>
            return Create_Memory_Fp64 (Read_Fp64 (Param1) / Read_Fp64 (Param2),
                                       Res_Typ);
         when Iir_Predefined_Floating_Exp =>
            return Create_Memory_Fp64
              (Read_Fp64 (Param1)
                 ** Integer (Read_Discrete (Param2)), Res_Typ);

         when Iir_Predefined_Universal_R_I_Div =>
            declare
               Rv : Int64;
               Res : Fp64;
            begin
               Rv := Read_Discrete (Param2);
               if Rv = 0 then
                  Error_Msg_Synth (Inst, Expr, "division by zero");
                  return Null_Memtyp;
               end if;
               Res := Read_Fp64 (Param1) / Fp64 (Rv);
               return Create_Memory_Fp64 (Res, Res_Typ);
            end;

         when Iir_Predefined_Array_Array_Concat =>
            declare
               use Flags;
               L_Len : constant Iir_Index32 :=
                 Iir_Index32 (Get_Bound_Length (Param1.Typ));
               R_Len : constant Iir_Index32 :=
                 Iir_Index32 (Get_Bound_Length (Param2.Typ));
               Le_Typ : constant Type_Acc := Get_Array_Element (Param1.Typ);
               Re_Typ : constant Type_Acc := Get_Array_Element (Param2.Typ);
               El_Typ : Type_Acc;
               Bnd : Bound_Type;
               Res_St : Type_Acc;
               Res : Memtyp;
            begin
               Check_Matching_Bounds (Le_Typ, Re_Typ, Expr);
               if L_Len = 0 and R_Len = 0 then
                  --  LRM08 9.2.5 Adding operators
                  --  If both operands are null arrays, then the result of the
                  --  concatenation is the right operand.
                  return Param2;
               end if;
               if Vhdl_Std > Vhdl_87 then
                  Bnd := Elab.Vhdl_Types.Create_Bounds_From_Length
                    (Get_Uarray_Index (Res_Typ).Drange, L_Len + R_Len);
               else
                  --  LRM87 7.2.3
                  --  [...], unless the left operand is a null array, in which
                  --  case the result of the concatenation is the right
                  --  operand.
                  if L_Len = 0 then
                     return Param2;
                  end if;

                  --  LRM87 7.2.3
                  --  The left bound of the result is the left operand, [...]
                  --
                  --  LRM87 7.2.3
                  --  The direction of the result is the direction of the left
                  --  operand, [...]
                  Bnd.Left := Param1.Typ.Abound.Left;
                  Bnd.Dir := Param1.Typ.Abound.Dir;
                  Bnd.Len := Uns32 (L_Len + R_Len);
                  --  TODO: overflow.
                  case Bnd.Dir is
                     when Dir_To =>
                        Bnd.Right := Bnd.Left + Int32 (L_Len + R_Len - 1);
                     when Dir_Downto =>
                        Bnd.Right := Bnd.Left - Int32 (L_Len + R_Len - 1);
                  end case;
               end if;
               El_Typ := Unshare_Type_Expr (Le_Typ,
                                            Get_Array_Element (Res_Typ));
               Res_St := Create_Onedimensional_Array_Subtype
                 (Res_Typ, Bnd, El_Typ);
               Res := Create_Memory (Res_St);
               if Param1.Typ.Sz > 0 then
                  Copy_Memory (Res.Mem, Param1.Mem, Param1.Typ.Sz);
               end if;
               if Param2.Typ.Sz > 0 then
                  Copy_Memory
                    (Res.Mem + Param1.Typ.Sz, Param2.Mem, Param2.Typ.Sz);
               end if;
               return Res;
            end;
         when Iir_Predefined_Element_Array_Concat =>
            declare
               Rlen : constant Iir_Index32 :=
                 Iir_Index32 (Get_Bound_Length (Param2.Typ));
               Re_Typ : constant Type_Acc := Get_Array_Element (Param2.Typ);
               El_Typ : Type_Acc;
               Bnd : Bound_Type;
               Res_St : Type_Acc;
               Res : Memtyp;
            begin
               Check_Matching_Bounds (Param1.Typ, Re_Typ, Expr);
               Bnd := Elab.Vhdl_Types.Create_Bounds_From_Length
                 (Get_Uarray_Index (Res_Typ).Drange, 1 + Rlen);
               El_Typ := Unshare_Type_Expr (Re_Typ,
                                            Get_Array_Element (Res_Typ));
               Res_St := Create_Onedimensional_Array_Subtype
                 (Res_Typ, Bnd, El_Typ);
               Res := Create_Memory (Res_St);
               Copy_Memory (Res.Mem, Param1.Mem, Param1.Typ.Sz);
               Copy_Memory (Res.Mem + Param1.Typ.Sz,
                            Param2.Mem, Param2.Typ.Sz);
               return Res;
            end;
         when Iir_Predefined_Array_Element_Concat =>
            declare
               Llen : constant Iir_Index32 :=
                 Iir_Index32 (Get_Bound_Length (Param1.Typ));
               Le_Typ : constant Type_Acc := Get_Array_Element (Param1.Typ);
               El_Typ : Type_Acc;
               Bnd : Bound_Type;
               Res_St : Type_Acc;
               Res : Memtyp;
            begin
               Check_Matching_Bounds (Le_Typ, Param2.Typ, Expr);
               Bnd := Elab.Vhdl_Types.Create_Bounds_From_Length
                 (Get_Uarray_Index (Res_Typ).Drange, Llen + 1);
               El_Typ := Unshare_Type_Expr (Le_Typ,
                                            Get_Array_Element (Res_Typ));
               Res_St := Create_Onedimensional_Array_Subtype
                 (Res_Typ, Bnd, El_Typ);
               Res := Create_Memory (Res_St);
               Copy_Memory (Res.Mem, Param1.Mem, Param1.Typ.Sz);
               Copy_Memory (Res.Mem + Param1.Typ.Sz,
                            Param2.Mem, Param2.Typ.Sz);
               return Res;
            end;
         when Iir_Predefined_Element_Element_Concat =>
            declare
               Le_Typ : constant Type_Acc := Param1.Typ;
               El_Typ : Type_Acc;
               Bnd : Bound_Type;
               Res_St : Type_Acc;
               Res : Memtyp;
            begin
               Check_Matching_Bounds (Param1.Typ, Param2.Typ, Expr);
               Bnd := Elab.Vhdl_Types.Create_Bounds_From_Length
                 (Get_Uarray_Index (Res_Typ).Drange, 2);
               El_Typ := Unshare_Type_Expr (Le_Typ,
                                            Get_Array_Element (Res_Typ));
               Res_St := Create_Onedimensional_Array_Subtype
                 (Res_Typ, Bnd, El_Typ);
               Res := Create_Memory (Res_St);
               Copy_Memory (Res.Mem, Param1.Mem, El_Typ.Sz);
               Copy_Memory (Res.Mem + El_Typ.Sz,
                            Param2.Mem, El_Typ.Sz);
               return Res;
            end;

         when Iir_Predefined_Array_Equality
            | Iir_Predefined_Record_Equality
            | Iir_Predefined_Bit_Array_Match_Equality =>
            return Create_Memory_Boolean (Is_Equal (Param1, Param2));
         when Iir_Predefined_Array_Inequality
            | Iir_Predefined_Record_Inequality
            | Iir_Predefined_Bit_Array_Match_Inequality =>
            return Create_Memory_Boolean (not Is_Equal (Param1, Param2));

         when Iir_Predefined_Access_Equality =>
            return Create_Memory_Boolean
              (Read_Access (Param1) = Read_Access (Param2));
         when Iir_Predefined_Access_Inequality =>
            return Create_Memory_Boolean
              (Read_Access (Param1) /= Read_Access (Param2));

         when Iir_Predefined_Array_Less =>
            return Create_Memory_Boolean
              (Compare_Array (Param1, Param2) = Less);
         when Iir_Predefined_Array_Less_Equal =>
            return Create_Memory_Boolean
              (Compare_Array (Param1, Param2) <= Equal);
         when Iir_Predefined_Array_Greater =>
            return Create_Memory_Boolean
              (Compare_Array (Param1, Param2) = Greater);
         when Iir_Predefined_Array_Greater_Equal =>
            return Create_Memory_Boolean
              (Compare_Array (Param1, Param2) >= Equal);

         when Iir_Predefined_Array_Maximum =>
            --  IEEE 1076-2008 5.3.2.4 Predefined operations on array types
            if Compare_Array (Param1, Param2) = Less then
               return Param2;
            else
               return Param1;
            end if;
         when Iir_Predefined_Array_Minimum =>
            --  IEEE 1076-2008 5.3.2.4 Predefined operations on array types
            if Compare_Array (Param1, Param2) = Less then
               return Param1;
            else
               return Param2;
            end if;

         when Iir_Predefined_Array_Sll
           | Iir_Predefined_Array_Srl
           | Iir_Predefined_Array_Rol
           | Iir_Predefined_Array_Ror =>
            return Execute_Shift_Operator
              (Param1, Read_Discrete (Param2), 0, Def);

         when Iir_Predefined_TF_Array_And =>
            return Eval_TF_Vector_Dyadic
              (Inst, Param1, Param2, Tf_2d_And, Expr);
         when Iir_Predefined_TF_Array_Or =>
            return Eval_TF_Vector_Dyadic
              (Inst, Param1, Param2, Tf_2d_Or, Expr);
         when Iir_Predefined_TF_Array_Xor =>
            return Eval_TF_Vector_Dyadic
              (Inst, Param1, Param2, Tf_2d_Xor, Expr);
         when Iir_Predefined_TF_Array_Nand =>
            return Eval_TF_Vector_Dyadic
              (Inst, Param1, Param2, Tf_2d_Nand, Expr);
         when Iir_Predefined_TF_Array_Nor =>
            return Eval_TF_Vector_Dyadic
              (Inst, Param1, Param2, Tf_2d_Nor, Expr);
         when Iir_Predefined_TF_Array_Xnor =>
            return Eval_TF_Vector_Dyadic
              (Inst, Param1, Param2, Tf_2d_Xnor, Expr);

         when Iir_Predefined_TF_Element_Array_Or =>
            return Eval_TF_Array_Element (Param1, Param2, Tf_2d_Or);
         when Iir_Predefined_TF_Array_Element_Or =>
            return Eval_TF_Array_Element (Param2, Param1, Tf_2d_Or);

         when Iir_Predefined_TF_Element_Array_Nor =>
            return Eval_TF_Array_Element (Param1, Param2, Tf_2d_Nor);
         when Iir_Predefined_TF_Array_Element_Nor =>
            return Eval_TF_Array_Element (Param2, Param1, Tf_2d_Nor);

         when Iir_Predefined_TF_Element_Array_And =>
            return Eval_TF_Array_Element (Param1, Param2, Tf_2d_And);
         when Iir_Predefined_TF_Array_Element_And =>
            return Eval_TF_Array_Element (Param2, Param1, Tf_2d_And);

         when Iir_Predefined_TF_Element_Array_Nand =>
            return Eval_TF_Array_Element (Param1, Param2, Tf_2d_Nand);
         when Iir_Predefined_TF_Array_Element_Nand =>
            return Eval_TF_Array_Element (Param2, Param1, Tf_2d_Nand);

         when Iir_Predefined_TF_Element_Array_Xor =>
            return Eval_TF_Array_Element (Param1, Param2, Tf_2d_Xor);
         when Iir_Predefined_TF_Array_Element_Xor =>
            return Eval_TF_Array_Element (Param2, Param1, Tf_2d_Xor);

         when Iir_Predefined_TF_Element_Array_Xnor =>
            return Eval_TF_Array_Element (Param1, Param2, Tf_2d_Xnor);
         when Iir_Predefined_TF_Array_Element_Xnor =>
            return Eval_TF_Array_Element (Param2, Param1, Tf_2d_Xnor);

         when Iir_Predefined_Ieee_1164_Vector_And
           | Iir_Predefined_Ieee_Numeric_Std_And_Uns_Uns
           | Iir_Predefined_Ieee_Numeric_Std_And_Sgn_Sgn =>
            return Eval_Vector_Dyadic (Inst, Param1, Param2, And_Table, Expr);

         when Iir_Predefined_Ieee_1164_Vector_Nand
           | Iir_Predefined_Ieee_Numeric_Std_Nand_Uns_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Nand_Sgn_Sgn =>
            return Eval_Vector_Dyadic (Inst, Param1, Param2, Nand_Table, Expr);

         when Iir_Predefined_Ieee_1164_Vector_Or
           | Iir_Predefined_Ieee_Numeric_Std_Or_Uns_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Or_Sgn_Sgn =>
            return Eval_Vector_Dyadic (Inst, Param1, Param2, Or_Table, Expr);

         when Iir_Predefined_Ieee_1164_Vector_Nor
           | Iir_Predefined_Ieee_Numeric_Std_Nor_Uns_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Nor_Sgn_Sgn =>
            return Eval_Vector_Dyadic (Inst, Param1, Param2, Nor_Table, Expr);

         when Iir_Predefined_Ieee_1164_Vector_Xor
           | Iir_Predefined_Ieee_Numeric_Std_Xor_Uns_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Xor_Sgn_Sgn =>
            return Eval_Vector_Dyadic (Inst, Param1, Param2, Xor_Table, Expr);

         when Iir_Predefined_Ieee_1164_Vector_Xnor
            | Iir_Predefined_Ieee_Numeric_Std_Xnor_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Xnor_Sgn_Sgn =>
            return Eval_Vector_Dyadic (Inst, Param1, Param2, Xnor_Table, Expr);

         when Iir_Predefined_Ieee_1164_Scalar_And =>
            return Eval_Logic_Scalar (Param1, Param2, And_Table);
         when Iir_Predefined_Ieee_1164_Scalar_Or =>
            return Eval_Logic_Scalar (Param1, Param2, Or_Table);
         when Iir_Predefined_Ieee_1164_Scalar_Xor =>
            return Eval_Logic_Scalar (Param1, Param2, Xor_Table);
         when Iir_Predefined_Ieee_1164_Scalar_Nand =>
            return Eval_Logic_Scalar (Param1, Param2, Nand_Table);
         when Iir_Predefined_Ieee_1164_Scalar_Nor =>
            return Eval_Logic_Scalar (Param1, Param2, Nor_Table);
         when Iir_Predefined_Ieee_1164_Scalar_Xnor =>
            return Eval_Logic_Scalar (Param1, Param2, Xnor_Table);

         when Iir_Predefined_Std_Ulogic_Match_Equality =>
            return Eval_Logic_Scalar (Param1, Param2, Match_Eq_Table);
         when Iir_Predefined_Std_Ulogic_Match_Inequality =>
            return Eval_Logic_Scalar (Param1, Param2, Match_Eq_Table, True);
         when Iir_Predefined_Std_Ulogic_Match_Greater =>
            return Eval_Logic_Scalar (Param1, Param2, Match_Gt_Table);
         when Iir_Predefined_Std_Ulogic_Match_Greater_Equal =>
            return Eval_Logic_Scalar (Param1, Param2, Match_Ge_Table);
         when Iir_Predefined_Std_Ulogic_Match_Less_Equal =>
            return Eval_Logic_Scalar (Param1, Param2, Match_Le_Table);
         when Iir_Predefined_Std_Ulogic_Match_Less =>
            return Eval_Logic_Scalar (Param1, Param2, Match_Lt_Table);

         when Iir_Predefined_Std_Ulogic_Array_Match_Equality =>
            return Eval_Vector_Match (Inst, Param1, Param2, False, Expr);
         when Iir_Predefined_Std_Ulogic_Array_Match_Inequality =>
            return Eval_Vector_Match (Inst, Param1, Param2, True, Expr);

         when Iir_Predefined_Ieee_1164_And_Suv_Log
            | Iir_Predefined_Ieee_Numeric_Std_And_Uns_Log
            | Iir_Predefined_Ieee_Numeric_Std_And_Sgn_Log =>
            return Eval_Logic_Vector_Scalar (Param1, Param2, And_Table);
         when Iir_Predefined_Ieee_1164_Or_Suv_Log
            | Iir_Predefined_Ieee_Numeric_Std_Or_Uns_Log
            | Iir_Predefined_Ieee_Numeric_Std_Or_Sgn_Log =>
            return Eval_Logic_Vector_Scalar (Param1, Param2, Or_Table);
         when Iir_Predefined_Ieee_1164_Xor_Suv_Log
            | Iir_Predefined_Ieee_Numeric_Std_Xor_Uns_Log
            | Iir_Predefined_Ieee_Numeric_Std_Xor_Sgn_Log =>
            return Eval_Logic_Vector_Scalar (Param1, Param2, Xor_Table);
         when Iir_Predefined_Ieee_1164_Nand_Suv_Log
            | Iir_Predefined_Ieee_Numeric_Std_Nand_Uns_Log
            | Iir_Predefined_Ieee_Numeric_Std_Nand_Sgn_Log =>
            return Eval_Logic_Vector_Scalar (Param1, Param2, Nand_Table);
         when Iir_Predefined_Ieee_1164_Nor_Suv_Log
            | Iir_Predefined_Ieee_Numeric_Std_Nor_Uns_Log
            | Iir_Predefined_Ieee_Numeric_Std_Nor_Sgn_Log =>
            return Eval_Logic_Vector_Scalar (Param1, Param2, Nor_Table);
         when Iir_Predefined_Ieee_1164_Xnor_Suv_Log
            | Iir_Predefined_Ieee_Numeric_Std_Xnor_Uns_Log
            | Iir_Predefined_Ieee_Numeric_Std_Xnor_Sgn_Log =>
            return Eval_Logic_Vector_Scalar (Param1, Param2, Xnor_Table);

         when Iir_Predefined_Ieee_1164_And_Log_Suv
           | Iir_Predefined_Ieee_Numeric_Std_And_Log_Uns
           | Iir_Predefined_Ieee_Numeric_Std_And_Log_Sgn =>
            return Eval_Logic_Vector_Scalar (Param2, Param1, And_Table);
         when Iir_Predefined_Ieee_1164_Or_Log_Suv
           | Iir_Predefined_Ieee_Numeric_Std_Or_Log_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Or_Log_Sgn =>
            return Eval_Logic_Vector_Scalar (Param2, Param1, Or_Table);
         when Iir_Predefined_Ieee_1164_Xor_Log_Suv
           | Iir_Predefined_Ieee_Numeric_Std_Xor_Log_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Xor_Log_Sgn =>
            return Eval_Logic_Vector_Scalar (Param2, Param1, Xor_Table);
         when Iir_Predefined_Ieee_1164_Nand_Log_Suv
           | Iir_Predefined_Ieee_Numeric_Std_Nand_Log_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Nand_Log_Sgn =>
            return Eval_Logic_Vector_Scalar (Param2, Param1, Nand_Table);
         when Iir_Predefined_Ieee_1164_Nor_Log_Suv
           | Iir_Predefined_Ieee_Numeric_Std_Nor_Log_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Nor_Log_Sgn =>
            return Eval_Logic_Vector_Scalar (Param2, Param1, Nor_Table);
         when Iir_Predefined_Ieee_1164_Xnor_Log_Suv
           | Iir_Predefined_Ieee_Numeric_Std_Xnor_Log_Uns
           | Iir_Predefined_Ieee_Numeric_Std_Xnor_Log_Sgn =>
            return Eval_Logic_Vector_Scalar (Param2, Param1, Xnor_Table);

         when Iir_Predefined_Ieee_1164_Vector_Sll
            | Iir_Predefined_Ieee_Numeric_Std_Sla_Uns_Int =>
            return Execute_Shift_Operator
              (Param1, Read_Discrete (Param2), Std_Ulogic'Pos('0'),
               Iir_Predefined_Array_Sll);
         when Iir_Predefined_Ieee_1164_Vector_Srl
            | Iir_Predefined_Ieee_Numeric_Std_Sra_Uns_Int =>
            return Execute_Shift_Operator
              (Param1, Read_Discrete (Param2), Std_Ulogic'Pos('0'),
               Iir_Predefined_Array_Srl);
         when Iir_Predefined_Ieee_Numeric_Std_Sra_Sgn_Int =>
            declare
               Cnt : constant Int64 := Read_Discrete (Param2);
            begin
               if Cnt >= 0 then
                  return Execute_Shift_Operator
                    (Param1, Cnt, Std_Ulogic'Pos('0'),
                     Iir_Predefined_Array_Sra);
               else
                  return Execute_Shift_Operator
                    (Param1, -Cnt, Std_Ulogic'Pos('0'),
                     Iir_Predefined_Array_Sll);
               end if;
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Sla_Sgn_Int =>
            declare
               Cnt : Int64;
               Op : Iir_Predefined_Shift_Functions;
            begin
               Cnt := Read_Discrete (Param2);
               if Cnt >= 0 then
                  Op := Iir_Predefined_Array_Sll;
               else
                  Cnt := -Cnt;
                  Op :=Iir_Predefined_Array_Sra;
               end if;
               return Execute_Shift_Operator
                 (Param1, Cnt, Std_Ulogic'Pos('0'), Op);
            end;

         when Iir_Predefined_Ieee_1164_Vector_Rol
            | Iir_Predefined_Ieee_Numeric_Std_Rol_Uns_Int
            | Iir_Predefined_Ieee_Numeric_Std_Rol_Sgn_Int =>
            return Execute_Shift_Operator
              (Param1, Read_Discrete (Param2), Std_Ulogic'Pos('0'),
               Iir_Predefined_Array_Rol);
         when Iir_Predefined_Ieee_1164_Vector_Ror
            | Iir_Predefined_Ieee_Numeric_Std_Ror_Uns_Int
            | Iir_Predefined_Ieee_Numeric_Std_Ror_Sgn_Int =>
            return Execute_Shift_Operator
              (Param1, Read_Discrete (Param2), Std_Ulogic'Pos('0'),
               Iir_Predefined_Array_Ror);

         when Iir_Predefined_Ieee_Numeric_Std_Eq_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Eq_Slv_Slv =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Uns_Uns (Param1, Param2, Greater, +Expr) = Equal;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Uns_Nat =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Uns_Nat (Param1, Param2, Greater, +Expr) = Equal;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Nat_Uns =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Uns_Nat (Param2, Param1, Greater, +Expr) = Equal;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Eq_Slv_Slv =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Sgn_Sgn (Param1, Param2, Greater, +Expr) = Equal;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Signed_Eq_Slv_Int =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Sgn_Int (Param1, Param2, Greater, +Expr) = Equal;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Eq_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Eq_Int_Slv =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Sgn_Int (Param2, Param1, Greater, +Expr) = Equal;
               return Create_Memory_Boolean (Res);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Ne_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Ne_Slv_Slv =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Uns (Param1, Param2, Greater, +Expr);
               return Create_Memory_Boolean (Res /= Equal);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Ne_Uns_Nat =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Nat (Param1, Param2, Greater, +Expr);
               return Create_Memory_Boolean (Res /= Equal);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Ne_Nat_Uns =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Nat (Param2, Param1, Greater, +Expr);
               return Create_Memory_Boolean (Res /= Equal);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Ne_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Ne_Slv_Slv =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Sgn_Sgn (Param1, Param2, Greater, +Expr);
               return Create_Memory_Boolean (Res /= Equal);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Ne_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Signed_Ne_Slv_Int =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Sgn_Int (Param1, Param2, Greater, +Expr);
               return Create_Memory_Boolean (Res /= Equal);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Ne_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Ne_Int_Slv =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Sgn_Int (Param2, Param1, Greater, +Expr);
               return Create_Memory_Boolean (Res /= Equal);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Gt_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Gt_Slv_Slv =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Uns_Uns (Param1, Param2, Less, +Expr) = Greater;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Gt_Slv_Slv =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Sgn_Sgn (Param1, Param2, Less, +Expr) = Greater;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Nat_Uns =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Nat_Uns (Param1, Param2, Less, +Expr) = Greater;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Uns_Nat =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Uns_Nat (Param1, Param2, Less, +Expr) = Greater;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Signed_Gt_Slv_Int =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Sgn_Int (Param1, Param2, Less, +Expr) = Greater;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Gt_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Gt_Int_Slv =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Sgn_Int (Param2, Param1, Greater, +Expr) < Equal;
               return Create_Memory_Boolean (Res);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Ge_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Ge_Slv_Slv =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Uns_Uns (Param1, Param2, Less, +Expr) >= Equal;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Ge_Nat_Uns =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Nat_Uns (Param1, Param2, Less, +Expr) >= Equal;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Ge_Uns_Nat =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Uns_Nat (Param1, Param2, Less, +Expr) >= Equal;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Ge_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Ge_Slv_Slv =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Sgn_Sgn (Param1, Param2, Less, +Expr) >= Equal;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Ge_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Signed_Ge_Slv_Int =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Sgn_Int (Param1, Param2, Less, +Expr) >= Equal;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Ge_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Ge_Int_Slv =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Sgn_Int (Param2, Param1, Greater, +Expr);
               return Create_Memory_Boolean (Res <= Equal);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Le_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Le_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Le_Slv_Slv =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Uns (Param1, Param2, Greater, +Expr);
               return Create_Memory_Boolean (Res <= Equal);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Le_Uns_Nat =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Nat (Param1, Param2, Greater, +Expr);
               return Create_Memory_Boolean (Res <= Equal);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Le_Nat_Uns =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Nat_Uns (Param1, Param2, Greater, +Expr);
               return Create_Memory_Boolean (Res <= Equal);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Le_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Le_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Le_Slv_Slv =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Sgn_Sgn (Param1, Param2, Greater, +Expr);
               return Create_Memory_Boolean (Res <= Equal);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Le_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Le_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Le_Int_Slv =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Sgn_Int (Param2, Param1, Less, +Expr) >= Equal;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Le_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Arith_Le_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Signed_Le_Slv_Int =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Sgn_Int (Param1, Param2, Greater, +Expr);
               return Create_Memory_Boolean (Res <= Equal);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Lt_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Lt_Slv_Slv =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Uns_Uns (Param1, Param2, Greater, +Expr) < Equal;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Uns_Nat =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Uns_Nat (Param1, Param2, Greater, +Expr) < Equal;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Nat_Uns =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Nat_Uns (Param1, Param2, Greater, +Expr) < Equal;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Lt_Slv_Slv =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Sgn_Sgn (Param1, Param2, Greater, +Expr) < Equal;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Lt_Int_Slv =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Sgn_Int (Param2, Param1, Less, +Expr) > Equal;
               return Create_Memory_Boolean (Res);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Lt_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Signed_Lt_Slv_Int =>
            declare
               Res : Boolean;
            begin
               Res := Compare_Sgn_Int (Param1, Param2, Greater, +Expr) < Equal;
               return Create_Memory_Boolean (Res);
            end;

         when Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Uns_Sgn =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Sgn (Param1, Param2, +Expr);
               return Create_Memory_Boolean (Res < Equal);
            end;
         when Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Sgn_Uns =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Sgn (Param2, Param1, +Expr);
               return Create_Memory_Boolean (Res > Equal);
            end;
         when Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Uns_Int
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Lt_Slv_Int =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Int (Param1, Read_Discrete (Param2), +Expr);
               return Create_Memory_Boolean (Res < Equal);
            end;
         when Iir_Predefined_Ieee_Std_Logic_Arith_Lt_Int_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Lt_Int_Slv =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Int (Param2, Read_Discrete (Param1), +Expr);
               return Create_Memory_Boolean (Res > Equal);
            end;

         when Iir_Predefined_Ieee_Std_Logic_Arith_Le_Uns_Sgn =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Sgn (Param1, Param2, +Expr);
               return Create_Memory_Boolean (Res <= Equal);
            end;
         when Iir_Predefined_Ieee_Std_Logic_Arith_Le_Sgn_Uns =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Sgn (Param2, Param1, +Expr);
               return Create_Memory_Boolean (Res >= Equal);
            end;
         when Iir_Predefined_Ieee_Std_Logic_Arith_Le_Uns_Int
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Le_Slv_Int =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Int (Param1, Read_Discrete (Param2), +Expr);
               return Create_Memory_Boolean (Res <= Equal);
            end;
         when Iir_Predefined_Ieee_Std_Logic_Arith_Le_Int_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Le_Int_Slv =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Int (Param2, Read_Discrete (Param1), +Expr);
               return Create_Memory_Boolean (Res >= Equal);
            end;

         when Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Uns_Sgn =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Sgn (Param1, Param2, +Expr);
               return Create_Memory_Boolean (Res > Equal);
            end;
         when Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Sgn_Uns =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Sgn (Param2, Param1, +Expr);
               return Create_Memory_Boolean (Res < Equal);
            end;
         when Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Uns_Int
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Gt_Slv_Int =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Int (Param1, Read_Discrete (Param2), +Expr);
               return Create_Memory_Boolean (Res > Equal);
            end;
         when Iir_Predefined_Ieee_Std_Logic_Arith_Gt_Int_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Gt_Int_Slv =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Int (Param2, Read_Discrete (Param1), +Expr);
               return Create_Memory_Boolean (Res < Equal);
            end;

         when Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Uns_Sgn =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Sgn (Param1, Param2, +Expr);
               return Create_Memory_Boolean (Res >= Equal);
            end;
         when Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Sgn_Uns =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Sgn (Param2, Param1, +Expr);
               return Create_Memory_Boolean (Res <= Equal);
            end;
         when Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Uns_Int
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Ge_Slv_Int =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Int (Param1, Read_Discrete (Param2), +Expr);
               return Create_Memory_Boolean (Res >= Equal);
            end;
         when Iir_Predefined_Ieee_Std_Logic_Arith_Ge_Int_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Ge_Int_Slv =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Int (Param2, Read_Discrete (Param1), +Expr);
               return Create_Memory_Boolean (Res <= Equal);
            end;

         when Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Uns_Sgn =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Sgn (Param1, Param2, +Expr);
               return Create_Memory_Boolean (Res = Equal);
            end;
         when Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Sgn_Uns =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Sgn (Param2, Param1, +Expr);
               return Create_Memory_Boolean (Res = Equal);
            end;
         when Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Uns_Int
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Eq_Slv_Int =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Int (Param1, Read_Discrete (Param2), +Expr);
               return Create_Memory_Boolean (Res = Equal);
            end;
         when Iir_Predefined_Ieee_Std_Logic_Arith_Eq_Int_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Eq_Int_Slv =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Int (Param2, Read_Discrete (Param1), +Expr);
               return Create_Memory_Boolean (Res = Equal);
            end;

         when Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Uns_Sgn =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Sgn (Param1, Param2, +Expr);
               return Create_Memory_Boolean (Res /= Equal);
            end;
         when Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Sgn_Uns =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Sgn (Param2, Param1, +Expr);
               return Create_Memory_Boolean (Res /= Equal);
            end;
         when Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Uns_Int
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Ne_Slv_Int =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Int (Param1, Read_Discrete (Param2), +Expr);
               return Create_Memory_Boolean (Res /= Equal);
            end;
         when Iir_Predefined_Ieee_Std_Logic_Arith_Ne_Int_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Ne_Int_Slv =>
            declare
               Res : Order_Type;
            begin
               Res := Compare_Uns_Int (Param2, Read_Discrete (Param1), +Expr);
               return Create_Memory_Boolean (Res /= Equal);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Slv_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Uns_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_Add_Slv_Slv =>
            return Add_Uns_Uns (Param1, Param2, +Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Log
           | Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Slv_Log =>
            return Add_Uns_Uns (Param1, Log_To_Vec (Param2, Param1), +Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Add_Log_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Log_Slv =>
            return Add_Uns_Uns (Log_To_Vec (Param1, Param2), Param2, +Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Nat
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Slv_Int
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_Add_Slv_Nat =>
            return Add_Uns_Nat
              (Param1, To_Uns64 (Read_Discrete (Param2)), +Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Add_Nat_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_Add_Nat_Slv
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Add_Int_Slv =>
            return Add_Uns_Nat
              (Param2, To_Uns64 (Read_Discrete (Param1)), +Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Sgn_Slv
            | Iir_Predefined_Ieee_Std_Logic_Signed_Add_Slv_Slv =>
            return Add_Sgn_Sgn (Param1, Param2, +Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Signed_Add_Slv_Int =>
            return Add_Sgn_Int (Param1, Read_Discrete (Param2), +Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Add_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Add_Int_Slv =>
            return Add_Sgn_Int (Param2, Read_Discrete (Param1), +Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Log
            | Iir_Predefined_Ieee_Std_Logic_Signed_Add_Slv_Log =>
            return Add_Sgn_Sgn (Param1, Log_To_Vec (Param2, Param1), +Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Add_Log_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Add_Log_Slv =>
            return Add_Sgn_Sgn (Log_To_Vec (Param1, Param2), Param2, +Expr);

         --  std_logic_arith."+"
         when Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Sgn_Slv =>
            return Add_Uns_Sgn_Sgn (Param1, Param2, +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Uns_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Uns_Slv =>
            return Add_Sgn_Uns_Sgn (Param1, Param2, +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Int_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Int_Slv =>
            return Add_Uns_Int_Uns (Param1, Read_Discrete (Param2), +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Add_Int_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Int_Uns_Slv =>
            return Add_Uns_Int_Uns (Param2, Read_Discrete (Param1), +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Int_Slv =>
            return Add_Sgn_Int_Sgn (Param1, Read_Discrete (Param2), +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Add_Int_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Int_Sgn_Slv =>
            return Add_Sgn_Int_Sgn (Param2, Read_Discrete (Param1), +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Log_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Uns_Log_Slv =>
            return Add_Uns_Log_Uns (Param1, Param2, +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Add_Log_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Log_Uns_Slv =>
            return Add_Uns_Log_Uns (Param2, Param1, +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Log_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Sgn_Log_Slv =>
            return Add_Sgn_Log_Sgn (Param1, Param2, +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Add_Log_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Add_Log_Sgn_Slv =>
            return Add_Sgn_Log_Sgn (Param2, Param1, +Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_Sub_Slv_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Uns_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Sub_Slv_Slv =>
            return Sub_Uns_Uns (Param1, Param2, +Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Nat
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_Sub_Slv_Nat
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Sub_Slv_Int =>
            return Sub_Uns_Nat
              (Param1, To_Uns64 (Read_Discrete (Param2)), +Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Nat_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_Sub_Nat_Slv
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Sub_Int_Slv =>
            return Sub_Nat_Uns
              (To_Uns64 (Read_Discrete (Param1)), Param2, +Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Log
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Sub_Slv_Log =>
            return Sub_Uns_Uns (Param1, Log_To_Vec (Param2, Param1), +Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Log_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Sub_Log_Slv =>
            return Sub_Uns_Uns (Log_To_Vec (Param1, Param2), Param2, +Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Sub_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Sgn_Slv
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Sub_Slv_Slv =>
            return Sub_Sgn_Sgn (Param1, Param2, +Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Sgn_Int
            | Iir_Predefined_Ieee_Std_Logic_Signed_Sub_Slv_Int =>
            return Sub_Sgn_Int (Param1, Read_Discrete (Param2), +Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Sub_Int_Slv =>
            return Sub_Int_Sgn (Read_Discrete (Param1), Param2, +Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Sub_Sgn_Log
            | Iir_Predefined_Ieee_Std_Logic_Signed_Sub_Slv_Log =>
            return Sub_Sgn_Sgn (Param1, Log_To_Vec (Param2, Param1), +Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Sub_Log_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Sub_Log_Slv =>
            return Sub_Sgn_Sgn (Log_To_Vec (Param1, Param2), Param2, +Expr);

         --  std_logic_arith."-"
         when Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Sgn_Slv =>
            return Sub_Uns_Sgn_Sgn (Param1, Param2, +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Uns_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Uns_Slv =>
            return Sub_Sgn_Uns_Sgn (Param1, Param2, +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Int_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Int_Slv =>
            return Sub_Uns_Int_Uns (Param1, Read_Discrete (Param2), +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Int_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Int_Uns_Slv =>
            return Sub_Int_Uns_Uns (Read_Discrete (Param1), Param2, +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Int_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Int_Slv =>
            return Sub_Sgn_Int_Sgn (Param1, Read_Discrete (Param2), +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Int_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Int_Sgn_Slv =>
            return Sub_Int_Sgn_Sgn (Read_Discrete (Param1), Param2, +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Log_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Uns_Log_Slv =>
            return Sub_Uns_Log_Uns (Param1, Param2, +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Log_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Log_Uns_Slv =>
            return Sub_Log_Uns_Uns (Param1, Param2, +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Log_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Sgn_Log_Slv =>
            return Sub_Sgn_Log_Sgn (Param1, Param2, +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Log_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Sub_Log_Sgn_Slv =>
            return Sub_Log_Sgn_Sgn (Param1, Param2, +Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Mul_Uns_Uns =>
            return Mul_Uns_Uns (Param1, Param2, +Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Mul_Nat_Uns =>
            return Mul_Nat_Uns
              (To_Uns64 (Read_Discrete (Param1)), Param2, +Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Mul_Uns_Nat =>
            return Mul_Uns_Nat
              (Param1, To_Uns64 (Read_Discrete (Param2)), +Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Mul_Sgn_Sgn =>
            return Mul_Sgn_Sgn (Param1, Param2, +Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Mul_Sgn_Int =>
            return Mul_Sgn_Int (Param1, Read_Discrete (Param2), +Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Mul_Int_Sgn =>
            return Mul_Int_Sgn (Read_Discrete (Param1), Param2, +Expr);

         --  std_logic_arith."*"
         when Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Uns_Uns_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Uns_Uns_Slv
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Mul_Slv_Slv =>
            return Mul_Uns_Uns_Uns (Param1, Param2, +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Sgn_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Sgn_Sgn_Slv
            | Iir_Predefined_Ieee_Std_Logic_Signed_Mul_Slv_Slv =>
            return Mul_Sgn_Sgn_Sgn (Param1, Param2, +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Uns_Sgn_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Uns_Sgn_Slv =>
            return Mul_Uns_Sgn_Sgn (Param1, Param2, +Expr);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Sgn_Uns_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Mul_Sgn_Uns_Slv =>
            return Mul_Sgn_Uns_Sgn (Param1, Param2, +Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Div_Uns_Uns =>
            return Div_Uns_Uns (Inst, Param1, Param2, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Div_Uns_Nat =>
            return Div_Uns_Nat
              (Inst, Param1, To_Uns64 (Read_Discrete (Param2)), Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Div_Nat_Uns =>
            return Div_Nat_Uns
              (Inst, To_Uns64 (Read_Discrete (Param1)), Param2, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Div_Sgn_Sgn =>
            return Div_Sgn_Sgn (Inst, Param1, Param2, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Div_Int_Sgn =>
            return Div_Int_Sgn (Inst, Read_Discrete (Param1), Param2, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Div_Sgn_Int =>
            return Div_Sgn_Int (Inst, Param1, Read_Discrete (Param2), Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Rem_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Mod_Uns_Uns =>
            return Rem_Uns_Uns (Inst, Param1, Param2, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Rem_Uns_Nat
            | Iir_Predefined_Ieee_Numeric_Std_Mod_Uns_Nat =>
            return Rem_Uns_Nat
              (Inst, Param1, To_Uns64 (Read_Discrete (Param2)), Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Rem_Nat_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Mod_Nat_Uns =>
            return Rem_Nat_Uns
              (Inst, To_Uns64 (Read_Discrete (Param1)), Param2, Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Rem_Sgn_Sgn =>
            return Rem_Sgn_Sgn (Inst, Param1, Param2, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Rem_Int_Sgn =>
            return Rem_Int_Sgn (Inst, Read_Discrete (Param1), Param2, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Rem_Sgn_Int =>
            return Rem_Sgn_Int (Inst, Param1, Read_Discrete (Param2), Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Mod_Sgn_Sgn =>
            return Mod_Sgn_Sgn (Inst, Param1, Param2, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Mod_Int_Sgn =>
            return Mod_Int_Sgn (Inst, Read_Discrete (Param1), Param2, Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Mod_Sgn_Int =>
            return Mod_Sgn_Int (Inst, Param1, Read_Discrete (Param2), Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Srl_Uns_Int
           |  Iir_Predefined_Ieee_Numeric_Std_Srl_Sgn_Int =>
            declare
               Amt : Int64;
            begin
               Amt := Read_Discrete (Param2);
               if Amt >= 0 then
                  return Shift_Vec (Param1, Uns32 (Amt), True, False);
               else
                  return Shift_Vec (Param1, Uns32 (-Amt), False, False);
               end if;
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Sll_Uns_Int
           |  Iir_Predefined_Ieee_Numeric_Std_Sll_Sgn_Int =>
            declare
               Amt : Int64;
            begin
               Amt := Read_Discrete (Param2);
               if Amt >= 0 then
                  return Shift_Vec (Param1, Uns32 (Amt), False, False);
               else
                  return Shift_Vec (Param1, Uns32 (-Amt), True, False);
               end if;
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Uns_Uns =>
            declare
               Res : Std_Ulogic;
            begin
               Res := Match_Eq_Vec_Vec (Param1, Param2, False, +Expr);
               return Create_Memory_U8 (Std_Ulogic'Pos (Res), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Uns_Uns =>
            declare
               Res : Std_Ulogic;
            begin
               Res := Match_Eq_Vec_Vec (Param1, Param2, False, +Expr);
               Res := Not_Table (Res);
               return Create_Memory_U8 (Std_Ulogic'Pos (Res), Res_Typ);
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Uns_Uns =>
            return Match_Cmp_Vec_Vec (Param1, Param2, Map_Lt, False, +Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Sgn_Sgn =>
            return Match_Cmp_Vec_Vec (Param1, Param2, Map_Lt, True, +Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Match_Le_Uns_Uns =>
            return Match_Cmp_Vec_Vec (Param1, Param2, Map_Le, False, +Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Match_Le_Sgn_Sgn =>
            return Match_Cmp_Vec_Vec (Param1, Param2, Map_Le, True, +Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Uns_Uns =>
            return Match_Cmp_Vec_Vec (Param1, Param2, Map_Gt, False, +Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Sgn_Sgn =>
            return Match_Cmp_Vec_Vec (Param1, Param2, Map_Gt, True, +Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Uns_Uns =>
            return Match_Cmp_Vec_Vec (Param1, Param2, Map_Ge, False, +Expr);
         when Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Sgn_Sgn =>
            return Match_Cmp_Vec_Vec (Param1, Param2, Map_Ge, True, +Expr);

         when Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Sgn_Sgn =>
            declare
               Res : Std_Ulogic;
            begin
               Res := Match_Eq_Vec_Vec (Param1, Param2, True, +Expr);
               return Create_Memory_U8 (Std_Ulogic'Pos (Res), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Sgn_Sgn =>
            declare
               Res : Std_Ulogic;
            begin
               Res := Match_Eq_Vec_Vec (Param1, Param2, True, +Expr);
               Res := Not_Table (Res);
               return Create_Memory_U8 (Std_Ulogic'Pos (Res), Res_Typ);
            end;

         when Iir_Predefined_Ieee_Math_Real_Pow_Real_Real =>
            declare
               function Pow (L, R : Fp64) return Fp64;
               pragma Import (C, Pow);
            begin
               return Create_Memory_Fp64
                 (Pow (Read_Fp64 (Param1), Read_Fp64 (Param2)), Res_Typ);
            end;

         when Iir_Predefined_Ieee_Math_Real_Mod =>
            declare
               function Fmod (L, R : Fp64) return Fp64;
               pragma Import (C, Fmod);
            begin
               return Create_Memory_Fp64
                 (Fmod (Read_Fp64 (Param1), Read_Fp64 (Param2)), Res_Typ);
            end;

         when Iir_Predefined_Physical_Minimum
           | Iir_Predefined_Integer_Minimum
           | Iir_Predefined_Enum_Minimum =>
            return Create_Memory_Discrete
              (Int64'Min (Read_Discrete (Param1), Read_Discrete (Param2)),
               Res_Typ);
         when Iir_Predefined_Floating_Maximum =>
            return Create_Memory_Fp64
              (Fp64'Max (Read_Fp64 (Param1), Read_Fp64 (Param2)), Res_Typ);
         when Iir_Predefined_Physical_Maximum
           | Iir_Predefined_Integer_Maximum
           | Iir_Predefined_Enum_Maximum =>
            return Create_Memory_Discrete
              (Int64'Max (Read_Discrete (Param1), Read_Discrete (Param2)),
               Res_Typ);
         when Iir_Predefined_Floating_Minimum =>
            return Create_Memory_Fp64
              (Fp64'Min (Read_Fp64 (Param1), Read_Fp64 (Param2)), Res_Typ);

         when Iir_Predefined_Now_Function =>
            return Create_Memory_Discrete
              (Int64 (Grt.Vhdl_Types.Current_Time), Res_Typ);
         when Iir_Predefined_Real_Now_Function =>
            return Create_Memory_Fp64
              (Fp64 (Grt.Vhdl_Types.Current_Time_AMS), Res_Typ);

         when Iir_Predefined_Integer_To_String =>
            declare
               Str : String (1 .. 21);
               First : Natural;
            begin
               Grt.To_Strings.To_String
                 (Str, First, Ghdl_I64 (Read_Discrete (Param1)));
               return String_To_Memtyp (Str (First .. Str'Last), Res_Typ);
            end;
         when Iir_Predefined_Enum_To_String =>
            return Eval_Enum_To_String (Param1, Res_Typ, Imp);
         when Iir_Predefined_Floating_To_String =>
            declare
               Str : String (1 .. 24);
               Last : Natural;
            begin
               Grt.To_Strings.To_String
                 (Str, Last, Ghdl_F64 (Read_Fp64 (Param1)));
               return String_To_Memtyp (Str (Str'First .. Last), Res_Typ);
            end;
         when Iir_Predefined_Real_To_String_Digits =>
            declare
               Str : Grt.To_Strings.String_Real_Format;
               Last : Natural;
               Val : Ghdl_F64;
               Dig : Ghdl_I32;
            begin
               Val := Ghdl_F64 (Read_Fp64 (Param1));
               Dig := Ghdl_I32 (Read_Discrete (Param2));
               Grt.To_Strings.To_String (Str, Last, Val, Dig);
               return String_To_Memtyp (Str (Str'First .. Last), Res_Typ);
            end;
         when Iir_Predefined_Real_To_String_Format =>
            declare
               Format : String (1 .. Natural (Param2.Typ.Abound.Len) + 1);
               Str : Grt.To_Strings.String_Real_Format;
               Last : Natural;
            begin
               --  Copy format
               for I in 1 .. Param2.Typ.Abound.Len loop
                  Format (Positive (I)) := Character'Val
                    (Read_U8 (Param2.Mem + Size_Type (I - 1)));
               end loop;
               Format (Format'Last) := ASCII.NUL;
               Grt.To_Strings.To_String
                 (Str, Last, Ghdl_F64 (Read_Fp64 (Param1)),
                  To_Ghdl_C_String (Format'Address));
               return String_To_Memtyp (Str (Str'First .. Last), Res_Typ);
            end;

         when Iir_Predefined_Physical_To_String =>
            declare
               Phys_Type : constant Node :=
                 Get_Type (Get_Interface_Declaration_Chain (Imp));
               Id : constant Name_Id :=
                 Get_Identifier (Get_Primary_Unit (Phys_Type));
               Str : String (1 .. 21);
               First : Natural;
            begin
               Grt.To_Strings.To_String
                 (Str, First, Ghdl_I64 (Read_Discrete (Param1)));
               return String_To_Memtyp
                 (Str (First .. Str'Last) & ' ' & Name_Table.Image (Id),
                  Res_Typ);
            end;
         when Iir_Predefined_Time_To_String_Unit =>
            declare
               Time_Type : constant Node :=
                 Get_Type (Get_Interface_Declaration_Chain (Imp));
               Str : Grt.To_Strings.String_Time_Unit;
               First : Natural;
               Unit : Iir;
               Uval : Int64;
            begin
               Uval := Read_Discrete (Param2);
               Unit := Get_Unit_Chain (Time_Type);
               while Unit /= Null_Iir loop
                  exit when Vhdl.Evaluation.Get_Physical_Value (Unit) = Uval;
                  Unit := Get_Chain (Unit);
               end loop;
               if Unit = Null_Iir then
                  Error_Msg_Synth
                    (Inst, Expr, "to_string for time called with wrong unit");
               end if;
               Grt.To_Strings.To_String (Str, First,
                                         Ghdl_I64 (Read_Discrete (Param1)),
                                         Ghdl_I64 (Uval));
               return String_To_Memtyp
                 (Str (First .. Str'Last) & ' '
                    & Name_Table.Image (Get_Identifier (Unit)),
                 Res_Typ);
            end;

         when Iir_Predefined_Array_Char_To_String =>
            return Eval_Array_Char_To_String (Param1, Res_Typ, Imp);

         when Iir_Predefined_Bit_Vector_To_Hstring =>
            return Eval_Bit_Vector_To_String (Param1, Res_Typ, 4);
         when Iir_Predefined_Bit_Vector_To_Ostring =>
            return Eval_Bit_Vector_To_String (Param1, Res_Typ, 3);

         when Iir_Predefined_Std_Env_Resolution_Limit =>
            return Create_Memory_Discrete (1, Res_Typ);

         when Iir_Predefined_Ieee_Numeric_Bit_Touns_Nat_Nat_Uns =>
            return Eval_To_Bit_Vector
              (Uns64 (Read_Discrete (Param1)), Read_Discrete (Param2),
               Res_Typ);

         when Iir_Predefined_Ieee_Numeric_Std_Touns_Nat_Nat_Uns
            | Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Unsigned_Int
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_To_Slv_Nat_Nat
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_To_Suv_Nat_Nat =>
            return Eval_To_Log_Vector
              (Uns64 (Read_Discrete (Param1)), Read_Discrete (Param2),
               Res_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Touns_Nat_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_To_Slv_Nat_Slv
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_To_Suv_Nat_Suv =>
            return Eval_To_Log_Vector
              (Uns64 (Read_Discrete (Param1)), Int64 (Param2.Typ.Abound.Len),
               Res_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Tosgn_Int_Nat_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Vector_Int =>
            return Eval_To_Log_Vector
              (To_Uns64 (Read_Discrete (Param1)), Read_Discrete (Param2),
               Res_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Tosgn_Int_Sgn_Sgn =>
            return Eval_To_Log_Vector
              (To_Uns64 (Read_Discrete (Param1)),
               Int64 (Param2.Typ.Abound.Len),
               Res_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Toint_Uns_Nat
            | Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Integer_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Conv_Integer
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_To_Integer_Slv_Nat =>
            --  UNSIGNED to Natural.
            return Create_Memory_Discrete
              (Eval_Unsigned_To_Integer (Param1, Expr), Res_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Toint_Sgn_Int =>
            --  SIGNED to Integer
            return Create_Memory_Discrete
              (Eval_Signed_To_Integer (Param1, Expr), Res_Typ);
         when Iir_Predefined_Ieee_Std_Logic_Arith_Conv_Integer_Int =>
            return Param1;

         when Iir_Predefined_Ieee_Numeric_Std_Shf_Left_Uns_Nat
            | Iir_Predefined_Ieee_Numeric_Std_Shf_Left_Sgn_Nat
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_Shift_Left =>
            return Shift_Vec (Param1, Uns32 (Read_Discrete (Param2)),
                              False, False);
         when Iir_Predefined_Ieee_Numeric_Std_Shf_Right_Uns_Nat
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_Shift_Right =>
            return Shift_Vec (Param1, Uns32 (Read_Discrete (Param2)),
                              True, False);
         when Iir_Predefined_Ieee_Numeric_Std_Shf_Right_Sgn_Nat =>
            return Shift_Vec (Param1, Uns32 (Read_Discrete (Param2)),
                              True, True);
         when Iir_Predefined_Ieee_Numeric_Std_Rot_Left_Uns_Nat
            | Iir_Predefined_Ieee_Numeric_Std_Rot_Left_Sgn_Nat
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_Rotate_Left =>
            return Rotate_Vec (Param1, Uns32 (Read_Discrete (Param2)), False);
         when Iir_Predefined_Ieee_Numeric_Std_Rot_Right_Uns_Nat
            | Iir_Predefined_Ieee_Numeric_Std_Rot_Right_Sgn_Nat
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_Rotate_Right =>
            return Rotate_Vec (Param1, Uns32 (Read_Discrete (Param2)), True);

         when Iir_Predefined_Ieee_Numeric_Std_Resize_Uns_Nat
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_Resize_Slv_Nat =>
            return Resize_Vec (Param1, Uns32 (Read_Discrete (Param2)), False);
         when Iir_Predefined_Ieee_Numeric_Std_Resize_Uns_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_Resize_Slv_Slv =>
            return Resize_Vec (Param1, Param2.Typ.Abound.Len, False);
         when Iir_Predefined_Ieee_Numeric_Std_Resize_Sgn_Nat =>
            return Resize_Vec (Param1, Uns32 (Read_Discrete (Param2)), True);
         when Iir_Predefined_Ieee_Numeric_Std_Resize_Sgn_Sgn =>
            return Resize_Vec (Param1, Param2.Typ.Abound.Len, True);

         when Iir_Predefined_Ieee_Std_Logic_Arith_Ext =>
            declare
               Len : Int64;
            begin
               Len := Read_Discrete (Param2);
               if Len < 0 then
                  Len := 0;
               end if;
               return Resize_Vec (Param1, Uns32 (Len), False);
            end;
         when Iir_Predefined_Ieee_Std_Logic_Arith_Sxt =>
            declare
               Len : Int64;
            begin
               Len := Read_Discrete (Param2);
               if Len < 0 then
                  Len := 0;
               end if;
               return Resize_Vec (Param1, Uns32 (Len), True);
            end;

         when Iir_Predefined_Ieee_1164_To_Stdulogic =>
            declare
               B : Std_Ulogic;
            begin
               B := Read_Bit_To_Std_Logic (Param1.Mem, 0);
               return Create_Memory_U8 (Std_Ulogic'Pos (B), Res_Typ);
            end;

         when Iir_Predefined_Ieee_1164_To_X01_Log =>
            declare
               B : Std_Ulogic;
            begin
               B := Read_Std_Logic (Param1.Mem, 0);
               B := To_X01 (B);
               return Create_Memory_U8 (Std_Ulogic'Pos (B), Res_Typ);
            end;
         when Iir_Predefined_Ieee_1164_To_X01Z_Log =>
            declare
               B : Std_Ulogic;
            begin
               B := Read_Std_Logic (Param1.Mem, 0);
               B := Map_X01Z (B);
               return Create_Memory_U8 (Std_Ulogic'Pos (B), Res_Typ);
            end;
         when Iir_Predefined_Ieee_1164_To_X01_Slv
            | Iir_Predefined_Ieee_Numeric_Std_To_X01_Uns
            | Iir_Predefined_Ieee_Numeric_Std_To_X01_Sgn =>
            return Eval_To_X01 (Param1, Map_X01);
         when Iir_Predefined_Ieee_Numeric_Std_To_X01Z_Uns
            | Iir_Predefined_Ieee_Numeric_Std_To_X01Z_Sgn
            | Iir_Predefined_Ieee_1164_To_X01Z_Slv =>
            return Eval_To_X01 (Param1, Map_X01Z);
         when Iir_Predefined_Ieee_Numeric_Std_To_UX01_Uns
            | Iir_Predefined_Ieee_Numeric_Std_To_UX01_Sgn
            | Iir_Predefined_Ieee_1164_To_UX01_Slv =>
            return Eval_To_X01 (Param1, Map_UX01);

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
                  B := Read_Bit_To_Std_Logic (Param1.Mem, I - 1);
                  Write_Std_Logic (Res.Mem, I - 1, B);
               end loop;
               return Res;
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Match_Log =>
            return Create_Memory_Boolean
              (Match_Eq_Table (Read_Std_Logic (Param1.Mem, 0),
                               Read_Std_Logic (Param2.Mem, 0)) = '1');

         when Iir_Predefined_Ieee_Numeric_Std_Match_Suv
            | Iir_Predefined_Ieee_Numeric_Std_Match_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Match_Sgn =>
            return Create_Memory_Boolean (Match_Vec (Param1, Param2, +Expr));

         when Iir_Predefined_Ieee_1164_To_Bit =>
            declare
               V : Std_Ulogic;
               X : Bit;
               R : Bit;
            begin
               V := Read_Std_Logic (Param1.Mem, 0);
               X := Read_Bit (Param2.Mem, 0);
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
               X := Read_Bit (Param2.Mem, 0);
               Bnd := Create_Vec_Type_By_Length
                 (Uns32 (Vec_Length (Param1.Typ)), El_Type);
               Res := Create_Memory (Bnd);
               for I in 1 .. Uns32 (Vec_Length (Param1.Typ)) loop
                  S := Read_Std_Logic (Param1.Mem, I - 1);
                  R := To_Bit (S, X);
                  Write_Bit (Res.Mem, I - 1, R);
               end loop;
               return Res;
            end;

         when Iir_Predefined_Ieee_1164_To_01_Slv_Log
            | Iir_Predefined_Ieee_Numeric_Std_To_01_Uns =>
            declare
               Len : constant Uns32 := Param1.Typ.Abound.Len;
               S : Std_Ulogic;
               Xmap : Std_Ulogic;
               Res : Memtyp;
            begin
               Xmap := Read_Std_Logic (Param2.Mem, 0);
               Res := Create_Memory (Create_Res_Bound (Param1.Typ));
               for I in 1 .. Len loop
                  S := Read_Std_Logic (Param1.Mem, I - 1);
                  S := To_X01 (S);
                  if S = 'X' then
                     S := Xmap;
                  end if;
                  Write_Std_Logic (Res.Mem, I - 1, S);
               end loop;
               return Res;
            end;

         when Iir_Predefined_Ieee_1164_Is_X_Log =>
            declare
               B : Std_Ulogic;
            begin
               B := Read_Std_Logic (Param1.Mem, 0);
               B := To_X01 (B);
               return Create_Memory_Boolean (B = 'X');
            end;

         when Iir_Predefined_Ieee_Numeric_Std_Is_X_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Is_X_Sgn
            | Iir_Predefined_Ieee_1164_Is_X_Slv =>
            declare
               Len : constant Uns32 := Param1.Typ.Abound.Len;
               Res : Boolean;
               B : Std_Ulogic;
            begin
               Res := False;
               for I in 1 .. Len loop
                  B := Read_Std_Logic (Param1.Mem, I - 1);
                  if To_X01 (B) = 'X' then
                     Res := True;
                     exit;
                  end if;
               end loop;
               return Create_Memory_Boolean (Res);
            end;

         when Iir_Predefined_Ieee_1164_To_Stdlogicvector_Suv
           | Iir_Predefined_Ieee_1164_To_Stdulogicvector_Slv =>
            --  TODO
            return (Param1.Typ, Param1.Mem);

         when Iir_Predefined_Ieee_1164_To_Hstring
            | Iir_Predefined_Ieee_Numeric_Std_To_Hstring_Uns =>
            return Eval_Logic_Vector_To_String (Param1, Res_Typ, False, 4);
         when Iir_Predefined_Ieee_Numeric_Std_To_Hstring_Sgn =>
            return Eval_Logic_Vector_To_String (Param1, Res_Typ, True, 4);
         when Iir_Predefined_Ieee_1164_To_Ostring
            | Iir_Predefined_Ieee_Numeric_Std_To_Ostring_Uns =>
            return Eval_Logic_Vector_To_String (Param1, Res_Typ, False, 3);
         when Iir_Predefined_Ieee_Numeric_Std_To_Ostring_Sgn =>
            return Eval_Logic_Vector_To_String (Param1, Res_Typ, True, 3);

         when Iir_Predefined_Ieee_Numeric_Std_Max_Uns_Uns =>
            return Minmax (Param1, Param2, False, True);
         when Iir_Predefined_Ieee_Numeric_Std_Min_Uns_Uns =>
            return Minmax (Param1, Param2, False, False);
         when Iir_Predefined_Ieee_Numeric_Std_Max_Sgn_Sgn =>
            return Minmax (Param1, Param2, True, True);
         when Iir_Predefined_Ieee_Numeric_Std_Min_Sgn_Sgn =>
            return Minmax (Param1, Param2, True, False);

         when Iir_Predefined_Ieee_Numeric_Std_Find_Rightmost_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Find_Rightmost_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_Find_Rightmost =>
            return Create_Memory_Discrete
              (Int64 (Find_Rightmost (Param1, Param2)), Res_Typ);
         when Iir_Predefined_Ieee_Numeric_Std_Find_Leftmost_Uns
            | Iir_Predefined_Ieee_Numeric_Std_Find_Leftmost_Sgn
            | Iir_Predefined_Ieee_Numeric_Std_Unsigned_Find_Leftmost =>
            return Create_Memory_Discrete
              (Int64 (Find_Leftmost (Param1, Param2)), Res_Typ);

         when Iir_Predefined_Ieee_Numeric_Std_Unsigned_Maximum_Slv_Slv =>
            return Minmax (Param1, Param2, False, True);
         when Iir_Predefined_Ieee_Numeric_Std_Unsigned_Minimum_Slv_Slv =>
            return Minmax (Param1, Param2, False, False);

         when Iir_Predefined_Ieee_Std_Logic_Arith_Shl_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Shl =>
            return Execute_Shift_Operator
              (Param1, Read_Discrete (Param2),
               Std_Ulogic'Pos('0'), Iir_Predefined_Array_Sll);

         when Iir_Predefined_Ieee_Std_Logic_Arith_Shl_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Shl =>
            return Execute_Shift_Operator
              (Param1, Read_Discrete (Param2),
               Std_Ulogic'Pos('0'), Iir_Predefined_Array_Sla);

         when Iir_Predefined_Ieee_Std_Logic_Arith_Shr_Uns
            | Iir_Predefined_Ieee_Std_Logic_Unsigned_Shr =>
            return Execute_Shift_Operator
              (Param1, Read_Discrete (Param2),
               Std_Ulogic'Pos('0'), Iir_Predefined_Array_Srl);

         when Iir_Predefined_Ieee_Std_Logic_Arith_Shr_Sgn
            | Iir_Predefined_Ieee_Std_Logic_Signed_Shr =>
            return Execute_Shift_Operator
              (Param1, Read_Discrete (Param2),
               Std_Ulogic'Pos('0'), Iir_Predefined_Array_Sra);

         when Iir_Predefined_Ieee_Std_Logic_Misc_And_Reduce_Slv
            | Iir_Predefined_Ieee_Std_Logic_Misc_And_Reduce_Suv =>
            return Eval_Vector_Reduce ('1', Param1, And_Table, False);
         when Iir_Predefined_Ieee_Std_Logic_Misc_Nand_Reduce_Slv
           | Iir_Predefined_Ieee_Std_Logic_Misc_Nand_Reduce_Suv =>
            return Eval_Vector_Reduce ('1', Param1, And_Table, True);

         when Iir_Predefined_Ieee_Std_Logic_Misc_Or_Reduce_Slv
           | Iir_Predefined_Ieee_Std_Logic_Misc_Or_Reduce_Suv =>
            return Eval_Vector_Reduce ('0', Param1, Or_Table, False);
         when Iir_Predefined_Ieee_Std_Logic_Misc_Nor_Reduce_Slv
            | Iir_Predefined_Ieee_Std_Logic_Misc_Nor_Reduce_Suv =>
            return Eval_Vector_Reduce ('0', Param1, Or_Table, True);

         when Iir_Predefined_Ieee_Std_Logic_Misc_Xor_Reduce_Slv
            | Iir_Predefined_Ieee_Std_Logic_Misc_Xor_Reduce_Suv =>
            return Eval_Vector_Reduce ('0', Param1, Xor_Table, False);
         when Iir_Predefined_Ieee_Std_Logic_Misc_Xnor_Reduce_Slv
            | Iir_Predefined_Ieee_Std_Logic_Misc_Xnor_Reduce_Suv =>
            return Eval_Vector_Reduce ('0', Param1, Xor_Table, True);

         when Iir_Predefined_Ieee_Math_Real_Sign =>
            declare
               Val : constant Fp64 := Read_Fp64 (Param1);
               Res : Fp64;
            begin
               if Val > 0.0 then
                  Res := 1.0;
               elsif Val < 0.0 then
                  Res := -1.0;
               else
                  Res := 0.0;
               end if;
               return Create_Memory_Fp64 (Res, Res_Typ);
            end;
         when Iir_Predefined_Ieee_Math_Real_Sqrt =>
            declare
               function Sqrt (Arg : Fp64) return Fp64;
               pragma Import (C, Sqrt);
            begin
               return Create_Memory_Fp64 (Sqrt (Read_Fp64 (Param1)), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Math_Real_Log2 =>
            declare
               function Log2 (Arg : Fp64) return Fp64;
               pragma Import (C, Log2);
            begin
               return Create_Memory_Fp64 (Log2 (Read_Fp64 (Param1)), Res_Typ);
            end;
         when Iir_Predefined_Ieee_Math_Real_Log10 =>
            declare
               function Log10 (Arg : Fp64) return Fp64;
               pragma Import (C, Log10);
            begin
               return Create_Memory_Fp64 (Log10 (Read_Fp64 (Param1)), Res_Typ);
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

         when Iir_Predefined_Foreign_Textio_Read_Real =>
            declare
               Len : constant Natural := Natural (Param1.Typ.Abound.Len);
               Res : Fp64;
               Cs : Ghdl_C_String;
            begin
               Cs := To_Ghdl_C_String (To_Address (Param1.Mem));
               Res := Fp64 (Grt.Fcvt.From_String (Cs (1 .. Len)));
               return Create_Memory_Fp64 (Res, Res_Typ);
            end;
         when others =>
            Error_Msg_Synth (Inst, Expr, "unhandled (static) function: "
                               & Iir_Predefined_Functions'Image (Def));
            return Null_Memtyp;
      end case;
   end Eval_Static_Predefined_Function_Call;
end Synth.Vhdl_Eval;
