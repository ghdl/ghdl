--  numeric_std
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

with Types_Utils; use Types_Utils;
with Synth.Errors; use Synth.Errors;
with Synth.Ieee.Std_Logic_1164; use Synth.Ieee.Std_Logic_1164;

package body Synth.Ieee.Numeric_Std is
   subtype Sl_01 is Std_Ulogic range '0' .. '1';
   subtype Sl_X01 is Std_Ulogic range 'X' .. '1';

   type Carry_Array is array (Sl_01, Sl_01, Sl_01) of Sl_01;
   Compute_Carry : constant Carry_Array :=
     ('0' => ('0' => ('0' => '0', '1' => '0'),
              '1' => ('0' => '0', '1' => '1')),
      '1' => ('0' => ('0' => '0', '1' => '1'),
              '1' => ('0' => '1', '1' => '1')));
   Compute_Sum : constant Carry_Array :=
     ('0' => ('0' => ('0' => '0', '1' => '1'),
              '1' => ('0' => '1', '1' => '0')),
      '1' => ('0' => ('0' => '1', '1' => '0'),
              '1' => ('0' => '0', '1' => '1')));

   type Sl_To_X01_Array is array (Std_Ulogic) of Sl_X01;
   Sl_To_X01 : constant Sl_To_X01_Array :=
     ('0' | 'L' => '0', '1' | 'H' => '1', others => 'X');

   type Uns_To_01_Array is array (Uns64 range 0 .. 1) of Sl_X01;
   Uns_To_01 : constant Uns_To_01_Array := (0 => '0', 1 => '1');

   function Create_Res_Type (Otyp : Type_Acc; Len : Uns32) return Type_Acc is
   begin
      if Otyp.Vbound.Len = Len
        and then Otyp.Vbound.Right = 0
        and then Otyp.Vbound.Dir = Dir_Downto
      then
         pragma Assert (Otyp.Vbound.Left = Int32 (Len) - 1);
         return Otyp;
      end if;
      return Create_Vec_Type_By_Length (Len, Otyp.Vec_El);
   end Create_Res_Type;

   procedure Fill (Res : Memtyp; V : Std_Ulogic) is
   begin
      for I in 1 .. Res.Typ.Vbound.Len loop
         Write_Std_Logic (Res.Mem, I - 1, V);
      end loop;
   end Fill;

      procedure Warn_Compare_Null (Loc : Syn_Src) is
   begin
      Warning_Msg_Synth (+Loc, "null argument detected, returning false");
   end Warn_Compare_Null;

   procedure Warn_Compare_Meta (Loc : Syn_Src) is
   begin
      Warning_Msg_Synth (+Loc, "metavalue detected, returning false");
   end Warn_Compare_Meta;

   function Compare_Uns_Uns
     (Left, Right : Memtyp; Err : Order_Type; Loc : Syn_Src) return Order_Type
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
   end Compare_Uns_Uns;

   function Compare_Uns_Nat
     (Left, Right : Memtyp; Err : Order_Type; Loc : Syn_Src) return Order_Type
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
   end Compare_Uns_Nat;

   function Compare_Nat_Uns
     (Left, Right : Memtyp; Err : Order_Type; Loc : Syn_Src) return Order_Type
   is
      Rw   : constant Uns32 := Right.Typ.W;
      Lval : constant Uns64 := To_Uns64 (Read_Discrete (Left));
      R    : Std_Ulogic;
      Cnt  : Uns32;
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
   end Compare_Nat_Uns;

   function Compare_Sgn_Sgn
     (Left, Right : Memtyp; Err : Order_Type; Loc : Syn_Src) return Order_Type
   is
      Lw   : constant Uns32 := Left.Typ.W;
      Rw   : constant Uns32 := Right.Typ.W;
      Len  : constant Uns32 := Uns32'Min (Lw, Rw);
      P    : Uns32;
      L, R : Std_Ulogic;
      Res  : Order_Type;
   begin
      if Len = 0 then
         Warn_Compare_Null (Loc);
         return Err;
      end if;

      --  Compare the sign bit.
      L := To_X01 (Read_Std_Logic (Left.Mem, 0));
      R := To_X01 (Read_Std_Logic (Right.Mem, 0));
      if L = '1' and R = '0' then
         return Less;
      elsif L = '0' and R = '1' then
         return Greater;
      else
         Res := Equal;
      end if;

      --  Same sign.
      for I in 0 .. Uns32'Max (Lw, Rw) - 1 loop
         if I >= Lw then
            P := Lw - 1;
         else
            P := I;
         end if;
         L := To_X01 (Read_Std_Logic (Left.Mem, Lw - 1 - P));

         if I >= Rw then
            P := Rw - 1;
         else
            P := I;
         end if;
         R := To_X01 (Read_Std_Logic (Right.Mem, Rw - 1 - P));

         if L = 'X' or R = 'X' then
            Warn_Compare_Meta (Loc);
            return Err;
         end if;

         if L = '1' and R = '0' then
            Res := Greater;
         elsif L = '0' and R = '1' then
            Res := Less;
         end if;
      end loop;
      return Res;
   end Compare_Sgn_Sgn;

   function Compare_Sgn_Int
     (Left, Right : Memtyp; Err : Order_Type; Loc : Syn_Src) return Order_Type
   is
      Lw      : constant Uns32 := Left.Typ.W;
      Rval    : constant Int64 := Read_Discrete (Right);
      Rd      : Uns32;
      R1      : Uns64;
      Res     : Order_Type;
      L       : Std_Ulogic;
   begin
      if Lw = 0 then
         Warn_Compare_Null (Loc);
         return Err;
      end if;

      Res := Equal;
      R1 := To_Uns64 (Rval);

      --  Same sign.
      for I in 0 .. Lw - 1 loop
         L := To_X01 (Read_Std_Logic (Left.Mem, Lw - 1 - I));
         if L = 'X' then
            Warn_Compare_Meta (Loc);
            return Err;
         end if;

         Rd := Uns32 (R1 and 1);
         R1 := Shift_Right_Arithmetic (R1, 1);

         if L = '1' and then Rd = 0 then
            Res := Greater;
         elsif L = '0' and then Rd = 1 then
            Res := Less;
         end if;
      end loop;

      if L = '1' then
         if Rval >= 0 then
            Res := Less;
         end if;
      else
         if Rval < 0 then
            Res := Greater;
         end if;
      end if;
      return Res;
   end Compare_Sgn_Int;

   function Add_Vec_Vec (L, R : Memtyp; Signed : Boolean; Loc : Syn_Src)
                         return Memtyp
   is
      Llen : constant Uns32 := L.Typ.Vbound.Len;
      Rlen : constant Uns32 := R.Typ.Vbound.Len;
      Len : constant Uns32 := Uns32'Max (Llen, Rlen);
      Res : Memtyp;
      Lb, Rb, Carry : Sl_X01;
      R_Ext, L_Ext : Sl_X01;
   begin
      Res.Typ := Create_Res_Type (L.Typ, Len);
      Res := Create_Memory (Res.Typ);

      if Len = 0 then
         return Res;
      end if;

      if Signed then
         --  Extend with the sign bit.
         L_Ext := Sl_To_X01 (Read_Std_Logic (L.Mem, 0));
         R_Ext := Sl_To_X01 (Read_Std_Logic (R.Mem, 0));
      else
         --  Extend with '0'.
         L_Ext := '0';
         R_Ext := '0';
      end if;

      Carry := '0';
      for I in 1 .. Len loop
         if I > Llen then
            Lb := L_Ext;
         else
            Lb := Sl_To_X01 (Read_Std_Logic (L.Mem, Llen - I));
         end if;
         if I > Rlen then
            Rb := R_Ext;
         else
            Rb := Sl_To_X01 (Read_Std_Logic (R.Mem, Rlen - I));
         end if;
         if Lb = 'X' or Rb = 'X' then
            Warning_Msg_Synth
              (+Loc, "NUMERIC_STD.""+"": non logical value detected");
            Fill (Res, 'X');
            exit;
         end if;
         Write_Std_Logic (Res.Mem, Len - I, Compute_Sum (Carry, Rb, Lb));
         Carry := Compute_Carry (Carry, Rb, Lb);
      end loop;
      return Res;
   end Add_Vec_Vec;

   function Add_Uns_Uns (L, R : Memtyp; Loc : Syn_Src) return Memtyp is
   begin
      return Add_Vec_Vec (L, R, False, Loc);
   end Add_Uns_Uns;

   function Add_Sgn_Sgn (L, R : Memtyp; Loc : Syn_Src) return Memtyp is
   begin
      return Add_Vec_Vec (L, R, True, Loc);
   end Add_Sgn_Sgn;

   function Add_Vec_Int
     (L : Memtyp; R : Uns64; Signed : Boolean; Loc : Syn_Src) return Memtyp
   is
      Len          : constant Uns32 := L.Typ.Vbound.Len;
      Res : Memtyp;
      V : Uns64;
      Lb, Rb, Carry : Sl_X01;
   begin
      Res.Typ := Create_Res_Type (L.Typ, Len);
      Res := Create_Memory (Res.Typ);
      if Len < 1 then
         return Res;
      end if;
      V := R;
      Carry := '0';
      for I in 1 .. Len loop
         Lb := Sl_To_X01 (Read_Std_Logic (L.Mem, Len - I));
         Rb := Uns_To_01 (V and 1);
         if Lb = 'X' then
            Warning_Msg_Synth
              (+Loc, "NUMERIC_STD.""+"": non logical value detected");
            Fill (Res, 'X');
            exit;
         end if;
         Write_Std_Logic (Res.Mem, Len - I, Compute_Sum (Carry, Rb, Lb));
         Carry := Compute_Carry (Carry, Rb, Lb);
         if Signed then
            V := Shift_Right_Arithmetic (V, 1);
         else
            V := Shift_Right (V, 1);
         end if;
      end loop;
      return Res;
   end Add_Vec_Int;

   function Add_Sgn_Int (L : Memtyp; R : Int64; Loc : Syn_Src) return Memtyp is
   begin
      return Add_Vec_Int (L, To_Uns64 (R), True, Loc);
   end Add_Sgn_Int;

   function Add_Uns_Nat (L : Memtyp; R : Uns64; Loc : Syn_Src) return Memtyp is
   begin
      return Add_Vec_Int (L, R, True, Loc);
   end Add_Uns_Nat;

   function Sub_Vec_Vec (L, R : Memtyp; Signed : Boolean; Loc : Syn_Src)
                         return Memtyp
   is
      Llen          : constant Uns32 := L.Typ.Vbound.Len;
      Rlen          : constant Uns32 := R.Typ.Vbound.Len;
      Len           : constant Uns32 := Uns32'Max (Llen, Rlen);
      Res           : Memtyp;
      Lb, Rb, Carry : Sl_X01;
      R_Ext, L_Ext  : Sl_X01;
   begin
      Res.Typ := Create_Res_Type (L.Typ, Len);
      Res := Create_Memory (Res.Typ);

      if Len = 0 then
         return Res;
      end if;

      if Signed then
         --  Extend with the sign bit.
         L_Ext := Sl_To_X01 (Read_Std_Logic (L.Mem, 0));
         R_Ext := Sl_To_X01 (Read_Std_Logic (R.Mem, 0));
      else
         --  Extend with '0'.
         L_Ext := '0';
         R_Ext := '0';
      end if;

      Carry := '1';
      for I in 1 .. Len loop
         if I > Llen then
            Lb := L_Ext;
         else
            Lb := Sl_To_X01 (Read_Std_Logic (L.Mem, Llen - I));
         end if;
         if I > Rlen then
            Rb := R_Ext;
         else
            Rb := Sl_To_X01 (Read_Std_Logic (R.Mem, Rlen - I));
         end if;
         Rb := Not_Table (Rb);
         if Lb = 'X' or Rb = 'X' then
            Warning_Msg_Synth
              (+Loc, "NUMERIC_STD.""-"": non logical value detected");
            Fill (Res, 'X');
            exit;
         end if;
         Write_Std_Logic (Res.Mem, Len - I, Compute_Sum (Carry, Rb, Lb));
         Carry := Compute_Carry (Carry, Rb, Lb);
      end loop;
      return Res;
   end Sub_Vec_Vec;

   function Sub_Uns_Uns (L, R : Memtyp; Loc : Syn_Src) return Memtyp is
   begin
      return Sub_Vec_Vec (L, R, False, Loc);
   end Sub_Uns_Uns;

   function Sub_Sgn_Sgn (L, R : Memtyp; Loc : Syn_Src) return Memtyp is
   begin
      return Sub_Vec_Vec (L, R, True, Loc);
   end Sub_Sgn_Sgn;

   function Sub_Vec_Int
     (L : Memtyp; R : Uns64; Signed : Boolean; Loc : Syn_Src) return Memtyp
   is
      Len           : constant Uns32 := L.Typ.Vbound.Len;
      Res           : Memtyp;
      V             : Uns64;
      Lb, Rb, Carry : Sl_X01;
   begin
      Res.Typ := Create_Res_Type (L.Typ, Len);
      Res := Create_Memory (Res.Typ);
      if Len < 1 then
         return Res;
      end if;
      V := R;
      Carry := '1';
      for I in 1 .. Len loop
         Lb := Sl_To_X01 (Read_Std_Logic (L.Mem, Len - I));
         Rb := Uns_To_01 (V and 1);
         if Lb = 'X' then
            Warning_Msg_Synth
              (+Loc, "NUMERIC_STD.""+"": non logical value detected");
            Fill (Res, 'X');
            exit;
         end if;
         Rb := Not_Table (Rb);
         Write_Std_Logic (Res.Mem, Len - I, Compute_Sum (Carry, Rb, Lb));
         Carry := Compute_Carry (Carry, Rb, Lb);
         if Signed then
            V := Shift_Right_Arithmetic (V, 1);
         else
            V := Shift_Right (V, 1);
         end if;
      end loop;
      return Res;
   end Sub_Vec_Int;

   function Sub_Sgn_Int (L : Memtyp; R : Int64; Loc : Syn_Src) return Memtyp is
   begin
      return Sub_Vec_Int (L, To_Uns64 (R), True, Loc);
   end Sub_Sgn_Int;

   function Sub_Uns_Nat (L : Memtyp; R : Uns64; Loc : Syn_Src) return Memtyp is
   begin
      return Sub_Vec_Int (L, R, True, Loc);
   end Sub_Uns_Nat;

   function Mul_Uns_Uns (L, R : Memtyp; Loc : Syn_Src) return Memtyp
   is
      Llen          : constant Uns32 := L.Typ.Vbound.Len;
      Rlen          : constant Uns32 := R.Typ.Vbound.Len;
      Len           : constant Uns32 := Llen + Rlen;
      Res           : Memtyp;
      Lb, Rb, Vb, Carry : Sl_X01;
   begin
      Res.Typ := Create_Res_Type (L.Typ, Len);
      Res := Create_Memory (Res.Typ);
      if Llen = 0 or Rlen = 0 then
         return Res;
      end if;
      Fill (Res, '0');
      --  Shift and add L.
      for I in 1 .. Rlen loop
         Rb := Sl_To_X01 (Read_Std_Logic (R.Mem, Rlen - I));
         if Rb = '1' then
            --  Compute res := res + shift_left (l, i).
            Carry := '0';
            for J in 1 .. Llen loop
               Lb := Read_Std_Logic (L.Mem, Llen - J);
               Vb := Read_Std_Logic (Res.Mem, Len - (I + J - 1));
               Write_Std_Logic
                 (Res.Mem, Len - (I + J - 1), Compute_Sum (Carry, Vb, Lb));
               Carry := Compute_Carry (Carry, Vb, Lb);
            end loop;
            --  Propagate carry.
            for J in I + Llen .. Len loop
               exit when Carry = '0';
               Vb := Read_Std_Logic (Res.Mem, Len - J);
               Write_Std_Logic (Res.Mem, Len - J, Xor_Table (Carry, Vb));
               Carry := And_Table (Carry, Vb);
            end loop;
         elsif Rb = 'X' then
            Warning_Msg_Synth
              (+Loc, "NUMERIC_STD.""*"": non logical value detected");
            Fill (Res, 'X');
            exit;
         end if;
      end loop;
      return Res;
   end Mul_Uns_Uns;

   function To_Unsigned (Val : Uns64; Vtyp : Type_Acc) return Memtyp
   is
      Vlen : constant Uns32 := Vtyp.Vbound.Len;
      Res  : Memtyp;
      E    : Std_Ulogic;
   begin
      Res := Create_Memory (Vtyp);
      for I in 1 .. Vlen loop
         if (Shift_Right (Val, Natural (I - 1)) and 1) = 0 then
            E := '0';
         else
            E := '1';
         end if;
         Write_Std_Logic (Res.Mem, Vlen - I, E);
      end loop;
      return Res;
   end To_Unsigned;

   function Mul_Nat_Uns (L : Uns64; R : Memtyp; Loc : Syn_Src) return Memtyp
   is
      Lv : Memtyp;
   begin
      if R.Typ.Vbound.Len = 0 then
         return Create_Memory (R.Typ); --  FIXME: typ
      end if;
      Lv := To_Unsigned (L, R.Typ);
      return Mul_Uns_Uns (Lv, R, Loc);
   end Mul_Nat_Uns;

   function Mul_Uns_Nat (L : Memtyp; R : Uns64; Loc : Syn_Src) return Memtyp
   is
      Rv : Memtyp;
   begin
      if L.Typ.Vbound.Len = 0 then
         return Create_Memory (L.Typ); --  FIXME: typ
      end if;
      Rv := To_Unsigned (R, L.Typ);
      return Mul_Uns_Uns (L, Rv, Loc);
   end Mul_Uns_Nat;

   function Mul_Sgn_Sgn (L, R : Memtyp; Loc : Syn_Src) return Memtyp
   is
      Llen          : constant Uns32 := L.Typ.Vbound.Len;
      Rlen          : constant Uns32 := R.Typ.Vbound.Len;
      Len           : constant Uns32 := Llen + Rlen;
      Res           : Memtyp;
      Lb, Rb, Vb, Carry : Sl_X01;
   begin
      Res.Typ := Create_Res_Type (L.Typ, Len);
      Res := Create_Memory (Res.Typ);
      if Llen = 0 or Rlen = 0 then
         return Res;
      end if;
      Fill (Res, '0');
      --  Shift and add L, do not consider (yet) the sign bit of R.
      for I in 1 .. Rlen - 1 loop
         Rb := Sl_To_X01 (Read_Std_Logic (R.Mem, Rlen - I));
         if Rb = '1' then
            --  Compute res := res + shift_left (l, i).
            Carry := '0';
            for J in 1 .. Llen loop
               Lb := Read_Std_Logic (L.Mem, Llen - J);
               Vb := Read_Std_Logic (Res.Mem, Len - (I + J - 1));
               Write_Std_Logic
                 (Res.Mem, Len - (I + J - 1), Compute_Sum (Carry, Vb, Lb));
               Carry := Compute_Carry (Carry, Vb, Lb);
            end loop;
            --  Sign extend and propagate carry.
            Lb := Read_Std_Logic (L.Mem, 0);
            for J in I + Llen .. Len loop
               Vb := Read_Std_Logic (Res.Mem, Len - J);
               Write_Std_Logic (Res.Mem, Len - J, Compute_Sum (Carry, Vb, Lb));
               Carry := Compute_Carry (Carry, Vb, Lb);
            end loop;
         elsif Rb = 'X' then
            Warning_Msg_Synth
              (+Loc, "NUMERIC_STD.""*"": non logical value detected");
            Fill (Res, 'X');
            exit;
         end if;
      end loop;
      if Read_Std_Logic (R.Mem, 0) = '1' then
         --  R is a negative number.  It is considered as:
         --   -2**n + (Rn-1 Rn-2 ... R0).
         --  Compute res := res - 2**n * l.
         Carry := '1';
         for I in 1 .. Llen loop
            --  Start at len - (rlen - 1) = llen + 1
            Vb := Read_Std_Logic (Res.Mem, Llen - I + 1);
            Lb := Not_Table (Read_Std_Logic (L.Mem, Llen - I));
            Write_Std_Logic
              (Res.Mem, Llen - I + 1, Compute_Sum (Carry, Vb, Lb));
            Carry := Compute_Carry (Carry, Vb, Lb);
         end loop;
         --  The last bit.
         Vb := Read_Std_Logic (Res.Mem, 0);
         Lb := Not_Table (Read_Std_Logic (L.Mem, 0));
         Write_Std_Logic (Res.Mem, 0, Compute_Sum (Carry, Vb, Lb));
      end if;
      return Res;
   end Mul_Sgn_Sgn;

   function To_Signed (Val : Int64; Vtyp : Type_Acc) return Memtyp
   is
      Vlen : constant Uns32 := Vtyp.Vbound.Len;
      Uval : constant Uns64 := To_Uns64 (Val);
      Res  : Memtyp;
      E    : Std_Ulogic;
   begin
      Res := Create_Memory (Vtyp);
      for I in 1 .. Vlen loop
         if (Shift_Right_Arithmetic (Uval, Natural (I - 1)) and 1) = 0 then
            E := '0';
         else
            E := '1';
         end if;
         Write_Std_Logic (Res.Mem, Vlen - I, E);
      end loop;
      return Res;
   end To_Signed;

   function Mul_Int_Sgn (L : Int64; R : Memtyp; Loc : Syn_Src) return Memtyp
   is
      Lv : Memtyp;
   begin
      if R.Typ.Vbound.Len = 0 then
         return Create_Memory (R.Typ); --  FIXME: typ
      end if;
      Lv := To_Signed (L, R.Typ);
      return Mul_Sgn_Sgn (Lv, R, Loc);
   end Mul_Int_Sgn;

   function Mul_Sgn_Int (L : Memtyp; R : Int64; Loc : Syn_Src) return Memtyp
   is
      Rv : Memtyp;
   begin
      if L.Typ.Vbound.Len = 0 then
         return Create_Memory (L.Typ); --  FIXME: typ
      end if;
      Rv := To_Signed (R, L.Typ);
      return Mul_Sgn_Sgn (L, Rv, Loc);
   end Mul_Sgn_Int;

   function Neg_Vec_Notyp (V : Memtyp) return Memory_Ptr
   is
      Len : constant Uns32 := V.Typ.Vbound.Len;
      Vb, Carry : Sl_X01;
      Res       : Memory_Ptr;
   begin
      Res := Alloc_Memory (V.Typ);

      Carry := '1';
      for I in 1 .. Len loop
         Vb := Sl_To_X01 (Read_Std_Logic (V.Mem, Len - I));
         Vb := Not_Table (Vb);
         Write_Std_Logic (Res, Len - I, Xor_Table (Carry, Vb));
         Carry := And_Table (Carry, Vb);
      end loop;
      return Res;
   end Neg_Vec_Notyp;

   procedure Neg_Vec (V : Memtyp)
   is
      Len : constant Uns32 := V.Typ.Vbound.Len;
      Vb, Carry : Sl_X01;
   begin
      Carry := '1';
      for I in 1 .. Len loop
         Vb := Sl_To_X01 (Read_Std_Logic (V.Mem, Len - I));
         Vb := Not_Table (Vb);
         Write_Std_Logic (V.Mem, Len - I, Xor_Table (Carry, Vb));
         Carry := And_Table (Carry, Vb);
      end loop;
   end Neg_Vec;

   function Neg_Vec (V : Memtyp; Loc : Syn_Src) return Memtyp
   is
      Len : constant Uns32 := V.Typ.Vbound.Len;
      Res : Memtyp;
      Vb, Carry : Sl_X01;
   begin
      Res.Typ := Create_Res_Type (V.Typ, Len);
      Res := Create_Memory (Res.Typ);

      if Len = 0 then
         return Res;
      end if;

      Carry := '1';
      for I in 1 .. Len loop
         Vb := Sl_To_X01 (Read_Std_Logic (V.Mem, Len - I));
         if Vb = 'X' then
            Warning_Msg_Synth
              (+Loc, "NUMERIC_STD.""-"": non logical value detected");
            Fill (Res, 'X');
            exit;
         end if;
         Vb := Not_Table (Vb);
         Write_Std_Logic (Res.Mem, Len - I, Xor_Table (Carry, Vb));
         Carry := And_Table (Carry, Vb);
      end loop;
      return Res;
   end Neg_Vec;

   function Shift_Vec (Val : Memtyp;
                       Amt : Uns32;
                       Right : Boolean;
                       Arith : Boolean) return Memtyp
   is
      Len : constant Uns32 := Uns32 (Vec_Length (Val.Typ));
      Res : Memtyp;
      Pad, B : Std_Ulogic;
   begin
      Res.Typ := Create_Res_Type (Val.Typ, Len);
      Res := Create_Memory (Res.Typ);

      if Len = 0 then
         Fill (Res, '0');
         return Res;
      end if;

      if Arith then
         Pad := Read_Std_Logic (Val.Mem, 0);
      else
         Pad := '0';
      end if;

      if Amt >= Len then
         if Right then
            Fill (Res, Pad);
         else
            Fill (Res, '0');
         end if;
         return Res;
      end if;

      if Right then
         for I in 1 .. Amt loop
            Write_Std_Logic (Res.Mem, I - 1, Pad);
         end loop;
         for I in Amt + 1 .. Len loop
            B := Read_Std_Logic (Val.Mem, I - 1 - Amt);
            Write_Std_Logic (Res.Mem, I - 1, B);
         end loop;
      else
         for I in 1 .. Len - Amt loop
            B := Read_Std_Logic (Val.Mem, Amt + I - 1);
            Write_Std_Logic (Res.Mem, I - 1, B);
         end loop;
         for I in Len - Amt + 1 .. Len loop
            Write_Std_Logic (Res.Mem, I - 1, Pad);
         end loop;
      end if;
      return Res;
   end Shift_Vec;

   function Resize_Vec (Val : Memtyp;
                        Size : Uns32;
                        Signed : Boolean) return Memtyp
   is
      Old_Size : constant Uns32 := Uns32 (Vec_Length (Val.Typ));
      Res : Memtyp;
      Pad, B : Std_Ulogic;
   begin
      Res.Typ := Create_Res_Type (Val.Typ, Size);
      Res := Create_Memory (Res.Typ);

      if Signed and then Old_Size > 0 then
         Pad := Read_Std_Logic (Val.Mem, 0);
      else
         Pad := '0';
      end if;

      for I in 1 .. Size loop
         if I <= Old_Size then
            B := Read_Std_Logic (Val.Mem, Old_Size - I);
         else
            B := Pad;
         end if;
         Write_Std_Logic (Res.Mem, Size - I, B);
      end loop;

      return Res;
   end Resize_Vec;

   type Std_Logic_Vector_Type is array (Uns32 range <>) of Std_Ulogic;

   procedure Divmod (Num, Dem : Memtyp; Quot, Remain : Memtyp)
   is
      Nlen  : constant Uns32 := Num.Typ.Vbound.Len;
      Dlen  : constant Uns32 := Dem.Typ.Vbound.Len;
      pragma Assert (Nlen > 0);
      pragma Assert (Dlen > 0);
      pragma Assert (Quot.Typ.Vbound.Len = Nlen);
      Reg   : Std_Logic_Vector_Type (0 .. Dlen);
      Sub   : Std_Logic_Vector_Type (0 .. Dlen - 1);
      Carry : Sl_X01;
      D     : Sl_X01;
   begin
      Reg := (others => '0');
      Sub := (others => '0');

      -- Stupid pen and paper division algorithm.
      for I in 0 .. Nlen - 1 loop
         --  Shift
         Reg (0 .. Dlen - 1) := Reg (1 .. Dlen);
         Reg (Dlen) := Sl_To_X01 (Read_Std_Logic (Num.Mem, I));
         --  Substract
         Carry := '1';
         for J in reverse 0 .. Dlen - 1 loop
            D := Not_Table (Read_Std_Logic (Dem.Mem, J));
            Sub (J) := Compute_Sum (Carry, Reg (J + 1), D);
            Carry := Compute_Carry (Carry, Reg (J + 1), D);
         end loop;
         --  Extra REG bit.
         Carry := Compute_Carry (Carry, Reg (0), '1');
         --  Test
         Write_Std_Logic (Quot.Mem, I, Carry);
         if Carry = '1' then
            Reg (0) := '0';
            Reg (1 .. Dlen) := Sub;
         end if;
      end loop;
      if Remain /= Null_Memtyp then
         pragma Assert (Remain.Typ.Vbound.Len = Dlen);
         for I in 0 .. Dlen - 1 loop
            Write_Std_Logic (Remain.Mem, I, Reg (I + 1));
         end loop;
      end if;
   end Divmod;

   function Has_0x (V : Memtyp) return Sl_X01
   is
      Res : Sl_X01 := '0';
      E : Sl_X01;
   begin
      for I in 0 .. V.Typ.Vbound.Len - 1 loop
         E := To_X01 (Read_Std_Logic (V.Mem, I));
         if E = 'X' then
            return 'X';
         elsif E = '1' then
            Res := '1';
         end if;
      end loop;
      return Res;
   end Has_0x;

   function Div_Uns_Uns (L, R : Memtyp; Loc : Syn_Src) return Memtyp
   is
      Nlen  : constant Uns32 := L.Typ.Vbound.Len;
      Dlen  : constant Uns32 := R.Typ.Vbound.Len;
      Quot  : Memtyp;
      R0    : Sl_X01;
   begin
      Quot.Typ := Create_Res_Type (L.Typ, Nlen);
      Quot := Create_Memory (Quot.Typ);
      if Nlen = 0 or Dlen = 0 then
         return Quot;
      end if;

      R0 := Has_0x (R);
      if Has_0x (L) = 'X' or R0 = 'X' then
         Warning_Msg_Synth
           (+Loc, "NUMERIC_STD.""/"": non logical value detected");
         Fill (Quot, 'X');
         return Quot;
      end if;
      if R0 = '0' then
         Error_Msg_Synth (+Loc, "NUMERIC_STD.""/"": division by 0");
         Fill (Quot, 'X');
         return Quot;
      end if;
      Divmod (L, R, Quot, Null_Memtyp);
      return Quot;
   end Div_Uns_Uns;

   function Div_Sgn_Sgn (L, R : Memtyp; Loc : Syn_Src) return Memtyp
   is
      Nlen  : constant Uns32 := L.Typ.Vbound.Len;
      Dlen  : constant Uns32 := R.Typ.Vbound.Len;
      Quot  : Memtyp;
      R0    : Sl_X01;
      Lu    : Memtyp;
      Ru    : Memtyp;
      Neg   : Boolean;
   begin
      Quot.Typ := Create_Res_Type (L.Typ, Nlen);
      Quot := Create_Memory (Quot.Typ);
      if Nlen = 0 or Dlen = 0 then
         return Quot;
      end if;

      R0 := Has_0x (R);
      if Has_0x (L) = 'X' or R0 = 'X' then
         Warning_Msg_Synth
           (+Loc, "NUMERIC_STD.""/"": non logical value detected");
         Fill (Quot, 'X');
         return Quot;
      end if;
      if R0 = '0' then
         Error_Msg_Synth (+Loc, "NUMERIC_STD.""/"": division by 0");
         Fill (Quot, 'X');
         return Quot;
      end if;

      if To_X01 (Read_Std_Logic (L.Mem, 0)) = '1' then
         Lu.Typ := L.Typ;
         Lu.Mem := Neg_Vec_Notyp (L);
         Neg := True;
      else
         Lu := L;
         Neg := False;
      end if;

      if To_X01 (Read_Std_Logic (R.Mem, 0)) = '1' then
         Ru.Typ := R.Typ;
         Ru.Mem := Neg_Vec_Notyp (R);
         Neg := not Neg;
      else
         Ru := R;
      end if;

      Divmod (Lu, Ru, Quot, Null_Memtyp);

      if Neg then
         Neg_Vec (Quot);
      end if;
      return Quot;
   end Div_Sgn_Sgn;

end Synth.Ieee.Numeric_Std;
