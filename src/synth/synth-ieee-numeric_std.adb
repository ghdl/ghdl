--  numeric_std
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

with Types_Utils; use Types_Utils;
with Synth.Errors; use Synth.Errors;

package body Synth.Ieee.Numeric_Std is
   Null_Vec : constant Std_Logic_Vector (1 .. 0) := (others => '0');

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

   function Neg_Sgn (V : Std_Logic_Vector) return Std_Logic_Vector
   is
      pragma Assert (V'First = 1);
      Len : constant Integer := V'Last;
      subtype Res_Type is Std_Logic_Vector (1 .. Len);
      Res : Res_Type;
      Vb, Carry : Sl_X01;
   begin
      if Len < 1 then
         return Null_Vec;
      end if;
      Carry := '1';
      for I in 0 .. Len - 1 loop
         Vb := Sl_To_X01 (V (V'Last - I));
         if Vb = 'X' then
            --assert NO_WARNING
            --  report "NUMERIC_STD.""+"": non logical value detected"
            --  severity warning;
            Res := (others => 'X');
            exit;
         end if;
         Vb := Not_Table (Vb);
         Res (Res'Last - I) := Xor_Table (Carry, Vb);
         Carry := And_Table (Carry, Vb);
      end loop;
      return Res;
   end Neg_Sgn;

end Synth.Ieee.Numeric_Std;
