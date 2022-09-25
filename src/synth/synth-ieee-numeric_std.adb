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

with Elab.Memtype; use Elab.Memtype;

with Synth.Errors; use Synth.Errors;
with Synth.Ieee.Utils; use Synth.Ieee.Utils;
with Synth.Source; use Synth.Source;

package body Synth.Ieee.Numeric_Std is

   function Create_Res_Type (Otyp : Type_Acc; Len : Uns32) return Type_Acc is
   begin
      if Otyp.Abound.Len = Len
        and then Otyp.Abound.Right = 0
        and then Otyp.Abound.Dir = Dir_Downto
        and then not Otyp.Is_Global
      then
         --  Try to reuse the same type as the parameter.
         --  But the result type must be allocated on the expr_pool.
         --  FIXME: is this code ever executed ?
         pragma Assert (Otyp.Abound.Left = Int32 (Len) - 1);
         return Otyp;
      end if;
      return Create_Vec_Type_By_Length (Len, Otyp.Arr_El);
   end Create_Res_Type;

   procedure Fill (Res : Memtyp; V : Std_Ulogic) is
   begin
      for I in 1 .. Res.Typ.Abound.Len loop
         Write_Std_Logic (Res.Mem, I - 1, V);
      end loop;
   end Fill;

   procedure Warn_Compare_Null (Loc : Location_Type) is
   begin
      Warning_Msg_Synth (Loc, "null argument detected, returning false");
   end Warn_Compare_Null;

   procedure Warn_Compare_Meta (Loc : Location_Type) is
   begin
      Warning_Msg_Synth (Loc, "metavalue detected, returning false");
   end Warn_Compare_Meta;

   function Compare_Uns_Uns (Left, Right : Memtyp;
                             Err : Order_Type;
                             Loc : Location_Type) return Order_Type
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

   function Compare_Uns_Nat (Left, Right : Memtyp;
                             Err : Order_Type;
                             Loc : Location_Type) return Order_Type
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

   function Compare_Nat_Uns (Left, Right : Memtyp;
                             Err : Order_Type;
                             Loc : Location_Type) return Order_Type
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

   function Compare_Sgn_Sgn (Left, Right : Memtyp;
                             Err : Order_Type;
                             Loc : Location_Type) return Order_Type
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

   function Compare_Sgn_Int (Left, Right : Memtyp;
                             Err : Order_Type;
                             Loc : Location_Type) return Order_Type
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

   function Add_Vec_Vec (L, R : Memtyp; Signed : Boolean; Loc : Location_Type)
                         return Memtyp
   is
      Llen : constant Uns32 := L.Typ.Abound.Len;
      Rlen : constant Uns32 := R.Typ.Abound.Len;
      Len : constant Uns32 := Uns32'Max (Llen, Rlen);
      Res : Memtyp;
      Lb, Rb, Carry : Sl_X01;
      R_Ext, L_Ext : Sl_X01;
   begin
      if Rlen = 0 or Llen = 0 then
         Res.Typ := Create_Res_Type (L.Typ, 0);
         Res := Create_Memory (Res.Typ);
         return Res;
      end if;

      Res.Typ := Create_Res_Type (L.Typ, Len);
      Res := Create_Memory (Res.Typ);

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

   function Add_Uns_Uns (L, R : Memtyp; Loc : Location_Type) return Memtyp is
   begin
      return Add_Vec_Vec (L, R, False, Loc);
   end Add_Uns_Uns;

   function Log_To_Vec (Val : Memtyp; Vec : Memtyp) return Memtyp
   is
      Len : constant Uns32 := Vec.Typ.Abound.Len;
      Res : Memtyp;
   begin
      if Len = 0 then
         --  FIXME: is it an error ?
         return Vec;
      end if;
      Res := Create_Memory (Vec.Typ);
      Fill (Res, '0');
      Write_U8 (Res.Mem + Size_Type (Len - 1), Read_U8 (Val.Mem));
      return Res;
   end Log_To_Vec;

   function Add_Sgn_Sgn (L, R : Memtyp; Loc : Location_Type) return Memtyp is
   begin
      return Add_Vec_Vec (L, R, True, Loc);
   end Add_Sgn_Sgn;

   function Add_Vec_Int (L : Memtyp;
                         R : Uns64;
                         Signed : Boolean;
                         Loc : Location_Type) return Memtyp
   is
      Len          : constant Uns32 := L.Typ.Abound.Len;
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

   function Add_Sgn_Int (L : Memtyp; R : Int64; Loc : Location_Type)
                        return Memtyp is
   begin
      return Add_Vec_Int (L, To_Uns64 (R), True, Loc);
   end Add_Sgn_Int;

   function Add_Uns_Nat (L : Memtyp; R : Uns64; Loc : Location_Type)
                        return Memtyp is
   begin
      return Add_Vec_Int (L, R, True, Loc);
   end Add_Uns_Nat;

   function Sub_Vec_Vec (L, R : Memtyp; Signed : Boolean; Loc : Location_Type)
                         return Memtyp
   is
      Llen          : constant Uns32 := L.Typ.Abound.Len;
      Rlen          : constant Uns32 := R.Typ.Abound.Len;
      Len           : constant Uns32 := Uns32'Max (Llen, Rlen);
      Res           : Memtyp;
      Lb, Rb, Carry : Sl_X01;
      R_Ext, L_Ext  : Sl_X01;
   begin
      if Llen = 0 or Rlen = 0 then
         Res.Typ := Create_Res_Type (L.Typ, 0);
         Res := Create_Memory (Res.Typ);
         return Res;
      end if;

      Res.Typ := Create_Res_Type (L.Typ, Len);
      Res := Create_Memory (Res.Typ);

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

   function Sub_Uns_Uns (L, R : Memtyp; Loc : Location_Type) return Memtyp is
   begin
      return Sub_Vec_Vec (L, R, False, Loc);
   end Sub_Uns_Uns;

   function Sub_Sgn_Sgn (L, R : Memtyp; Loc : Location_Type) return Memtyp is
   begin
      return Sub_Vec_Vec (L, R, True, Loc);
   end Sub_Sgn_Sgn;

   function Sub_Vec_Int (L : Memtyp;
                         R : Uns64;
                         Signed : Boolean;
                         Loc : Location_Type) return Memtyp
   is
      Len           : constant Uns32 := L.Typ.Abound.Len;
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

   function Sub_Sgn_Int (L : Memtyp;
                         R : Int64;
                         Loc : Location_Type) return Memtyp is
   begin
      return Sub_Vec_Int (L, To_Uns64 (R), True, Loc);
   end Sub_Sgn_Int;

   function Sub_Uns_Nat (L : Memtyp; R : Uns64; Loc : Location_Type)
                        return Memtyp is
   begin
      return Sub_Vec_Int (L, R, True, Loc);
   end Sub_Uns_Nat;

   function Sub_Int_Vec (L : Uns64;
                         R : Memtyp;
                         Signed : Boolean;
                         Loc : Location_Type) return Memtyp
   is
      Len           : constant Uns32 := R.Typ.Abound.Len;
      Res           : Memtyp;
      V             : Uns64;
      Lb, Rb, Carry : Sl_X01;
   begin
      Res.Typ := Create_Res_Type (R.Typ, Len);
      Res := Create_Memory (Res.Typ);
      if Len < 1 then
         return Res;
      end if;
      V := L;
      Carry := '1';
      for I in 1 .. Len loop
         Lb := Uns_To_01 (V and 1);
         Rb := Sl_To_X01 (Read_Std_Logic (R.Mem, Len - I));
         if Rb = 'X' then
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
   end Sub_Int_Vec;

   function Sub_Nat_Uns (L : Uns64; R : Memtyp; Loc : Location_Type)
                        return Memtyp is
   begin
      return Sub_Int_Vec (L, R, False, Loc);
   end Sub_Nat_Uns;

   function Sub_Int_Sgn (L : Int64; R : Memtyp; Loc : Location_Type)
                        return Memtyp is
   begin
      return Sub_Int_Vec (To_Uns64 (L), R, True, Loc);
   end Sub_Int_Sgn;

   function Mul_Uns_Uns (L, R : Memtyp; Loc : Location_Type) return Memtyp
   is
      Llen          : constant Uns32 := L.Typ.Abound.Len;
      Rlen          : constant Uns32 := R.Typ.Abound.Len;
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
      Vlen : constant Uns32 := Vtyp.Abound.Len;
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

   function Mul_Nat_Uns (L : Uns64; R : Memtyp; Loc : Location_Type)
                        return Memtyp
   is
      Lv : Memtyp;
   begin
      if R.Typ.Abound.Len = 0 then
         return Create_Memory (R.Typ); --  FIXME: typ
      end if;
      Lv := To_Unsigned (L, R.Typ);
      return Mul_Uns_Uns (Lv, R, Loc);
   end Mul_Nat_Uns;

   function Mul_Uns_Nat (L : Memtyp; R : Uns64; Loc : Location_Type)
                        return Memtyp
   is
      Rv : Memtyp;
   begin
      if L.Typ.Abound.Len = 0 then
         return Create_Memory (L.Typ); --  FIXME: typ
      end if;
      Rv := To_Unsigned (R, L.Typ);
      return Mul_Uns_Uns (L, Rv, Loc);
   end Mul_Uns_Nat;

   function Mul_Sgn_Sgn (L, R : Memtyp; Loc : Location_Type) return Memtyp
   is
      Llen          : constant Uns32 := L.Typ.Abound.Len;
      Rlen          : constant Uns32 := R.Typ.Abound.Len;
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
      Vlen : constant Uns32 := Vtyp.Abound.Len;
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

   function Mul_Int_Sgn (L : Int64; R : Memtyp; Loc : Location_Type)
                        return Memtyp
   is
      Lv : Memtyp;
   begin
      if R.Typ.Abound.Len = 0 then
         return Create_Memory (R.Typ); --  FIXME: typ
      end if;
      Lv := To_Signed (L, R.Typ);
      return Mul_Sgn_Sgn (Lv, R, Loc);
   end Mul_Int_Sgn;

   function Mul_Sgn_Int (L : Memtyp; R : Int64; Loc : Location_Type)
                        return Memtyp
   is
      Rv : Memtyp;
   begin
      if L.Typ.Abound.Len = 0 then
         return Create_Memory (L.Typ); --  FIXME: typ
      end if;
      Rv := To_Signed (R, L.Typ);
      return Mul_Sgn_Sgn (L, Rv, Loc);
   end Mul_Sgn_Int;

   function Neg_Vec_Notyp (V : Memtyp) return Memory_Ptr
   is
      Res : Memory_Ptr;
   begin
      Res := Alloc_Memory (V.Typ, Current_Pool);

      Neg_Vec (V.Mem, Res, V.Typ.Abound.Len);
      return Res;
   end Neg_Vec_Notyp;

   procedure Neg_Vec (V : Memtyp) is
   begin
      Neg_Vec (V.Mem, V.Mem, V.Typ.Abound.Len);
   end Neg_Vec;

   function Has_0x (V : Memtyp) return Sl_X01
   is
      Res : Sl_X01 := '0';
      E : Sl_X01;
   begin
      for I in 0 .. V.Typ.Abound.Len - 1 loop
         E := To_X01 (Read_Std_Logic (V.Mem, I));
         if E = 'X' then
            return 'X';
         elsif E = '1' then
            Res := '1';
         end if;
      end loop;
      return Res;
   end Has_0x;

   function Neg_Vec (V : Memtyp; Loc : Location_Type) return Memtyp
   is
      Len : constant Uns32 := V.Typ.Abound.Len;
      Res : Memtyp;
   begin
      Res.Typ := Create_Res_Type (V.Typ, Len);
      Res := Create_Memory (Res.Typ);

      if Len = 0 then
         return Res;
      end if;

      if Has_0x (V) = 'X' then
         Warning_Msg_Synth
           (+Loc, "NUMERIC_STD.""-"": non logical value detected");
         Fill (Res, 'X');
      else
         Neg_Vec (V.Mem, Res.Mem, V.Typ.Abound.Len);
      end if;
      return Res;
   end Neg_Vec;

   procedure To_01X (Src : Memory_Ptr; Dst : Memory_Ptr; Len : Uns32)
   is
      V : Sl_X01;
   begin
      for I in 1 .. Len loop
         V := Sl_To_X01 (Read_Std_Logic (Src, Len - I));
         if V = 'X' then
            for J in 1 .. Len loop
               Write_Std_Logic (Dst, J - 1, 'X');
            end loop;
            return;
         end if;
         Write_Std_Logic (Dst, Len - I, V);
      end loop;
   end To_01X;

   function Abs_Vec (V : Memtyp; Loc : Location_Type) return Memtyp
   is
      pragma Unreferenced (Loc);
      Len : constant Uns32 := V.Typ.Abound.Len;
      Res : Memtyp;
      Msb : Sl_X01;
   begin
      Res.Typ := Create_Res_Type (V.Typ, Len);
      Res := Create_Memory (Res.Typ);

      if Len = 0 then
         return Res;
      end if;

      --  Convert to 01, check for X.
      To_01X (V.Mem, Res.Mem, Len);
      Msb := Read_Std_Logic (Res.Mem, 0);
      if Msb = '1' then
         Neg_Vec (Res);
      end if;
      return Res;
   end Abs_Vec;

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

   function Rotate_Vec (Val : Memtyp;
                        Amt : Uns32;
                        Right : Boolean) return Memtyp
   is
      Len : constant Uns32 := Uns32 (Vec_Length (Val.Typ));
      Cnt : Uns32;
      Res : Memtyp;
      B : Std_Ulogic;
   begin
      Res.Typ := Create_Res_Type (Val.Typ, Len);
      Res := Create_Memory (Res.Typ);

      if Len = 0 then
         return Res;
      end if;

      Cnt := Amt rem Len;
      pragma Unreferenced (Amt);

      if Right then
         for I in 1 .. Len - Cnt loop
            B := Read_Std_Logic (Val.Mem, I - 1);
            Write_Std_Logic (Res.Mem, Cnt + I - 1, B);
         end loop;
         for I in 1 .. Cnt loop
            B := Read_Std_Logic (Val.Mem, Len - I);
            Write_Std_Logic (Res.Mem, Cnt - I, B);
         end loop;
      else
         for I in 1 .. Cnt loop
            B := Read_Std_Logic (Val.Mem, I - 1);
            Write_Std_Logic (Res.Mem, Len - Cnt + I - 1, B);
         end loop;
         for I in 1 .. Len - Cnt loop
            B := Read_Std_Logic (Val.Mem, Len - I);
            Write_Std_Logic (Res.Mem, Len - Cnt - I, B);
         end loop;
      end if;
      return Res;
   end Rotate_Vec;

   procedure Resize_Vec (Dest : Memtyp; Val : Memtyp; Signed : Boolean)
   is
      Size : constant Uns32 := Dest.Typ.Abound.Len;
      Old_Size : constant Uns32 := Val.Typ.Abound.Len;
      L : Uns32;
      Pad, B : Std_Ulogic;
   begin
      if Size = 0 then
         return;
      end if;

      if Signed and then Old_Size > 0 then
         Pad := Read_Std_Logic (Val.Mem, 0);
         Write_Std_Logic (Dest.Mem, 0, Pad);
         L := Size - 1;
      else
         Pad := '0';
         L := Size;
      end if;

      for I in 1 .. L loop
         if I <= Old_Size then
            B := Read_Std_Logic (Val.Mem, Old_Size - I);
         else
            B := Pad;
         end if;
         Write_Std_Logic (Dest.Mem, Size - I, B);
      end loop;
   end Resize_Vec;

   function Resize_Vec (Val : Memtyp;
                        Size : Uns32;
                        Signed : Boolean) return Memtyp
   is
      Res : Memtyp;
   begin
      Res.Typ := Create_Res_Type (Val.Typ, Size);
      Res := Create_Memory (Res.Typ);

      Resize_Vec (Res, Val, Signed);

      return Res;
   end Resize_Vec;

   type Std_Logic_Vector_Type is array (Uns32 range <>) of Std_Ulogic;

   procedure Divmod (Num, Dem : Memtyp; Quot, Remain : Memtyp)
   is
      Nlen  : constant Uns32 := Num.Typ.Abound.Len;
      Dlen  : constant Uns32 := Dem.Typ.Abound.Len;
      pragma Assert (Nlen > 0);
      pragma Assert (Dlen > 0);
      pragma Assert (Quot.Typ = null or else Quot.Typ.Abound.Len = Nlen);
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
         if Quot.Mem /= null then
            Write_Std_Logic (Quot.Mem, I, Carry);
         end if;
         if Carry = '1' then
            Reg (0) := '0';
            Reg (1 .. Dlen) := Sub;
         end if;
      end loop;
      if Remain /= Null_Memtyp then
         pragma Assert (Remain.Typ.Abound.Len = Dlen);
         for I in 0 .. Dlen - 1 loop
            Write_Std_Logic (Remain.Mem, I, Reg (I + 1));
         end loop;
      end if;
   end Divmod;

   function Div_Uns_Uns (Inst : Synth_Instance_Acc;
                         L, R : Memtyp;
                         Loc : Node) return Memtyp
   is
      Nlen  : constant Uns32 := L.Typ.Abound.Len;
      Dlen  : constant Uns32 := R.Typ.Abound.Len;
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
         Error_Msg_Synth (Inst, Loc, "NUMERIC_STD.""/"": division by 0");
         Fill (Quot, 'X');
         return Quot;
      end if;
      Divmod (L, R, Quot, Null_Memtyp);
      return Quot;
   end Div_Uns_Uns;

   function Div_Uns_Nat (Inst : Synth_Instance_Acc;
                         L : Memtyp;
                         R : Uns64;
                         Loc : Node) return Memtyp
   is
      Rv : Memtyp;
   begin
      if L.Typ.Abound.Len = 0 then
         return Create_Memory (L.Typ); --  FIXME: typ
      end if;
      Rv := To_Unsigned (R, L.Typ);
      return Div_Uns_Uns (Inst, L, Rv, Loc);
   end Div_Uns_Nat;

   function Div_Nat_Uns (Inst : Synth_Instance_Acc;
                         L : Uns64;
                         R : Memtyp;
                         Loc : Node) return Memtyp
   is
      Lv : Memtyp;
   begin
      if R.Typ.Abound.Len = 0 then
         return Create_Memory (R.Typ); --  FIXME: typ
      end if;
      Lv := To_Unsigned (L, R.Typ);
      return Div_Uns_Uns (Inst, Lv, R, Loc);
   end Div_Nat_Uns;

   function Div_Sgn_Sgn (Inst : Synth_Instance_Acc;
                         L, R : Memtyp;
                         Loc : Node) return Memtyp
   is
      Nlen  : constant Uns32 := L.Typ.Abound.Len;
      Dlen  : constant Uns32 := R.Typ.Abound.Len;
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
         Error_Msg_Synth (Inst, Loc, "NUMERIC_STD.""/"": division by 0");
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

   function Div_Sgn_Int (Inst : Synth_Instance_Acc;
                         L : Memtyp;
                         R : Int64;
                         Loc : Node) return Memtyp
   is
      Rv : Memtyp;
   begin
      if L.Typ.Abound.Len = 0 then
         return Create_Memory (L.Typ); --  FIXME: typ
      end if;
      Rv := To_Signed (R, L.Typ);
      return Div_Sgn_Sgn (Inst, L, Rv, Loc);
   end Div_Sgn_Int;

   function Div_Int_Sgn (Inst : Synth_Instance_Acc;
                         L : Int64;
                         R : Memtyp;
                         Loc : Node) return Memtyp
   is
      Lv : Memtyp;
   begin
      if R.Typ.Abound.Len = 0 then
         return Create_Memory (R.Typ); --  FIXME: typ
      end if;
      Lv := To_Signed (L, R.Typ);
      return Div_Sgn_Sgn (Inst, Lv, R, Loc);
   end Div_Int_Sgn;

   function Rem_Uns_Uns (Inst : Synth_Instance_Acc;
                         L, R : Memtyp;
                         Loc : Node) return Memtyp
   is
      Nlen  : constant Uns32 := L.Typ.Abound.Len;
      Dlen  : constant Uns32 := R.Typ.Abound.Len;
      Rema  : Memtyp;
      R0    : Sl_X01;
   begin
      Rema.Typ := Create_Res_Type (R.Typ, Dlen);
      Rema := Create_Memory (Rema.Typ);
      if Nlen = 0 or Dlen = 0 then
         return Rema;
      end if;

      R0 := Has_0x (R);
      if Has_0x (L) = 'X' or R0 = 'X' then
         Warning_Msg_Synth
           (+Loc, "NUMERIC_STD.""rem"": non logical value detected");
         Fill (Rema, 'X');
         return Rema;
      end if;
      if R0 = '0' then
         Error_Msg_Synth (Inst, Loc, "NUMERIC_STD.""rem"": division by 0");
         Fill (Rema, 'X');
         return Rema;
      end if;
      Divmod (L, R, Null_Memtyp, Rema);
      return Rema;
   end Rem_Uns_Uns;

   function Rem_Uns_Nat (Inst : Synth_Instance_Acc;
                         L : Memtyp;
                         R : Uns64;
                         Loc : Node) return Memtyp
   is
      Rv : Memtyp;
   begin
      if L.Typ.Abound.Len = 0 then
         return Create_Memory (L.Typ); --  FIXME: typ
      end if;
      Rv := To_Unsigned (R, L.Typ);
      return Rem_Uns_Uns (Inst, L, Rv, Loc);
   end Rem_Uns_Nat;

   function Rem_Nat_Uns (Inst : Synth_Instance_Acc;
                         L : Uns64;
                         R : Memtyp;
                         Loc : Node) return Memtyp
   is
      Lv : Memtyp;
   begin
      if R.Typ.Abound.Len = 0 then
         return Create_Memory (R.Typ); --  FIXME: typ
      end if;
      Lv := To_Unsigned (L, R.Typ);
      return Rem_Uns_Uns (Inst, Lv, R, Loc);
   end Rem_Nat_Uns;

   function Rem_Sgn_Sgn (Inst : Synth_Instance_Acc;
                         L, R : Memtyp;
                         Loc : Node) return Memtyp
   is
      Nlen  : constant Uns32 := L.Typ.Abound.Len;
      Dlen  : constant Uns32 := R.Typ.Abound.Len;
      Rema  : Memtyp;
      R0    : Sl_X01;
      Lu    : Memtyp;
      Ru    : Memtyp;
      Neg   : Boolean;
   begin
      Rema.Typ := Create_Res_Type (L.Typ, Dlen);
      Rema := Create_Memory (Rema.Typ);
      if Nlen = 0 or Dlen = 0 then
         return Rema;
      end if;

      R0 := Has_0x (R);
      if Has_0x (L) = 'X' or R0 = 'X' then
         Warning_Msg_Synth
           (+Loc, "NUMERIC_STD.""rem"": non logical value detected");
         Fill (Rema, 'X');
         return Rema;
      end if;
      if R0 = '0' then
         Error_Msg_Synth (Inst, Loc, "NUMERIC_STD.""rem"": division by 0");
         Fill (Rema, 'X');
         return Rema;
      end if;

      if To_X01 (Read_Std_Logic (L.Mem, 0)) = '1' then
         Lu.Typ := L.Typ;
         Lu.Mem := Neg_Vec_Notyp (L);
         Neg := True;
      else
         Neg := False;
         Lu := L;
      end if;

      if To_X01 (Read_Std_Logic (R.Mem, 0)) = '1' then
         Ru.Typ := R.Typ;
         Ru.Mem := Neg_Vec_Notyp (R);
      else
         Ru := R;
      end if;

      Divmod (Lu, Ru, Null_Memtyp, Rema);

      --  Result of rem has the sign of the dividend.
      if Neg then
         Neg_Vec (Rema);
      end if;
      return Rema;
   end Rem_Sgn_Sgn;

   function Rem_Sgn_Int (Inst : Synth_Instance_Acc;
                         L : Memtyp;
                         R : Int64;
                         Loc : Node) return Memtyp
   is
      Rv : Memtyp;
   begin
      if L.Typ.Abound.Len = 0 then
         return Create_Memory (L.Typ); --  FIXME: typ
      end if;
      Rv := To_Signed (R, L.Typ);
      return Rem_Sgn_Sgn (Inst, L, Rv, Loc);
   end Rem_Sgn_Int;

   function Rem_Int_Sgn (Inst : Synth_Instance_Acc;
                         L : Int64;
                         R : Memtyp;
                         Loc : Node) return Memtyp
   is
      Lv : Memtyp;
   begin
      if R.Typ.Abound.Len = 0 then
         return Create_Memory (R.Typ); --  FIXME: typ
      end if;
      Lv := To_Signed (L, R.Typ);
      return Rem_Sgn_Sgn (Inst, Lv, R, Loc);
   end Rem_Int_Sgn;

   function Mod_Sgn_Sgn (Inst : Synth_Instance_Acc;
                         L, R : Memtyp;
                         Loc : Node) return Memtyp
   is
      Nlen  : constant Uns32 := L.Typ.Abound.Len;
      Dlen  : constant Uns32 := R.Typ.Abound.Len;
      Rema  : Memtyp;
      R0    : Sl_X01;
      Lu    : Memtyp;
      Ru    : Memtyp;
      L_Neg, R_Neg : Boolean;
   begin
      Rema.Typ := Create_Res_Type (L.Typ, Dlen);
      Rema := Create_Memory (Rema.Typ);
      if Nlen = 0 or Dlen = 0 then
         return Rema;
      end if;

      R0 := Has_0x (R);
      if Has_0x (L) = 'X' or R0 = 'X' then
         Warning_Msg_Synth
           (+Loc, "NUMERIC_STD.""rem"": non logical value detected");
         Fill (Rema, 'X');
         return Rema;
      end if;
      if R0 = '0' then
         Error_Msg_Synth (Inst, Loc, "NUMERIC_STD.""rem"": division by 0");
         Fill (Rema, 'X');
         return Rema;
      end if;

      if To_X01 (Read_Std_Logic (L.Mem, 0)) = '1' then
         Lu.Typ := L.Typ;
         Lu.Mem := Neg_Vec_Notyp (L);
         L_Neg := True;
      else
         Lu := L;
         L_Neg := False;
      end if;

      if To_X01 (Read_Std_Logic (R.Mem, 0)) = '1' then
         Ru.Typ := R.Typ;
         Ru.Mem := Neg_Vec_Notyp (R);
         R_Neg := True;
      else
         Ru := R;
         R_Neg := False;
      end if;

      Divmod (Lu, Ru, Null_Memtyp, Rema);

      if Has_0x (Rema) = '0' then
         --  If the remainder is 0, then the modulus is 0.
         return Rema;
      else
         --  Result of rem has the sign of the divisor.
         if R_Neg then
            if L_Neg then
               Neg_Vec (Rema);
               return Rema;
            else
               return Add_Vec_Vec (R, Rema, True, +Loc);
            end if;
         else
            if L_Neg then
               return Sub_Vec_Vec (R, Rema, True, +Loc);
            else
               return Rema;
            end if;
         end if;
      end if;
   end Mod_Sgn_Sgn;

   function Mod_Sgn_Int (Inst : Synth_Instance_Acc;
                         L : Memtyp;
                         R : Int64;
                         Loc : Node) return Memtyp
   is
      Rv : Memtyp;
   begin
      if L.Typ.Abound.Len = 0 then
         return Create_Memory (L.Typ); --  FIXME: typ
      end if;
      Rv := To_Signed (R, L.Typ);
      return Mod_Sgn_Sgn (Inst, L, Rv, Loc);
   end Mod_Sgn_Int;

   function Mod_Int_Sgn (Inst : Synth_Instance_Acc;
                         L : Int64;
                         R : Memtyp;
                         Loc : Node) return Memtyp
   is
      Lv : Memtyp;
   begin
      if R.Typ.Abound.Len = 0 then
         return Create_Memory (R.Typ); --  FIXME: typ
      end if;
      Lv := To_Signed (L, R.Typ);
      return Mod_Sgn_Sgn (Inst, Lv, R, Loc);
   end Mod_Int_Sgn;

   function Minmax (L, R : Memtyp; Is_Signed : Boolean; Is_Max : Boolean)
                   return Memtyp
   is
      Len : constant Uns32 := Uns32'Max (L.Typ.Abound.Len, R.Typ.Abound.Len);
      Res : Memtyp;
      Lt : Boolean;
   begin
      if L.Typ.Abound.Len = 0 or R.Typ.Abound.Len = 0 then
         Res.Typ := Create_Res_Type (L.Typ, 0);
         Res := Create_Memory (Res.Typ);
         return Res;
      end if;

      Res.Typ := Create_Res_Type (L.Typ, Len);
      Res := Create_Memory (Res.Typ);

      if Has_0x (L) = 'X' or else Has_0x (R) = 'X' then
         Fill (Res, 'X');
         return Res;
      end if;

      if Is_Signed then
         Lt := Compare_Sgn_Sgn (L, R, Less, No_Location) = Less;
      else
         Lt := Compare_Uns_Uns (L, R, Less, No_Location) = Less;
      end if;

      if Lt xor Is_Max then
         Resize_Vec (Res, L, False);
      else
         Resize_Vec (Res, R, False);
      end if;
      return Res;
   end Minmax;

   function Offset_To_Index (Off : Int32; Typ : Type_Acc) return Int32 is
   begin
      case Typ.Abound.Dir is
         when Dir_To =>
            return Typ.Abound.Left + Off;
         when Dir_Downto =>
            return Typ.Abound.Left - Off;
      end case;
   end Offset_To_Index;

   function Find_Rightmost (Arg : Memtyp; Val : Memtyp) return Int32
   is
      Len : constant Uns32 := Arg.Typ.Abound.Len;
      Y : Std_Ulogic;
   begin
      Y := Read_Std_Logic (Val.Mem, 0);

      for I in reverse 1 .. Len loop
         if Match_Eq_Table (Read_Std_Logic (Arg.Mem, I - 1), Y) = '1' then
            return Offset_To_Index (Int32 (I - 1), Arg.Typ);
         end if;
      end loop;
      return -1;
   end Find_Rightmost;

   function Find_Leftmost (Arg : Memtyp; Val : Memtyp) return Int32
   is
      Len : constant Uns32 := Arg.Typ.Abound.Len;
      Y : Std_Ulogic;
   begin
      Y := Read_Std_Logic (Val.Mem, 0);

      for I in 1 .. Len loop
         if Match_Eq_Table (Read_Std_Logic (Arg.Mem, I - 1), Y) = '1' then
            return Offset_To_Index (Int32 (I - 1), Arg.Typ);
         end if;
      end loop;
      return -1;
   end Find_Leftmost;

   function Match_Vec (L, R : Memtyp; Loc : Location_Type) return Boolean
   is
      Llen : constant Uns32 := L.Typ.Abound.Len;
      Rlen : constant Uns32 := R.Typ.Abound.Len;
   begin
      if Llen = 0 or Rlen = 0 then
         Warn_Compare_Null (Loc);
         return False;
      end if;
      if Llen /= Rlen then
         Warning_Msg_Synth
           (+Loc, "NUMERIC_STD.STD_MATCH: length mismatch, returning FALSE");
         return False;
      end if;

      for I in 1 .. Llen loop
         if Match_Eq_Table (Read_Std_Logic (L.Mem, I - 1),
                            Read_Std_Logic (R.Mem, I - 1)) /= '1'
         then
            return False;
         end if;
      end loop;
      return True;
   end Match_Vec;

   function Match_Eq_Vec_Vec (Left, Right : Memtyp;
                              Is_Signed : Boolean;
                              Loc : Location_Type) return Std_Ulogic
   is
      Lw : constant Uns32 := Left.Typ.W;
      Rw : constant Uns32 := Right.Typ.W;
      Len : constant Uns32 := Uns32'Max (Left.Typ.W, Right.Typ.W);
      L, R, T : Std_Ulogic;
      Res : Std_Ulogic;
   begin
      if Len = 0 then
         Warn_Compare_Null (Loc);
         return 'X';
      end if;

      Res := '1';
      for I in 1 .. Len loop
         if I > Lw then
            if not Is_Signed then
               L := '0';
            end if;
         else
            L := Read_Std_Logic (Left.Mem, Lw - I);
         end if;
         if I > Rw then
            if not Is_Signed then
               R := '0';
            end if;
         else
            R := Read_Std_Logic (Right.Mem, Rw - I);
         end if;
         T := Match_Eq_Table (L, R);
         if T = 'U' then
            return T;
         elsif T = 'X' or Res = 'X' then
            --  Lower priority than 'U'.
            Res := 'X';
         elsif T = '0' then
            Res := '0';
         end if;
      end loop;
      return Res;
   end Match_Eq_Vec_Vec;

   function Has_Xd (V : Memtyp) return Std_Ulogic
   is
      Res : Std_Ulogic;
      E : Std_Ulogic;
   begin
      Res := '0';
      for I in 0 .. V.Typ.Abound.Len - 1 loop
         E := Read_Std_Logic (V.Mem, I);
         if E = '-' then
            return '-';
         elsif To_X01 (E) = 'X' then
            Res := 'X';
         end if;
      end loop;
      return Res;
   end Has_Xd;

   function Match_Cmp_Vec_Vec (Left, Right : Memtyp;
                               Map : Order_Map_Type;
                               Is_Signed : Boolean;
                               Loc : Location_Type) return Memtyp
   is
      Llen : constant Uns32 := Left.Typ.Abound.Len;
      Rlen : constant Uns32 := Right.Typ.Abound.Len;
      L, R : Std_Ulogic;
      Res : Std_Ulogic;
      Cmp : Order_Type;
   begin
      if Rlen = 0 or Llen = 0 then
         Warn_Compare_Null (Loc);
         Res := 'X';
      else
         L := Has_Xd (Left);
         R := Has_Xd (Right);
         if L = '-' or R = '-' then
            Warning_Msg_Synth (+Loc, "'-' found in compare string");
            Res := 'X';
         elsif L = 'X' or R = 'X' then
            Res := 'X';
         else
            if Is_Signed then
               Cmp := Compare_Sgn_Sgn (Left, Right, Equal, Loc);
            else
               Cmp := Compare_Uns_Uns (Left, Right, Equal, Loc);
            end if;
            Res := Map (Cmp);
         end if;
      end if;

      return Create_Memory_U8 (Std_Ulogic'Pos (Res), Logic_Type);
   end Match_Cmp_Vec_Vec;
end Synth.Ieee.Numeric_Std;
