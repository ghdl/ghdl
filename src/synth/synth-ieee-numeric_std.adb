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

   function Add_Uns_Uns (L, R : Std_Logic_Vector) return Std_Logic_Vector
   is
      pragma Assert (L'First = 1);
      pragma Assert (R'First = 1);
      Len : constant Integer := Integer'Max (L'Last, R'Last);
      subtype Res_Type is Std_Logic_Vector (1 .. Len);
      Res : Res_Type;
      Lb, Rb, Carry : Sl_X01;
   begin
      if L'Last < 1 or R'Last < 1 then
         return Null_Vec;
      end if;
      Carry := '0';
      for I in 0 .. Len - 1 loop
         if I >= L'Last then
            Lb := '0';
         else
            Lb := Sl_To_X01 (L (L'Last - I));
         end if;
         if I >= R'Last then
            Rb := '0';
         else
            Rb := Sl_To_X01 (R (R'Last - I));
         end if;
         if Lb = 'X' or Rb = 'X' then
            --assert NO_WARNING
            --  report "NUMERIC_STD.""+"": non logical value detected"
            --  severity warning;
            Res := (others => 'X');
            exit;
         end if;
         Res (Res'Last - I) := Compute_Sum (Carry, Rb, Lb);
         Carry := Compute_Carry (Carry, Rb, Lb);
      end loop;
      return Res;
   end Add_Uns_Uns;

   function Add_Sgn_Int (L : Std_Logic_Vector; R : Int64)
                        return Std_Logic_Vector
   is
      pragma Assert (L'First = 1);
      Res : Std_Logic_Vector (1 .. L'Last);
      V : Uns64;
      Lb, Rb, Carry : Sl_X01;
   begin
      if L'Last < 1 then
         return Null_Vec;
      end if;
      V := To_Uns64 (R);
      Carry := '0';
      for I in reverse Res'Range loop
         Lb := Sl_To_X01 (L (I));
         Rb := Uns_To_01 (V and 1);
         if Lb = 'X' then
            --assert NO_WARNING
            --  report "NUMERIC_STD.""+"": non logical value detected"
            --  severity warning;
            Res := (others => 'X');
            exit;
         end if;
         Res (I) := Compute_Sum (Carry, Rb, Lb);
         Carry := Compute_Carry (Carry, Rb, Lb);
         V := Shift_Right_Arithmetic (V, 1);
      end loop;
      return Res;
   end Add_Sgn_Int;

   function Add_Uns_Nat (L : Std_Logic_Vector; R : Uns64)
                        return Std_Logic_Vector
   is
      pragma Assert (L'First = 1);
      Res : Std_Logic_Vector (1 .. L'Last);
      V : Uns64;
      Lb, Rb, Carry : Sl_X01;
   begin
      if L'Last < 1 then
         return Null_Vec;
      end if;
      V := R;
      Carry := '0';
      for I in reverse Res'Range loop
         Lb := Sl_To_X01 (L (I));
         Rb := Uns_To_01 (V and 1);
         if Lb = 'X' then
            --assert NO_WARNING
            --  report "NUMERIC_STD.""+"": non logical value detected"
            --  severity warning;
            Res := (others => 'X');
            exit;
         end if;
         Res (I) := Compute_Sum (Carry, Rb, Lb);
         Carry := Compute_Carry (Carry, Rb, Lb);
         V := Shift_Right (V, 1);
      end loop;
      return Res;
   end Add_Uns_Nat;

   function Mul_Uns_Uns (L, R : Std_Logic_Vector) return Std_Logic_Vector
   is
      pragma Assert (L'First = 1);
      pragma Assert (R'First = 1);
      Len : constant Integer := L'Last + R'Last;
      Res : Std_Logic_Vector (1 .. Len);
      Lb, Rb, Vb, Carry : Sl_X01;
   begin
      if L'Last < 1 or R'Last < 1 then
         return Null_Vec;
      end if;
      Res := (others => '0');
      --  Shift and add L.
      for I in 0 .. R'Last - 1 loop
         Rb := Sl_To_X01 (R (R'Last - I));
         if Rb = '1' then
            --  Compute res := res + shift_left (l, i).
            Carry := '0';
            for J in 0 .. L'Last - 1 loop
               Lb := L (L'Last - J);
               Vb := Res (Len - (I + J));
               Res (Len - (I + J)) := Compute_Sum (Carry, Vb, Lb);
               Carry := Compute_Carry (Carry, Vb, Lb);
            end loop;
            --  Propagate carry.
            for J in I + L'Last .. Res'Last loop
               exit when Carry = '0';
               Vb := Res (Len - J);
               Res (Len - J) := Xor_Table (Carry, Vb);
               Carry := And_Table (Carry, Vb);
            end loop;
         elsif Rb = 'X' then
            null;
            -- assert NO_WARNING
            --  report "NUMERIC_STD.""*"": non logical value detected"
            --  severity warning;
         end if;
      end loop;
      return Res;
   end Mul_Uns_Uns;

end Synth.Ieee.Numeric_Std;
