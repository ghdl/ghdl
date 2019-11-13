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
      for I in 1 .. Len loop
         if I > L'Last then
            Lb := '0';
         else
            Lb := Sl_To_X01 (L (I));
         end if;
         if I > R'Last then
            Rb := '0';
         else
            Rb := Sl_To_X01 (R (I));
         end if;
         if Lb = 'X' or Rb = 'X' then
            --assert NO_WARNING
            --  report "NUMERIC_STD.""+"": non logical value detected"
            --  severity warning;
            Res := (others => 'X');
            exit;
         end if;
         Res (I) := Compute_Sum (Carry, Rb, Lb);
         Carry := Compute_Carry (Carry, Rb, Lb);
      end loop;
      return Res;
   end Add_Uns_Uns;
end Synth.Ieee.Numeric_Std;
