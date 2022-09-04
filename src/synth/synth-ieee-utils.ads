--  Simple logic utilities for ieee.std_logic
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

with Elab.Memtype; use Elab.Memtype;

with Synth.Ieee.Std_Logic_1164; use Synth.Ieee.Std_Logic_1164;

package Synth.Ieee.Utils is
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

   type Sl_To_01_Array is array (Std_Ulogic) of Sl_01;
   Sl_To_01 : constant Sl_To_X01_Array :=
     ('1' | 'H' => '1', others => '0');

   type Uns_To_01_Array is array (Uns64 range 0 .. 1) of Sl_X01;
   Uns_To_01 : constant Uns_To_01_Array := (0 => '0', 1 => '1');

   procedure Fill (Res : Memory_Ptr; Len : Uns32; V : Std_Ulogic);

   --  Note: SRC = DST is allowed.
   procedure Neg_Vec (Src : Memory_Ptr; Dst : Memory_Ptr; Len : Uns32);

   --  Note: SRC = DST is allowed.
   procedure Abs_Vec (Src : Memory_Ptr; Dst : Memory_Ptr; Len : Uns32);

   --  Multiplication.
   --  Length of RES is LLEN + RLEN + 1 (if L_SIGN /= R_SIGN)
   procedure Mul_Vec (L, R : Memory_Ptr;
                      Llen, Rlen : Uns32;
                      L_Sign, R_Sign : Boolean;
                      Res : Memory_Ptr);

   --  Assume no X (they are considered as '0').
   function Compare_Vec (L, R : Memory_Ptr;
                         Llen, Rlen : Uns32;
                         L_Sign, R_Sign : Boolean) return Order_Type;
end Synth.Ieee.Utils;
