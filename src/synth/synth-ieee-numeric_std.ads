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

with Types; use Types;

with Synth.Ieee.Std_Logic_1164; use Synth.Ieee.Std_Logic_1164;

package Synth.Ieee.Numeric_Std is
   --  Reminder: vectors elements are from left to right.

   function Neg_Sgn (V : Std_Logic_Vector) return Std_Logic_Vector;

   function Add_Uns_Uns (L, R : Std_Logic_Vector) return Std_Logic_Vector;

   function Add_Sgn_Int (L : Std_Logic_Vector; R : Int64)
                        return Std_Logic_Vector;
   function Add_Uns_Nat (L : Std_Logic_Vector; R : Uns64)
                        return Std_Logic_Vector;

   function Sub_Uns_Uns (L, R : Std_Logic_Vector) return Std_Logic_Vector;

   function Sub_Uns_Nat (L : Std_Logic_Vector; R : Uns64)
                        return Std_Logic_Vector;

   function Mul_Uns_Uns (L, R : Std_Logic_Vector) return Std_Logic_Vector;
   function Mul_Nat_Uns (L : Uns64; R : Std_Logic_Vector)
                        return Std_Logic_Vector;
   function Mul_Uns_Nat (L : Std_Logic_Vector; R : Uns64)
                        return Std_Logic_Vector;

   function Mul_Sgn_Sgn (L, R : Std_Logic_Vector) return Std_Logic_Vector;
end Synth.Ieee.Numeric_Std;
