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

with Types; use Types;

with Synth.Objtypes; use Synth.Objtypes;
with Synth.Source; use Synth.Source;

package Synth.Ieee.Numeric_Std is
   --  Reminder: vectors elements are from left to right.

   function Compare_Uns_Uns
     (Left, Right : Memtyp; Err : Order_Type; Loc : Syn_Src) return Order_Type;
   function Compare_Uns_Nat
     (Left, Right : Memtyp; Err : Order_Type; Loc : Syn_Src) return Order_Type;
   function Compare_Nat_Uns
     (Left, Right : Memtyp; Err : Order_Type; Loc : Syn_Src) return Order_Type;
   function Compare_Sgn_Sgn
     (Left, Right : Memtyp; Err : Order_Type; Loc : Syn_Src) return Order_Type;
   function Compare_Sgn_Int
     (Left, Right : Memtyp; Err : Order_Type; Loc : Syn_Src) return Order_Type;

   --  Unary "-"
   function Neg_Vec (V : Memtyp; Loc : Syn_Src) return Memtyp;

   --  "+"
   function Add_Uns_Uns (L, R : Memtyp; Loc : Syn_Src) return Memtyp;
   function Add_Sgn_Sgn (L, R : Memtyp; Loc : Syn_Src) return Memtyp;
   function Add_Sgn_Int (L : Memtyp; R : Int64; Loc : Syn_Src) return Memtyp;
   function Add_Uns_Nat (L : Memtyp; R : Uns64; Loc : Syn_Src) return Memtyp;

   --  "-"
   function Sub_Uns_Uns (L, R : Memtyp; Loc : Syn_Src) return Memtyp;
   function Sub_Sgn_Sgn (L, R : Memtyp; Loc : Syn_Src) return Memtyp;
   function Sub_Sgn_Int (L : Memtyp; R : Int64; Loc : Syn_Src) return Memtyp;
   function Sub_Uns_Nat (L : Memtyp; R : Uns64; Loc : Syn_Src) return Memtyp;

   --  "*"
   function Mul_Uns_Uns (L, R : Memtyp; Loc : Syn_Src) return Memtyp;
   function Mul_Nat_Uns (L : Uns64; R : Memtyp; Loc : Syn_Src) return Memtyp;
   function Mul_Uns_Nat (L : Memtyp; R : Uns64; Loc : Syn_Src) return Memtyp;

   function Mul_Sgn_Sgn (L, R : Memtyp; Loc : Syn_Src) return Memtyp;
   function Mul_Int_Sgn (L : Int64; R : Memtyp; Loc : Syn_Src) return Memtyp;
   function Mul_Sgn_Int (L : Memtyp; R : Int64; Loc : Syn_Src) return Memtyp;

   --  "/"
   function Div_Uns_Uns (L, R : Memtyp; Loc : Syn_Src) return Memtyp;
   function Div_Sgn_Sgn (L, R : Memtyp; Loc : Syn_Src) return Memtyp;

   --  Shift
   function Shift_Vec (Val : Memtyp;
                       Amt : Uns32;
                       Right : Boolean;
                       Arith : Boolean) return Memtyp;

   function Resize_Vec (Val : Memtyp;
                        Size : Uns32;
                        Signed : Boolean) return Memtyp;
end Synth.Ieee.Numeric_Std;
