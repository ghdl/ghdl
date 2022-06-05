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

with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;

with Synth.Ieee.Std_Logic_1164; use Synth.Ieee.Std_Logic_1164;

package Synth.Ieee.Numeric_Std is
   --  Reminder: vectors elements are from left to right.

   function Compare_Uns_Uns (Left, Right : Memtyp;
                             Err : Order_Type;
                             Loc : Location_Type) return Order_Type;
   function Compare_Uns_Nat (Left, Right : Memtyp;
                             Err : Order_Type;
                             Loc : Location_Type) return Order_Type;
   function Compare_Nat_Uns (Left, Right : Memtyp;
                             Err : Order_Type;
                             Loc : Location_Type) return Order_Type;
   function Compare_Sgn_Sgn (Left, Right : Memtyp;
                             Err : Order_Type;
                             Loc : Location_Type) return Order_Type;
   function Compare_Sgn_Int (Left, Right : Memtyp;
                             Err : Order_Type;
                             Loc : Location_Type) return Order_Type;

   --  Unary "-"
   function Neg_Vec (V : Memtyp; Loc : Location_Type) return Memtyp;

   --  "abs"
   function Abs_Vec (V : Memtyp; Loc : Location_Type) return Memtyp;

   --  Create a vector whose length is VEC'length, set to logic value VAL
   --  at the lsb and filled with 0.
   function Log_To_Vec (Val : Memtyp; Vec : Memtyp) return Memtyp;

   --  "+"
   function Add_Uns_Uns (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Add_Sgn_Sgn (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Add_Sgn_Int (L : Memtyp; R : Int64; Loc : Location_Type)
                        return Memtyp;
   function Add_Uns_Nat (L : Memtyp; R : Uns64; Loc : Location_Type)
                        return Memtyp;

   --  "-"
   function Sub_Uns_Uns (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Sub_Uns_Nat (L : Memtyp; R : Uns64; Loc : Location_Type)
                        return Memtyp;
   function Sub_Nat_Uns (L : Uns64; R : Memtyp; Loc : Location_Type)
                        return Memtyp;

   function Sub_Sgn_Sgn (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Sub_Sgn_Int (L : Memtyp; R : Int64; Loc : Location_Type)
                        return Memtyp;
   function Sub_Int_Sgn (L : Int64; R : Memtyp; Loc : Location_Type)
                        return Memtyp;

   --  "*"
   function Mul_Uns_Uns (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Mul_Nat_Uns (L : Uns64; R : Memtyp; Loc : Location_Type)
                        return Memtyp;
   function Mul_Uns_Nat (L : Memtyp; R : Uns64; Loc : Location_Type)
                        return Memtyp;

   function Mul_Sgn_Sgn (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Mul_Int_Sgn (L : Int64; R : Memtyp; Loc : Location_Type)
                        return Memtyp;
   function Mul_Sgn_Int (L : Memtyp; R : Int64; Loc : Location_Type)
                        return Memtyp;

   --  "/"
   function Div_Uns_Uns (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Div_Uns_Nat (L : Memtyp; R : Uns64; Loc : Location_Type)
                        return Memtyp;
   function Div_Nat_Uns (L : Uns64; R : Memtyp; Loc : Location_Type)
                        return Memtyp;
   function Div_Sgn_Sgn (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Div_Sgn_Int (L : Memtyp; R : Int64; Loc : Location_Type)
                        return Memtyp;
   function Div_Int_Sgn (L : Int64; R : Memtyp; Loc : Location_Type)
                        return Memtyp;

   --  "rem"
   function Rem_Uns_Uns (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Rem_Uns_Nat (L : Memtyp; R : Uns64; Loc : Location_Type)
                        return Memtyp;
   function Rem_Nat_Uns (L : Uns64; R : Memtyp; Loc : Location_Type)
                        return Memtyp;
   function Rem_Sgn_Sgn (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Rem_Sgn_Int (L : Memtyp; R : Int64; Loc : Location_Type)
                        return Memtyp;
   function Rem_Int_Sgn (L : Int64; R : Memtyp; Loc : Location_Type)
                        return Memtyp;

   --  "mod"
   function Mod_Sgn_Sgn (L, R : Memtyp; Loc : Location_Type)
                        return Memtyp;
   function Mod_Sgn_Int (L : Memtyp; R : Int64; Loc : Location_Type)
                        return Memtyp;
   function Mod_Int_Sgn (L : Int64; R : Memtyp; Loc : Location_Type)
                        return Memtyp;

   --  Shift
   function Shift_Vec (Val : Memtyp;
                       Amt : Uns32;
                       Right : Boolean;
                       Arith : Boolean) return Memtyp;

   --  Rotate
   function Rotate_Vec (Val : Memtyp;
                        Amt : Uns32;
                        Right : Boolean) return Memtyp;

   function Resize_Vec (Val : Memtyp;
                        Size : Uns32;
                        Signed : Boolean) return Memtyp;

   --  Minimum/Maximum.
   function Minmax (L, R : Memtyp; Is_Signed : Boolean; Is_Max : Boolean)
                   return Memtyp;

   --  Find_Rightmost/Find_Leftmost
   function Find_Rightmost (Arg : Memtyp; Val : Memtyp) return Int32;
   function Find_Leftmost (Arg : Memtyp; Val : Memtyp) return Int32;

   --  Std_Match
   function Match_Vec (L, R : Memtyp; Loc : Location_Type) return Boolean;

   --  Matching comparisons.
   function Match_Eq_Vec_Vec (Left, Right : Memtyp;
                              Is_Signed : Boolean;
                              Loc : Location_Type) return Std_Ulogic;

   type Order_Map_Type is array (Order_Type) of X01;

   Map_Lt : constant Order_Map_Type := "100";
   Map_Le : constant Order_Map_Type := "110";
   Map_Ge : constant Order_Map_Type := "011";
   Map_Gt : constant Order_Map_Type := "001";

   function Match_Cmp_Vec_Vec (Left, Right : Memtyp;
                               Map : Order_Map_Type;
                               Is_Signed : Boolean;
                               Loc : Location_Type) return Memtyp;

end Synth.Ieee.Numeric_Std;
