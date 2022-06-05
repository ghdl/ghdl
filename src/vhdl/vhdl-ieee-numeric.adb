--  Nodes recognizer for ieee.numeric_std and ieee.numeric_bit.
--  Copyright (C) 2016 Tristan Gingold
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
with Vhdl.Std_Package;
with Std_Names; use Std_Names;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Utils; use Vhdl.Utils;

package body Vhdl.Ieee.Numeric is
   type Pkg_Kind is (Pkg_Std, Pkg_Bit);
   type Sign_Kind is (Type_Signed, Type_Unsigned,
                      Type_Log, Type_Slv, Type_Suv);
   subtype Sign_Num_Kind is Sign_Kind range Type_Signed .. Type_Unsigned;
   type Arg_Kind is (Arg_Vect, Arg_Scal);
   type Args_Kind is (Arg_Vect_Vect, Arg_Vect_Scal, Arg_Scal_Vect,
                      Arg_Vect_Log, Arg_Log_Vect);

   type Binary_Pattern_Type is array (Pkg_Kind, Sign_Num_Kind, Args_Kind)
     of Iir_Predefined_Functions;

   type Unary_Pattern_Type is array (Pkg_Kind, Sign_Num_Kind)
     of Iir_Predefined_Functions;

   type Shift_Pattern_Type is array (Type_Signed .. Type_Unsigned)
     of Iir_Predefined_Functions;

   Add_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Add_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_Ieee_Numeric_Std_Add_Uns_Log,
            Arg_Log_Vect  => Iir_Predefined_Ieee_Numeric_Std_Add_Log_Uns),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Add_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_Ieee_Numeric_Std_Add_Sgn_Log,
            Arg_Log_Vect  => Iir_Predefined_Ieee_Numeric_Std_Add_Log_Sgn)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Sub_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Sub_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_Ieee_Numeric_Std_Sub_Uns_Log,
            Arg_Log_Vect  => Iir_Predefined_Ieee_Numeric_Std_Sub_Log_Uns),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Sub_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Sub_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Sub_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_Ieee_Numeric_Std_Sub_Sgn_Log,
            Arg_Log_Vect  => Iir_Predefined_Ieee_Numeric_Std_Sub_Log_Sgn)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Mul_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Mul_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Mul_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Mul_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Mul_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Mul_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Mul_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Div_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Div_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Div_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Div_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Div_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Div_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Div_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Rem_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Rem_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Rem_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Rem_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Rem_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Rem_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Rem_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Mod_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Mod_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Mod_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Mod_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Mod_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Mod_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Mod_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Eq_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Eq_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Eq_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Eq_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Eq_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Eq_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Eq_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Ne_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Ne_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Ne_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Ne_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Ne_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Ne_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Ne_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Lt_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Lt_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Lt_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Lt_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Lt_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Lt_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Lt_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Le_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Le_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Le_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Le_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Le_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Le_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Le_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Gt_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Gt_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Gt_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Gt_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Gt_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Gt_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Gt_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Ge_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Ge_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Ge_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Ge_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Ge_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Ge_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Ge_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Min_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Min_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Min_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Min_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Min_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Min_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Min_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Max_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Max_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Max_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Max_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Max_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Max_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Max_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Match_Eq_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Eq_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Match_Ne_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Ne_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Match_Lt_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Lt_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Match_Le_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Le_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Match_Le_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Le_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Le_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Match_Le_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Le_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Match_Gt_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Gt_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Match_Ge_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Uns_Uns,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Uns_Nat,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Nat_Uns,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Sgn_Sgn,
            Arg_Vect_Scal => Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Sgn_Int,
            Arg_Scal_Vect => Iir_Predefined_Ieee_Numeric_Std_Match_Ge_Int_Sgn,
            Arg_Vect_Log  => Iir_Predefined_None,
            Arg_Log_Vect  => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Neg_Patterns : constant Unary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_Neg_Uns,
         Type_Signed => Iir_Predefined_Ieee_Numeric_Std_Neg_Sgn),
      Pkg_Bit =>
        (others => Iir_Predefined_None));

   Abs_Patterns : constant Unary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned => Iir_Predefined_None,
         Type_Signed => Iir_Predefined_Ieee_Numeric_Std_Abs_Sgn),
      Pkg_Bit =>
        (others => Iir_Predefined_None));

   Not_Patterns : constant Unary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_Not_Uns,
         Type_Signed => Iir_Predefined_Ieee_Numeric_Std_Not_Sgn),
      Pkg_Bit =>
        (others => Iir_Predefined_None));

   Red_And_Patterns : constant Unary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_And_Uns,
         Type_Signed => Iir_Predefined_Ieee_Numeric_Std_And_Sgn),
      Pkg_Bit =>
        (others => Iir_Predefined_None));

   Red_Nand_Patterns : constant Unary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_Nand_Uns,
         Type_Signed => Iir_Predefined_Ieee_Numeric_Std_Nand_Sgn),
      Pkg_Bit =>
        (others => Iir_Predefined_None));

   Red_Or_Patterns : constant Unary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_Or_Uns,
         Type_Signed => Iir_Predefined_Ieee_Numeric_Std_Or_Sgn),
      Pkg_Bit =>
        (others => Iir_Predefined_None));

   Red_Nor_Patterns : constant Unary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_Nor_Uns,
         Type_Signed => Iir_Predefined_Ieee_Numeric_Std_Nor_Sgn),
      Pkg_Bit =>
        (others => Iir_Predefined_None));

   Red_Xor_Patterns : constant Unary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_Xor_Uns,
         Type_Signed => Iir_Predefined_Ieee_Numeric_Std_Xor_Sgn),
      Pkg_Bit =>
        (others => Iir_Predefined_None));

   Red_Xnor_Patterns : constant Unary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_Xnor_Uns,
         Type_Signed => Iir_Predefined_Ieee_Numeric_Std_Xnor_Sgn),
      Pkg_Bit =>
        (others => Iir_Predefined_None));

   And_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_And_Uns_Uns,
            Arg_Vect_Log  => Iir_Predefined_Ieee_Numeric_Std_And_Uns_Log,
            Arg_Log_Vect  => Iir_Predefined_Ieee_Numeric_Std_And_Log_Uns,
            others        => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_And_Sgn_Sgn,
            Arg_Vect_Log  => Iir_Predefined_Ieee_Numeric_Std_And_Sgn_Log,
            Arg_Log_Vect  => Iir_Predefined_Ieee_Numeric_Std_And_Log_Sgn,
            others        => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Or_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Or_Uns_Uns,
            Arg_Vect_Log  => Iir_Predefined_Ieee_Numeric_Std_Or_Uns_Log,
            Arg_Log_Vect  => Iir_Predefined_Ieee_Numeric_Std_Or_Log_Uns,
            others        => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Or_Sgn_Sgn,
            Arg_Vect_Log  => Iir_Predefined_Ieee_Numeric_Std_Or_Sgn_Log,
            Arg_Log_Vect  => Iir_Predefined_Ieee_Numeric_Std_Or_Log_Sgn,
            others        => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Nand_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Nand_Uns_Uns,
            Arg_Vect_Log  => Iir_Predefined_Ieee_Numeric_Std_Nand_Uns_Log,
            Arg_Log_Vect  => Iir_Predefined_Ieee_Numeric_Std_Nand_Log_Uns,
            others        => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Nand_Sgn_Sgn,
            Arg_Vect_Log  => Iir_Predefined_Ieee_Numeric_Std_Nand_Sgn_Log,
            Arg_Log_Vect  => Iir_Predefined_Ieee_Numeric_Std_Nand_Log_Sgn,
            others        => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Nor_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Nor_Uns_Uns,
            Arg_Vect_Log  => Iir_Predefined_Ieee_Numeric_Std_Nor_Uns_Log,
            Arg_Log_Vect  => Iir_Predefined_Ieee_Numeric_Std_Nor_Log_Uns,
            others        => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Nor_Sgn_Sgn,
            Arg_Vect_Log  => Iir_Predefined_Ieee_Numeric_Std_Nor_Sgn_Log,
            Arg_Log_Vect  => Iir_Predefined_Ieee_Numeric_Std_Nor_Log_Sgn,
            others        => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Xor_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Xor_Uns_Uns,
            Arg_Vect_Log  => Iir_Predefined_Ieee_Numeric_Std_Xor_Uns_Log,
            Arg_Log_Vect  => Iir_Predefined_Ieee_Numeric_Std_Xor_Log_Uns,
            others        => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Xor_Sgn_Sgn,
            Arg_Vect_Log  => Iir_Predefined_Ieee_Numeric_Std_Xor_Sgn_Log,
            Arg_Log_Vect  => Iir_Predefined_Ieee_Numeric_Std_Xor_Log_Sgn,
            others        => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Xnor_Patterns : constant Binary_Pattern_Type :=
     (Pkg_Std =>
        (Type_Unsigned =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Xnor_Uns_Uns,
            Arg_Vect_Log  => Iir_Predefined_Ieee_Numeric_Std_Xnor_Uns_Log,
            Arg_Log_Vect  => Iir_Predefined_Ieee_Numeric_Std_Xnor_Log_Uns,
            others        => Iir_Predefined_None),
         Type_Signed =>
           (Arg_Vect_Vect => Iir_Predefined_Ieee_Numeric_Std_Xnor_Sgn_Sgn,
            Arg_Vect_Log  => Iir_Predefined_Ieee_Numeric_Std_Xnor_Sgn_Log,
            Arg_Log_Vect  => Iir_Predefined_Ieee_Numeric_Std_Xnor_Log_Sgn,
            others        => Iir_Predefined_None)),
      Pkg_Bit =>
        (others =>
           (others => Iir_Predefined_None)));

   Shl_Patterns : constant Shift_Pattern_Type :=
     (Type_Signed   => Iir_Predefined_Ieee_Numeric_Std_Shf_Left_Sgn_Nat,
      Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_Shf_Left_Uns_Nat);

   Shr_Patterns : constant Shift_Pattern_Type :=
     (Type_Signed   => Iir_Predefined_Ieee_Numeric_Std_Shf_Right_Sgn_Nat,
      Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_Shf_Right_Uns_Nat);

   Rotate_Left_Patterns : constant Shift_Pattern_Type :=
     (Type_Signed   => Iir_Predefined_Ieee_Numeric_Std_Rot_Left_Sgn_Nat,
      Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_Rot_Left_Uns_Nat);

   Rotate_Right_Patterns : constant Shift_Pattern_Type :=
     (Type_Signed   => Iir_Predefined_Ieee_Numeric_Std_Rot_Right_Sgn_Nat,
      Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_Rot_Right_Uns_Nat);

   Sll_Patterns : constant Shift_Pattern_Type :=
     (Type_Signed   => Iir_Predefined_Ieee_Numeric_Std_Sll_Sgn_Int,
      Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_Sll_Uns_Int);

   Srl_Patterns : constant Shift_Pattern_Type :=
     (Type_Signed   => Iir_Predefined_Ieee_Numeric_Std_Srl_Sgn_Int,
      Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_Srl_Uns_Int);

   Sla_Patterns : constant Shift_Pattern_Type :=
     (Type_Signed   => Iir_Predefined_Ieee_Numeric_Std_Sla_Sgn_Int,
      Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_Sla_Uns_Int);

   Sra_Patterns : constant Shift_Pattern_Type :=
     (Type_Signed   => Iir_Predefined_Ieee_Numeric_Std_Sra_Sgn_Int,
      Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_Sra_Uns_Int);

   Rol_Patterns : constant Shift_Pattern_Type :=
     (Type_Signed   => Iir_Predefined_Ieee_Numeric_Std_Rol_Sgn_Int,
      Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_Rol_Uns_Int);

   Ror_Patterns : constant Shift_Pattern_Type :=
     (Type_Signed   => Iir_Predefined_Ieee_Numeric_Std_Ror_Sgn_Int,
      Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_Ror_Uns_Int);

   Leftmost_Patterns : constant Shift_Pattern_Type :=
     (Type_Signed   => Iir_Predefined_Ieee_Numeric_Std_Find_Leftmost_Sgn,
      Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_Find_Leftmost_Uns);

   Rightmost_Patterns : constant Shift_Pattern_Type :=
     (Type_Signed   => Iir_Predefined_Ieee_Numeric_Std_Find_Rightmost_Sgn,
      Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_Find_Rightmost_Uns);

   To_01_Patterns : constant Shift_Pattern_Type :=
     (Type_Signed   => Iir_Predefined_Ieee_Numeric_Std_To_01_Sgn,
      Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_To_01_Uns);

   To_X01_Patterns : constant Shift_Pattern_Type :=
     (Type_Signed   => Iir_Predefined_Ieee_Numeric_Std_To_X01_Sgn,
      Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_To_X01_Uns);

   To_X01z_Patterns : constant Shift_Pattern_Type :=
     (Type_Signed   => Iir_Predefined_Ieee_Numeric_Std_To_X01Z_Sgn,
      Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_To_X01Z_Uns);

   To_Ux01_Patterns : constant Shift_Pattern_Type :=
     (Type_Signed   => Iir_Predefined_Ieee_Numeric_Std_To_UX01_Sgn,
      Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_To_UX01_Uns);

   Is_X_Patterns : constant Shift_Pattern_Type :=
     (Type_Signed   => Iir_Predefined_Ieee_Numeric_Std_Is_X_Sgn,
      Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_Is_X_Uns);

   To_Hstring_Patterns : constant Shift_Pattern_Type :=
     (Type_Signed   => Iir_Predefined_Ieee_Numeric_Std_To_Hstring_Sgn,
      Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_To_Hstring_Uns);

   To_Ostring_Patterns : constant Shift_Pattern_Type :=
     (Type_Signed   => Iir_Predefined_Ieee_Numeric_Std_To_Ostring_Sgn,
      Type_Unsigned => Iir_Predefined_Ieee_Numeric_Std_To_Ostring_Uns);

   Error : exception;

   procedure Extract_Declarations (Pkg_Decl : Iir_Package_Declaration;
                                   Pkg : Pkg_Kind;
                                   Unsigned_Type : out Iir;
                                   Signed_Type : out Iir)
   is
      procedure Classify_Arg
        (Arg : Iir; Sign : out Sign_Kind; Kind : out Arg_Kind)
      is
         Arg_Type : constant Iir := Get_Type (Arg);
      begin
         if Arg_Type = Signed_Type then
            Sign := Type_Signed;
            Kind := Arg_Vect;
         elsif Arg_Type = Unsigned_Type then
            Sign := Type_Unsigned;
            Kind := Arg_Vect;
         elsif Arg_Type = Vhdl.Std_Package.Integer_Subtype_Definition then
            Sign := Type_Signed;
            Kind := Arg_Scal;
         elsif Arg_Type = Vhdl.Std_Package.Natural_Subtype_Definition then
            Sign := Type_Unsigned;
            Kind := Arg_Scal;
         elsif Arg_Type = Ieee.Std_Logic_1164.Std_Ulogic_Type then
            Sign := Type_Log;
            Kind := Arg_Scal;
         elsif Arg_Type = Ieee.Std_Logic_1164.Std_Ulogic_Vector_Type then
            Sign := Type_Suv;
            Kind := Arg_Vect;
         elsif Arg_Type = Ieee.Std_Logic_1164.Std_Logic_Type then
            Sign := Type_Log;
            Kind := Arg_Scal;
         elsif Arg_Type = Ieee.Std_Logic_1164.Std_Logic_Vector_Type then
            Sign := Type_Slv;
            Kind := Arg_Vect;
         elsif Arg_Type = Vhdl.Std_Package.Bit_Type_Definition then
            Sign := Type_Log;
            Kind := Arg_Scal;
         else
            raise Error;
         end if;
      end Classify_Arg;

      Decl : Iir;
      Def : Iir;

      Arg1, Arg2 : Iir;
      Arg1_Sign, Arg2_Sign : Sign_Kind;
      Arg1_Kind, Arg2_Kind : Arg_Kind;

      procedure Handle_Binary (Pats : Binary_Pattern_Type)
      is
         Kind : Args_Kind;
         Sign : Sign_Kind;
      begin
         if Arg1_Sign = Arg2_Sign then
            Sign := Arg1_Sign;
            case Arg1_Kind is
               when Arg_Vect =>
                  case Arg2_Kind is
                     when Arg_Vect => Kind := Arg_Vect_Vect;
                     when Arg_Scal => Kind := Arg_Vect_Scal;
                  end case;
               when Arg_Scal =>
                  case Arg2_Kind is
                     when Arg_Vect => Kind := Arg_Scal_Vect;
                     when Arg_Scal => raise Error;
                  end case;
            end case;
         elsif Arg1_Kind = Arg_Vect and Arg2_Sign = Type_Log then
            Sign := Arg1_Sign;
            Kind := Arg_Vect_Log;
         elsif Arg1_Sign = Type_Log and Arg2_Kind = Arg_Vect then
            Sign := Arg2_Sign;
            Kind := Arg_Log_Vect;
         else
            raise Error;
         end if;

         Set_Implicit_Definition (Decl, Pats (Pkg, Sign, Kind));
      end Handle_Binary;

      procedure Handle_Unary (Pats : Unary_Pattern_Type) is
      begin
         Set_Implicit_Definition (Decl, Pats (Pkg, Arg1_Sign));
      end Handle_Unary;

      procedure Handle_To_Unsigned
      is
         Predefined : Iir_Predefined_Functions;
      begin
         if Arg1_Kind = Arg_Scal and Arg1_Sign = Type_Unsigned then
            if Arg2_Kind = Arg_Scal and Arg2_Sign = Type_Unsigned then
               case Pkg is
                  when Pkg_Std =>
                     Predefined :=
                       Iir_Predefined_Ieee_Numeric_Std_Touns_Nat_Nat_Uns;
                  when Pkg_Bit =>
                     Predefined :=
                       Iir_Predefined_Ieee_Numeric_Bit_Touns_Nat_Nat_Uns;
               end case;
            elsif Arg2_Kind = Arg_Vect and Arg2_Sign = Type_Unsigned then
               case Pkg is
                  when Pkg_Std =>
                     Predefined :=
                       Iir_Predefined_Ieee_Numeric_Std_Touns_Nat_Uns_Uns;
                  when Pkg_Bit =>
                     Predefined :=
                       Iir_Predefined_Ieee_Numeric_Bit_Touns_Nat_Uns_Uns;
               end case;
            else
               raise Error;
            end if;
         else
            raise Error;
         end if;
         Set_Implicit_Definition (Decl, Predefined);
      end Handle_To_Unsigned;

      procedure Handle_To_Signed is
      begin
         if Arg1_Kind = Arg_Scal and Arg1_Sign = Type_Signed then
            if Arg2_Kind = Arg_Scal and Arg2_Sign = Type_Unsigned then
               Set_Implicit_Definition
                 (Decl, Iir_Predefined_Ieee_Numeric_Std_Tosgn_Int_Nat_Sgn);
            elsif Arg2_Kind = Arg_Vect and Arg2_Sign = Type_Signed then
               Set_Implicit_Definition
                 (Decl, Iir_Predefined_Ieee_Numeric_Std_Tosgn_Int_Sgn_Sgn);
            else
               raise Error;
            end if;
         else
            raise Error;
         end if;
      end Handle_To_Signed;

      procedure Handle_To_Integer is
      begin
         if Arg1_Kind = Arg_Vect and Arg1_Sign = Type_Unsigned then
            Set_Implicit_Definition
              (Decl, Iir_Predefined_Ieee_Numeric_Std_Toint_Uns_Nat);
         elsif Arg1_Kind = Arg_Vect and Arg1_Sign = Type_Signed then
            Set_Implicit_Definition
              (Decl, Iir_Predefined_Ieee_Numeric_Std_Toint_Sgn_Int);
         else
            raise Error;
         end if;
      end Handle_To_Integer;

      procedure Handle_Resize is
      begin
         if Arg2_Kind = Arg_Scal and Arg2_Sign = Type_Unsigned then
            if Arg1_Kind = Arg_Vect and Arg1_Sign = Type_Unsigned then
               Set_Implicit_Definition
                 (Decl, Iir_Predefined_Ieee_Numeric_Std_Resize_Uns_Nat);
            elsif Arg1_Kind = Arg_Vect and Arg1_Sign = Type_Signed then
               Set_Implicit_Definition
                 (Decl, Iir_Predefined_Ieee_Numeric_Std_Resize_Sgn_Nat);
            else
               raise Error;
            end if;
         elsif Arg2_Kind = Arg_Vect then
            if Arg1_Kind = Arg_Vect and Arg1_Sign = Type_Unsigned
              and Arg2_Sign = Type_Unsigned
            then
               Set_Implicit_Definition
                 (Decl, Iir_Predefined_Ieee_Numeric_Std_Resize_Uns_Uns);
            elsif Arg1_Kind = Arg_Vect and Arg1_Sign = Type_Signed
              and Arg2_Sign = Type_Signed
            then
               Set_Implicit_Definition
                 (Decl, Iir_Predefined_Ieee_Numeric_Std_Resize_Sgn_Sgn);
            else
               raise Error;
            end if;
         else
            raise Error;
         end if;
      end Handle_Resize;

      procedure Handle_Std_Match
      is
         Predefined : Iir_Predefined_Functions;
      begin
         if Arg1_Kind /= Arg2_Kind or else Arg1_Sign /= Arg2_Sign then
            raise Error;
         end if;

         if Arg1_Kind = Arg_Scal and Arg1_Sign = Type_Log then
            Predefined := Iir_Predefined_Ieee_Numeric_Std_Match_Log;
         elsif Arg1_Kind = Arg_Vect then
            case Arg1_Sign is
               when Type_Unsigned =>
                  Predefined := Iir_Predefined_Ieee_Numeric_Std_Match_Uns;
               when Type_Signed =>
                  Predefined := Iir_Predefined_Ieee_Numeric_Std_Match_Sgn;
               when Type_Suv =>
                  Predefined := Iir_Predefined_Ieee_Numeric_Std_Match_Suv;
               when Type_Slv =>
                  Predefined := Iir_Predefined_Ieee_Numeric_Std_Match_Slv;
               when Type_Log =>
                  raise Error;
            end case;
         else
            raise Error;
         end if;

         Set_Implicit_Definition (Decl, Predefined);
      end Handle_Std_Match;

      procedure Handle_To_01
      is
         Predefined : Iir_Predefined_Functions;
      begin
         if Arg1_Kind /= Arg_Vect
           or else Arg2_Kind /= Arg_Scal
           or else Arg2_Sign /= Type_Log
         then
            raise Error;
         end if;

         Predefined := To_01_Patterns (Arg1_Sign);

         Set_Implicit_Definition (Decl, Predefined);
      end Handle_To_01;

      procedure Handle_To_X01 (Pats : Shift_Pattern_Type) is
      begin
         if Arg1_Kind /= Arg_Vect then
            raise Error;
         end if;

         Set_Implicit_Definition (Decl, Pats (Arg1_Sign));
      end Handle_To_X01;

      procedure Handle_Shift (Pats : Shift_Pattern_Type; Sh_Sign : Sign_Kind)
      is
         Res : Iir_Predefined_Functions;
      begin
         if Arg1_Kind = Arg_Vect
           and then Arg2_Kind = Arg_Scal
           and then Arg2_Sign = Sh_Sign
         then
            case Arg1_Sign is
               when Type_Signed | Type_Unsigned =>
                  Res := Pats (Arg1_Sign);
               when others =>
                  Res := Iir_Predefined_None;
            end case;
            Set_Implicit_Definition (Decl, Res);
         end if;
      end Handle_Shift;

      procedure Handle_Find (Pats : Shift_Pattern_Type)
      is
         Res : Iir_Predefined_Functions;
      begin
         if Arg1_Kind = Arg_Vect
           and then Arg2_Kind = Arg_Scal
           and then Arg2_Sign = Type_Log
         then
            case Arg1_Sign is
               when Type_Signed | Type_Unsigned =>
                  Res := Pats (Arg1_Sign);
               when others =>
                  Res := Iir_Predefined_None;
            end case;
            Set_Implicit_Definition (Decl, Res);
         end if;
      end Handle_Find;
   begin
      Decl := Get_Declaration_Chain (Pkg_Decl);

      --  Skip a potential copyright constant.
      if Decl /= Null_Iir
        and then Get_Kind (Decl) = Iir_Kind_Constant_Declaration
        and then (Get_Base_Type (Get_Type (Decl))
                  = Vhdl.Std_Package.String_Type_Definition)
      then
         Decl := Get_Chain (Decl);
      end if;

      --  The first declaration should be type Unsigned or Unresolved_Unsigned
      if not (Decl /= Null_Iir
                and then Get_Kind (Decl) = Iir_Kind_Type_Declaration
                and then (Get_Identifier (Decl) = Name_Unsigned
                            or else
                            Get_Identifier (Decl) = Name_Unresolved_Unsigned))
      then
         raise Error;
      end if;

      Def := Get_Type_Definition (Decl);
      if Get_Kind (Def) /= Iir_Kind_Array_Type_Definition then
         raise Error;
      end if;
      Unsigned_Type := Def;

      --  The second declaration should be type Signed.
      Decl := Get_Chain (Decl);
      Decl := Skip_Implicit (Decl);
      if not (Decl /= Null_Iir
                and then Get_Kind (Decl) = Iir_Kind_Type_Declaration
                and then (Get_Identifier (Decl) = Name_Signed
                            or else
                            Get_Identifier (Decl) = Name_Unresolved_Signed))
      then
         raise Error;
      end if;

      Def := Get_Type_Definition (Decl);
      if Get_Kind (Def) /= Iir_Kind_Array_Type_Definition then
         raise Error;
      end if;
      Signed_Type := Def;

      --  For vhdl 2008, skip subtypes
      Decl := Get_Chain (Decl);
      Decl := Skip_Implicit (Decl);
      while Is_Valid (Decl) loop
         exit when Get_Kind (Decl) /= Iir_Kind_Subtype_Declaration;
         Decl := Get_Chain (Decl);
      end loop;

      --  Handle functions.
      while Is_Valid (Decl) loop
         case Get_Kind (Decl) is
            when Iir_Kind_Function_Declaration =>
               Arg1 := Get_Interface_Declaration_Chain (Decl);
               if Is_Null (Arg1) then
                  raise Error;
               end if;

               Classify_Arg (Arg1, Arg1_Sign, Arg1_Kind);
               Arg2 := Get_Chain (Arg1);
               if Is_Valid (Arg2) then
                  --  Dyadic function.
                  Classify_Arg (Arg2, Arg2_Sign, Arg2_Kind);

                  case Get_Identifier (Decl) is
                     when Name_Op_Plus =>
                        Handle_Binary (Add_Patterns);
                     when Name_Op_Minus =>
                        Handle_Binary (Sub_Patterns);
                     when Name_Op_Mul =>
                        Handle_Binary (Mul_Patterns);
                     when Name_Op_Div =>
                        Handle_Binary (Div_Patterns);
                     when Name_Mod =>
                        Handle_Binary (Mod_Patterns);
                     when Name_Rem =>
                        Handle_Binary (Rem_Patterns);
                     when Name_Op_Equality =>
                        Handle_Binary (Eq_Patterns);
                     when Name_Op_Inequality =>
                        Handle_Binary (Ne_Patterns);
                     when Name_Op_Less =>
                        Handle_Binary (Lt_Patterns);
                     when Name_Op_Less_Equal =>
                        Handle_Binary (Le_Patterns);
                     when Name_Op_Greater =>
                        Handle_Binary (Gt_Patterns);
                     when Name_Op_Greater_Equal =>
                        Handle_Binary (Ge_Patterns);
                     when Name_Minimum =>
                        Handle_Binary (Min_Patterns);
                     when Name_Maximum =>
                        Handle_Binary (Max_Patterns);
                     when Name_Op_Match_Equality =>
                        Handle_Binary (Match_Eq_Patterns);
                     when Name_Op_Match_Inequality =>
                        Handle_Binary (Match_Ne_Patterns);
                     when Name_Op_Match_Less =>
                        Handle_Binary (Match_Lt_Patterns);
                     when Name_Op_Match_Less_Equal =>
                        Handle_Binary (Match_Le_Patterns);
                     when Name_Op_Match_Greater =>
                        Handle_Binary (Match_Gt_Patterns);
                     when Name_Op_Match_Greater_Equal =>
                        Handle_Binary (Match_Ge_Patterns);
                     when Name_And =>
                        Handle_Binary (And_Patterns);
                     when Name_Or =>
                        Handle_Binary (Or_Patterns);
                     when Name_Nand =>
                        Handle_Binary (Nand_Patterns);
                     when Name_Nor =>
                        Handle_Binary (Nor_Patterns);
                     when Name_Xor =>
                        Handle_Binary (Xor_Patterns);
                     when Name_Xnor =>
                        Handle_Binary (Xnor_Patterns);
                     when Name_To_Unsigned =>
                        Handle_To_Unsigned;
                     when Name_To_Signed =>
                        Handle_To_Signed;
                     when Name_Resize =>
                        Handle_Resize;
                     when Name_Std_Match =>
                        Handle_Std_Match;
                     when Name_Shift_Left =>
                        Handle_Shift (Shl_Patterns, Type_Unsigned);
                     when Name_Shift_Right =>
                        Handle_Shift (Shr_Patterns, Type_Unsigned);
                     when Name_Sll =>
                        Handle_Shift (Sll_Patterns, Type_Signed);
                     when Name_Srl =>
                        Handle_Shift (Srl_Patterns, Type_Signed);
                     when Name_Sla =>
                        Handle_Shift (Sla_Patterns, Type_Signed);
                     when Name_Sra =>
                        Handle_Shift (Sra_Patterns, Type_Signed);
                     when Name_Rol =>
                        Handle_Shift (Rol_Patterns, Type_Signed);
                     when Name_Ror =>
                        Handle_Shift (Ror_Patterns, Type_Signed);
                     when Name_Rotate_Left =>
                        Handle_Shift (Rotate_Left_Patterns, Type_Unsigned);
                     when Name_Rotate_Right =>
                        Handle_Shift (Rotate_Right_Patterns, Type_Unsigned);
                     when Name_Find_Leftmost =>
                        Handle_Find (Leftmost_Patterns);
                     when Name_Find_Rightmost =>
                        Handle_Find (Rightmost_Patterns);
                     when Name_To_01 =>
                        Handle_To_01;
                     when others =>
                        null;
                  end case;
               else
                  --  Monadic function.
                  case Get_Identifier (Decl) is
                     when Name_Op_Minus =>
                        Handle_Unary (Neg_Patterns);
                     when Name_Not =>
                        Handle_Unary (Not_Patterns);
                     when Name_Abs =>
                        Handle_Unary (Abs_Patterns);
                     when Name_To_Integer =>
                        Handle_To_Integer;
                     when Name_And =>
                        Handle_Unary (Red_And_Patterns);
                     when Name_Nand =>
                        Handle_Unary (Red_Nand_Patterns);
                     when Name_Or =>
                        Handle_Unary (Red_Or_Patterns);
                     when Name_Nor =>
                        Handle_Unary (Red_Nor_Patterns);
                     when Name_Xor =>
                        Handle_Unary (Red_Xor_Patterns);
                     when Name_Xnor =>
                        Handle_Unary (Red_Xnor_Patterns);
                     when Name_To_X01 =>
                        Handle_To_X01 (To_X01_Patterns);
                     when Name_To_X01Z =>
                        Handle_To_X01 (To_X01z_Patterns);
                     when Name_To_UX01 =>
                        Handle_To_X01 (To_Ux01_Patterns);
                     when Name_Is_X =>
                        Handle_To_X01 (Is_X_Patterns);
                     when Name_To_Bstring =>
                        null;
                     when Name_To_Ostring =>
                        Handle_To_X01 (To_Ostring_Patterns);
                     when Name_To_Hstring =>
                        Handle_To_X01 (To_Hstring_Patterns);
                     when others =>
                        null;
                  end case;
               end if;

            when Iir_Kind_Non_Object_Alias_Declaration
              | Iir_Kind_Procedure_Declaration =>
               null;

            when others =>
               raise Error;
         end case;
         Decl := Get_Chain (Decl);
      end loop;
   end Extract_Declarations;

   procedure Extract_Std_Declarations (Pkg : Iir_Package_Declaration) is
   begin
      Numeric_Std_Pkg := Pkg;

      Extract_Declarations
        (Pkg, Pkg_Std, Numeric_Std_Unsigned_Type, Numeric_Std_Signed_Type);
   exception
      when Error =>
         Error_Msg_Sem (+Pkg, "package ieee.numeric_std is ill-formed");
         Numeric_Std_Pkg := Null_Iir;
         Numeric_Std_Unsigned_Type := Null_Iir;
         Numeric_Std_Signed_Type := Null_Iir;
   end Extract_Std_Declarations;

   procedure Extract_Bit_Declarations (Pkg : Iir_Package_Declaration) is
   begin
      Numeric_Bit_Pkg := Pkg;

      Extract_Declarations
        (Pkg, Pkg_Bit, Numeric_Bit_Unsigned_Type, Numeric_Bit_Signed_Type);
   exception
      when Error =>
         Error_Msg_Sem (+Pkg, "package ieee.numeric_bit is ill-formed");
         Numeric_Bit_Pkg := Null_Iir;
         Numeric_Bit_Unsigned_Type := Null_Iir;
         Numeric_Bit_Signed_Type := Null_Iir;
   end Extract_Bit_Declarations;
end Vhdl.Ieee.Numeric;
