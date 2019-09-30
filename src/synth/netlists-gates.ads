--  Gates declaration
--  Copyright (C) 2017 Tristan Gingold
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

package Netlists.Gates is
   --  Id 0 is None
   --  Id 1 is Free
   --  Id 2 is top design

   --  Dyadic gates.  Inputs and output have the same width.
   Id_And  : constant Module_Id := 3;
   Id_Or   : constant Module_Id := 4;
   Id_Xor  : constant Module_Id := 5;
   Id_Nand : constant Module_Id := 6;
   Id_Nor  : constant Module_Id := 7;
   Id_Xnor : constant Module_Id := 8;

   Id_Add : constant Module_Id := 9;
   Id_Sub : constant Module_Id := 10;
   Id_Umul : constant Module_Id := 11;
   Id_Smul : constant Module_Id := 12;
   Id_Udiv : constant Module_Id := 13;
   Id_Sdiv : constant Module_Id := 14;
   Id_Umod : constant Module_Id := 15;
   Id_Smod : constant Module_Id := 16;

   subtype Dyadic_Module_Id is Module_Id range Id_And .. Id_Smod;

   --  Logical and arithmetic shifts.
   --  FIXME: clarify right operand: width, large values
   Id_Lsl : constant Module_Id := 20;
   Id_Lsr : constant Module_Id := 21;
   Id_Asr : constant Module_Id := 22;

   subtype Shift_Module_Id is Module_Id range Id_Lsl .. Id_Asr;

   --  Rotations.
   --  FIXME: clarify right operand.
   Id_Rol : constant Module_Id := 23;
   Id_Ror : constant Module_Id := 24;

   subtype Rotate_Module_Id is Module_Id range Id_Lsl .. Id_Asr;

   subtype Shift_Rotate_Module_Id is Module_Id range Id_Lsl .. Id_Ror;

   Id_Not : constant Module_Id := 25;
   Id_Neg : constant Module_Id := 26;

   subtype Monadic_Module_Id is Module_Id range Id_Not .. Id_Neg;

   Id_Eq  : constant Module_Id := 27;
   Id_Ne  : constant Module_Id := 28;
   Id_Ule : constant Module_Id := 29;
   Id_Sle : constant Module_Id := 30;
   Id_Ult : constant Module_Id := 31;
   Id_Slt : constant Module_Id := 32;
   Id_Uge : constant Module_Id := 33;
   Id_Sge : constant Module_Id := 34;
   Id_Ugt : constant Module_Id := 35;
   Id_Sgt : constant Module_Id := 36;

   subtype Compare_Module_Id is Module_Id range Id_Eq .. Id_Sgt;

   Id_Red_And : constant Module_Id := 37;
   Id_Red_Or  : constant Module_Id := 38;

   subtype Reduce_Module_Id is Module_Id range Id_Red_And .. Id_Red_Or;

   Id_Concat2 : constant Module_Id := 40;
   Id_Concat3 : constant Module_Id := 41;
   Id_Concat4 : constant Module_Id := 42;

   subtype Concat_Module_Id is Module_Id range Id_Concat2 .. Id_Concat4;

   --  Concatenation with N inputs.
   Id_Concatn : constant Module_Id := 43;

   --  Inputs: s, i0, i1
   --  Output: o
   Id_Mux2 : constant Module_Id := 44;
   --  Inputs: s, i0, i1, s2, s3
   --  Output: o
   Id_Mux4 : constant Module_Id := 45;

   subtype Mux_Module_Id is Module_Id range Id_Mux2 .. Id_Mux4;

   --  Like a wire: the output is equal to the input, but could be elimited
   --  at any time.  Isignal has an initial value.
   Id_Signal  : constant Module_Id := 46;
   Id_Isignal : constant Module_Id := 47;
   Id_Output  : constant Module_Id := 48;
   Id_Port    : constant Module_Id := 49;

   --  Note: initial values must be constant nets.
   --
   --  A simple D flip-flop.  The D input is stored on a rising edge of CLK.
   --  Q is the output.  For falling edge dff, use a NOT gate on the CLK
   --  input.
   --  Inputs: CLK, D
   --  Output: Q
   Id_Dff   : constant Module_Id := 50;

   --  A DFF with an asynchronous reset.  Note that the asynchronous reset
   --  has priority over the clock.  When RST is asserted, the value is
   --  set to RST_VAL.
   --  Inputs: CLK, D, RST, RST_VAL
   --  Output: Q
   Id_Adff  : constant Module_Id := 51;

   --  A simple DFF with an initial value (must be constant).  This is
   --  for FPGAs.
   Id_Idff  : constant Module_Id := 52;
   --  A DFF with an asynchronous reset and an initial value.
   Id_Iadff : constant Module_Id := 53;

   --  Width change: truncate or extend.  Sign is know in order to possibly
   --  detect loss of value.
   Id_Utrunc : constant Module_Id := 64;
   Id_Strunc : constant Module_Id := 65;
   Id_Uextend : constant Module_Id := 66;
   Id_Sextend : constant Module_Id := 67;

   subtype Truncate_Module_Id is Module_Id range Id_Utrunc .. Id_Strunc;
   subtype Extend_Module_Id is Module_Id range Id_Uextend .. Id_Sextend;

   --  Extract a bit or a slice at a constant offset.
   --  OUT := IN0[OFF+WD-1:OFF]
   Id_Extract : constant Module_Id := 68;

   --  OUT := IN0[IN1*STEP+OFF+WD-1:IN1*STEP+OFF]
   Id_Dyn_Extract : constant Module_Id := 69;

   --  Like Insert but for dynamic values.
   --  T := IN0
   --  T [IN2*STEP+OFF+WD-1:IN2*STEP+OFF] := IN1
   --  OUT := T
   Id_Dyn_Insert : constant Module_Id := 70;

   --  Positive/rising edge detector.  This is a pseudo gate.
   --  A negative edge detector can be made using by negating the clock before
   --  the detector.
   Id_Edge : constant Module_Id := 71;

   --  Input signal must always be true.
   Id_Assert : constant Module_Id := 72;
   Id_Assume : constant Module_Id := 73;
   Id_Cover : constant Module_Id := 74;

   --  Constants are gates with only one constant output.  There are multiple
   --  kind of constant gates: for small width, the value is stored as a
   --  parameter, possibly signed or unsigned extended.
   Id_Const_UB32 : constant Module_Id := 96;
   Id_Const_SB32 : constant Module_Id := 97;
   Id_Const_UL32 : constant Module_Id := 98;
   Id_Const_UB64 : constant Module_Id := 99;
   Id_Const_UL64 : constant Module_Id := 100;
   Id_Const_X : constant Module_Id := 101;
   Id_Const_Z : constant Module_Id := 102;
   Id_Const_0 : constant Module_Id := 103;

   --  Should we keep them ?
   pragma Unreferenced (Id_Const_UB64, Id_Const_UL64);

   --  Large width.
   Id_Const_Bit : constant Module_Id := 104;
   Id_Const_Log : constant Module_Id := 105;

   subtype Constant_Module_Id is Module_Id range Id_Const_UB32 .. Id_Const_Log;

   --  Id 128 is the first user id.
end Netlists.Gates;
