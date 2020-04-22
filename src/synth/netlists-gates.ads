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
   Id_Umin : constant Module_Id := 11;
   Id_Smin : constant Module_Id := 12;
   Id_Umax : constant Module_Id := 13;
   Id_Smax : constant Module_Id := 14;
   Id_Umul : constant Module_Id := 15;
   Id_Smul : constant Module_Id := 16;
   Id_Udiv : constant Module_Id := 17;
   Id_Sdiv : constant Module_Id := 18;
   Id_Umod : constant Module_Id := 19;
   Id_Smod : constant Module_Id := 20;
   Id_Srem : constant Module_Id := 21;

   subtype Dyadic_Module_Id is Module_Id range Id_And .. Id_Srem;

   Id_Not : constant Module_Id := 22;
   Id_Neg : constant Module_Id := 23;
   Id_Abs : constant Module_Id := 24;

   subtype Monadic_Module_Id is Module_Id range Id_Not .. Id_Abs;

   --  Logical and arithmetic shifts.
   --  FIXME: clarify right operand: width, large values
   Id_Lsl : constant Module_Id := 25;
   Id_Lsr : constant Module_Id := 26;
   Id_Asr : constant Module_Id := 27;

   subtype Shift_Module_Id is Module_Id range Id_Lsl .. Id_Asr;

   --  Rotations.
   --  FIXME: clarify right operand.
   Id_Rol : constant Module_Id := 28;
   Id_Ror : constant Module_Id := 29;

   subtype Rotate_Module_Id is Module_Id range Id_Lsl .. Id_Asr;

   subtype Shift_Rotate_Module_Id is Module_Id range Id_Lsl .. Id_Ror;

   Id_Eq  : constant Module_Id := 30;
   Id_Ne  : constant Module_Id := 31;
   Id_Ule : constant Module_Id := 32;
   Id_Sle : constant Module_Id := 33;
   Id_Ult : constant Module_Id := 34;
   Id_Slt : constant Module_Id := 35;
   Id_Uge : constant Module_Id := 36;
   Id_Sge : constant Module_Id := 37;
   Id_Ugt : constant Module_Id := 38;
   Id_Sgt : constant Module_Id := 39;

   subtype Compare_Module_Id is Module_Id range Id_Eq .. Id_Sgt;

   Id_Red_And : constant Module_Id := 40;
   Id_Red_Or  : constant Module_Id := 41;

   subtype Reduce_Module_Id is Module_Id range Id_Red_And .. Id_Red_Or;

   Id_Concat2 : constant Module_Id := 42;
   Id_Concat3 : constant Module_Id := 43;
   Id_Concat4 : constant Module_Id := 44;

   subtype Concat_Module_Id is Module_Id range Id_Concat2 .. Id_Concat4;

   --  Concatenation with N inputs.
   Id_Concatn : constant Module_Id := 45;

   --  Inputs: s, i0, i1
   --  Output: o
   Id_Mux2 : constant Module_Id := 46;
   --  Inputs: s, i0, i1, s2, s3
   --  Output: o
   Id_Mux4 : constant Module_Id := 47;

   subtype Mux_Module_Id is Module_Id range Id_Mux2 .. Id_Mux4;

   --  Like a wire: the output is equal to the input, but could be elimited
   --  at any time.  Isignal has an initial value.
   --
   --  Id_Output are inserted at the beginning because a module output cannot
   --  be read.  At the end, this is not an issue because an output is driven
   --  by a gate (and thus the value of the output could be read), but that
   --  driving value may not be available early enough.
   --  Id_Ioutput is an output with an initial value.
   Id_Signal  : constant Module_Id := 48;
   Id_Isignal : constant Module_Id := 49;
   Id_Output  : constant Module_Id := 50;
   Id_Ioutput : constant Module_Id := 51;
   Id_Port    : constant Module_Id := 52;

   --  Id_Inout is a virtual gate used to fit inout direction into the netlist
   --  model which has only inputs and outputs.
   --  It is virtual because it doesn't perform any computation.
   --  Its output 1 must always be connected to an inout port of the module.
   --  (more precisely: to an input port marked as inout of the self instance).
   --  If input 0 is connected, it is a driver to the inout port.
   --  The current value of the inout port can be read from output 0.
   --
   --  Inputs:  0: value to be assigned to the port
   --  Outputs: 0: value of the port
   --           1: direct and only connection to the port
   Id_Inout   : constant Module_Id := 53;

   --  Note: initial values must be constant nets.
   --
   --  A simple D flip-flop.  The D input is stored on a rising edge of CLK.
   --  Q is the output.  For falling edge dff, use a NOT gate on the CLK
   --  input.
   --  Inputs:  0: CLK
   --           1: D
   --  Output:  0: Q
   Id_Dff   : constant Module_Id := 56;

   --  A DFF with an asynchronous reset.  Note that the asynchronous reset
   --  has priority over the clock.  When RST is asserted, the value is
   --  set to RST_VAL.
   --  Inputs:  0: CLK
   --           1: D
   --           2: RST
   --           3: RST_VAL
   --  Output:  0: Q
   Id_Adff  : constant Module_Id := 57;

   --  A simple DFF with an initial value (must be constant).  This is
   --  for FPGAs.
   --  Inputs:  0: CLK
   --           1: D
   --           2: INIT (initial value)
   --  Output:  0: Q
   Id_Idff  : constant Module_Id := 58;

   --  A DFF with an asynchronous reset and an initial value.
   --  Inputs:  0: CLK
   --           1: D
   --           2: RST
   --           3: RST_VAL
   --           4: INIT (initial value)
   --  Output:  0: Q
   Id_Iadff : constant Module_Id := 59;

   --  Multi clock dff.  ELSE is the output of the next DFF.
   --  Inputs:  0: CLK
   --           1: D
   --           2: ELSE
   --  Output:  0: Q
   Id_Mdff : constant Module_Id := 60;

   --  Multi clock dff with initial value.  ELSE is the output of the next DFF.
   --  Inputs:  0: CLK
   --           1: D
   --           2: ELSE
   --           3: Init
   --  Output:  0: Q
   Id_Midff : constant Module_Id := 61;

   --  Temporary gate, O = I
   Id_Nop : constant Module_Id := 62;

   --  Tri state buffer.
   --  Inputs:  0: D
   --           1: EN
   --  Outputs: 0: O
   --  O <= EN ? O : 'Z'
   Id_Tri : constant Module_Id := 63;

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

   --  OUT := IN0[IN1+OFF+WD-1:IN1+OFF]
   --  Inputs: MEM (the memory)
   --          IDX (then index)
   --  Param0: offset
   Id_Dyn_Extract : constant Module_Id := 69;

   --  Like Insert but for dynamic values.
   --  Input0: the memory
   --  Input1: the value to insert
   --  Input2: the index.
   --  Param0: offset
   --  T := IN0
   --  T [IN2+OFF+WD-1:IN2+OFF] := IN1
   --  OUT := T
   Id_Dyn_Insert : constant Module_Id := 70;

   --  Like Dyn_Insert but with an enable input.
   --  Input3: enable
   Id_Dyn_Insert_En : constant Module_Id := 71;

   subtype Dyn_Insert_Module_Id is
     Module_Id range Id_Dyn_Insert .. Id_Dyn_Insert_En;

   --  Input0: index
   --  Param0: step
   --  Param1: max
   --  OUT := IN0 * STEP,  IN0 < MAX
   Id_Memidx : constant Module_Id := 72;

   --  OUT := IN0 + IN1, size extension (max of inputs width).
   Id_Addidx : constant Module_Id := 73;

   --  TODO:
   --  Id_Addidx_Cst : constant Module_Id := XX;

   --  Represent a memory with a fixed size.
   --  This is not a regular gate as it has only one output, PORTS.
   --  The width of the output is the size (in bits) of the memory.
   --  The PORTS links to the first read or write port.  There must be only
   --  one connection.  The order is important as it defines the order of
   --  actions.
   --  Outputs: PORTS
   Id_Memory : constant Module_Id := 74;

   --  Same as Id_Memory but with an initial value.
   --  Input: INIT
   Id_Memory_Init : constant Module_Id := 75;

   --  Asynchronous memory read port.
   --  Inputs:  PPORT  (previous memory port)
   --           ADDR
   --  Outputs: NPORT  (next memory port)
   --           DATA
   Id_Mem_Rd : constant Module_Id := 76;

   --  Synchronous memory read port.
   --  Inputs:  PPORT (previous memory port)
   --           ADDR
   --           CLK
   --           EN
   --  Outputs: NPORT (next memory port)
   --           DATA
   Id_Mem_Rd_Sync : constant Module_Id := 77;

   --  Synchronous memory write port
   --  Inputs:  PPORT (previous memory port)
   --           ADDR
   --           CLK
   --           EN
   --           DATA
   --  Outputs: NPORT (next memory port)
   Id_Mem_Wr_Sync : constant Module_Id := 78;

   --  Virtual gate to gather 2 dffs of a multiport memory.
   Id_Mem_Multiport : constant Module_Id := 79;

   --  Positive/rising edge and negative/falling edge detector.
   --  These are pseudo gates.
   Id_Posedge : constant Module_Id := 80;
   Id_Negedge : constant Module_Id := 81;
   subtype Edge_Module_Id is Module_Id range Id_Posedge .. Id_Negedge;

   --  Input signal must always be true.
   Id_Assert : constant Module_Id := 82;
   Id_Assume : constant Module_Id := 83;

   --  Input is true when a sequence is covered.
   Id_Cover : constant Module_Id := 84;
   --  Use to cover the precedent of an assertion.
   Id_Assert_Cover : constant Module_Id := 85;

   --  Formal gates.
   Id_Allconst : constant Module_Id := 90;
   Id_Anyconst : constant Module_Id := 91;
   Id_Allseq : constant Module_Id := 92;
   Id_Anyseq : constant Module_Id := 93;

   subtype Formal_Module_Id is Module_Id range Id_Allconst .. Id_Anyseq;

   --  A resolver for tri-state.  The two inputs (tri or resolver gates) are
   --  connected together and to the output.
   --  I0  I1  O
   --  Z   Z   Z
   --  Z   v1  v1
   --  v0  Z   v0
   --  v0  v1  vo   Ok if v0 = v1, error if v0 /= v1.
   Id_Resolver : constant Module_Id := 94;

   --  Constants are gates with only one constant output.  There are multiple
   --  kind of constant gates: for small width, the value is stored as a
   --  parameter, possibly signed or unsigned extended.
   Id_Const_UB32 : constant Module_Id := 112;
   Id_Const_SB32 : constant Module_Id := 113;
   Id_Const_UL32 : constant Module_Id := 114;
   Id_Const_UB64 : constant Module_Id := 115;
   Id_Const_UL64 : constant Module_Id := 116;
   Id_Const_X : constant Module_Id := 117;
   Id_Const_Z : constant Module_Id := 118;
   Id_Const_0 : constant Module_Id := 119;

   --  Should we keep them ?
   pragma Unreferenced (Id_Const_UB64, Id_Const_UL64);

   --  Large width.
   --  For Const_Bit: param N is for bits 32*N .. 32*N+31
   --  For Const_Log: param 2*N   is for 0/1 of bits 32*N .. 32*N+31
   --                 param 2*N+1 is for Z/X of bits 32*N .. 32*N+31
   Id_Const_Bit : constant Module_Id := 120;
   Id_Const_Log : constant Module_Id := 121;

   subtype Constant_Module_Id is Module_Id range Id_Const_UB32 .. Id_Const_Log;

   --  Id 128 is the first user id.
end Netlists.Gates;
