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
   --  Dyadic gates.  Inputs and output have the same width.
   Id_And  : constant Module_Id := 3;
   Id_Or   : constant Module_Id := 4;
   Id_Xor  : constant Module_Id := 5;
   Id_Nand : constant Module_Id := 6;
   Id_Nor  : constant Module_Id := 7;
   Id_Xnor : constant Module_Id := 8;

   Id_Add : constant Module_Id := 9;
   Id_Sub : constant Module_Id := 10;
   Id_Mul : constant Module_Id := 11;

   subtype Dyadic_Module_Id is Module_Id range Id_And .. Id_Mul;

   Id_Buf : constant Module_Id := 13;
   Id_Not : constant Module_Id := 14;

   Id_Neg : constant Module_Id := 15;

   subtype Monadic_Module_Id is Module_Id range Id_Buf .. Id_Neg;

   Id_Eq  : constant Module_Id := 16;
   Id_Ne  : constant Module_Id := 17;
   Id_Ule : constant Module_Id := 18;
   Id_Sle : constant Module_Id := 19;
   Id_Ult : constant Module_Id := 18;
   Id_Slt : constant Module_Id := 19;
   Id_Uge : constant Module_Id := 18;
   Id_Sge : constant Module_Id := 19;
   Id_Ugt : constant Module_Id := 18;
   Id_Sgt : constant Module_Id := 19;

   subtype Compare_Module_Id is Module_Id range Id_Eq .. Id_Sgt;

   Id_Red_And : constant Module_Id := 20;
   Id_Red_Or  : constant Module_Id := 21;

   Id_Concat2 : constant Module_Id := 22;
   Id_Concat3 : constant Module_Id := 23;
   Id_Concat4 : constant Module_Id := 24;

   subtype Concat_Module_Id is Module_Id range Id_Concat2 .. Id_Concat4;

   Id_Split2 : constant Module_Id := 25;
   Id_Split3 : constant Module_Id := 26;

   Id_Mux2 : constant Module_Id := 27;
   Id_Mux4 : constant Module_Id := 28;

   --  Like a wire: the output is equal to the input, but could be elimited
   --  at any time.  Isignal has an initial value.
   Id_Signal  : constant Module_Id := 29;
   Id_Isignal : constant Module_Id := 30;
   Id_Output  : constant Module_Id := 31;

   --  Note: initial values must be constant nets.
   Id_Dff   : constant Module_Id := 32;
   Id_Adff  : constant Module_Id := 33; --  Async reset
   Id_Idff  : constant Module_Id := 34; --  With initial value
   Id_Iadff : constant Module_Id := 35; --  With initial value, async reset

   --  Width change: truncate or extend.  Sign is know in order to possibly
   --  detect loss of value.
   Id_Utrunc : constant Module_Id := 40;
   Id_Strunc : constant Module_Id := 41;
   Id_Uextend : constant Module_Id := 42;
   Id_Sextend : constant Module_Id := 43;

   subtype Truncate_Module_Id is Module_Id range Id_Utrunc .. Id_Strunc;
   subtype Extend_Module_Id is Module_Id range Id_Uextend .. Id_Sextend;

   --  Extract a bit or a slice at a constant offset.
   Id_Extract : constant Module_Id := 44;

   --  Edge detectors.  These are pseudo gates.
   Id_Posedge : constant Module_Id := 50;
   Id_Negedge : constant Module_Id := 51;

   subtype Edge_Module_Id is Module_Id range Id_Posedge .. Id_Negedge;

   --  Constants are gates with only one constant output.  There are multiple
   --  kind of constant gates: for small width, the value is stored as a
   --  parameter, possibly signed or unsigned extended.  For large width
   --  (> 128), the value is stored in a table.
   Id_Const_UB32 : constant Module_Id := 56;
   Id_Const_SB32 : constant Module_Id := 57;
   Id_Const_UB64 : constant Module_Id := 58;
   Id_Const_SB64 : constant Module_Id := 59;
   Id_Const_UB128 : constant Module_Id := 60;
   Id_Const_SB128 : constant Module_Id := 61;
   Id_Const_UL32 : constant Module_Id := 62;
   Id_Const_SL32 : constant Module_Id := 63;
end Netlists.Gates;
