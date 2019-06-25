--  Expressions synthesis.
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

with Types; use Types;
with Synth.Values; use Synth.Values;
with Synth.Context; use Synth.Context;
with Vhdl.Nodes; use Vhdl.Nodes;

package Synth.Expr is
   function Is_Const (Val : Value_Acc) return Boolean;
   function Get_Width (Val : Value_Acc) return Uns32;

   procedure From_Std_Logic (Enum : Int64; Val : out Uns32; Zx  : out Uns32);
   procedure From_Bit (Enum : Int64; Val : out Uns32);
   procedure To_Logic
     (Enum : Int64; Etype : Node; Val : out Uns32; Zx  : out Uns32);

   function Bit_Extract (Val : Value_Acc; Off : Uns32) return Value_Acc;

   function Synth_Expression_With_Type
     (Syn_Inst : Synth_Instance_Acc; Expr : Node; Expr_Type : Node)
     return Value_Acc;

   function Synth_Expression (Syn_Inst : Synth_Instance_Acc; Expr : Node)
                             return Value_Acc;

   function Synth_Bounds_From_Range (Syn_Inst : Synth_Instance_Acc;
                                     Atype : Node) return Value_Bound_Acc;

   function Synth_Array_Bounds (Syn_Inst : Synth_Instance_Acc;
                                Atype : Node;
                                Dim : Natural) return Value_Bound_Acc;

   function Synth_Range_Expression
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Value_Acc;

   --  Convert index IDX in PFX to an offset.  LOC is used in case of error.
   function Index_To_Offset (Pfx : Value_Acc; Idx : Int64; Loc : Node)
                            return Uns32;

   procedure Synth_Slice_Suffix (Syn_Inst : Synth_Instance_Acc;
                                 Name : Node;
                                 Pfx_Bnd : Value_Bound_Acc;
                                 Res_Bnd : out Value_Bound_Acc;
                                 Off : out Uns32);
end Synth.Expr;
