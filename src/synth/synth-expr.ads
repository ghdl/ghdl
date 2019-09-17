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

with Netlists; use Netlists;
with Netlists.Utils; use Netlists.Utils;

with Synth.Source;
with Synth.Values; use Synth.Values;
with Synth.Context; use Synth.Context;
with Vhdl.Nodes; use Vhdl.Nodes;

package Synth.Expr is
   procedure Set_Location (N : Net; Loc : Node);
   pragma Inline (Set_Location);

   function Synth_Subtype_Conversion
     (Val : Value_Acc; Dtype : Type_Acc; Loc : Source.Syn_Src)
     return Value_Acc;

   function Get_Const_Discrete (V : Value_Acc) return Int64;

   procedure From_Std_Logic (Enum : Int64; Val : out Uns32; Zx : out Uns32);
   procedure From_Bit (Enum : Int64; Val : out Uns32);
   procedure To_Logic
     (Enum : Int64; Etype : Type_Acc; Val : out Uns32; Zx : out Uns32);

   --  Try to match: clk'event and clk = X
   --            or: clk = X and clk'event
   --  where X is '0' or '1'.
   function Synth_Clock_Edge
     (Syn_Inst : Synth_Instance_Acc; Left, Right : Node) return Net;

   function Bit_Extract (Val : Value_Acc; Off : Uns32; Loc : Node)
                        return Value_Acc;

   function Concat_Array (Arr : Net_Array_Acc) return Net;

   function Synth_Expression_With_Type
     (Syn_Inst : Synth_Instance_Acc; Expr : Node; Expr_Type : Node)
     return Value_Acc;

   function Synth_Expression (Syn_Inst : Synth_Instance_Acc; Expr : Node)
                             return Value_Acc;

   function Synth_Bounds_From_Range (Syn_Inst : Synth_Instance_Acc;
                                     Atype : Node) return Bound_Type;

   function Synth_Array_Bounds (Syn_Inst : Synth_Instance_Acc;
                                Atype : Node;
                                Dim : Natural) return Bound_Type;

   function Synth_Discrete_Range_Expression
     (L : Int64; R : Int64; Dir : Iir_Direction) return Discrete_Range_Type;
   function Synth_Discrete_Range_Expression
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Discrete_Range_Type;
   function Synth_Float_Range_Expression
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Float_Range_Type;

   procedure Synth_Discrete_Range (Syn_Inst : Synth_Instance_Acc;
                                   Bound : Node;
                                   Rng : out Discrete_Range_Type;
                                   W : out Width);

   --  Convert index IDX in PFX to an offset.  LOC is used in case of error.
   function Index_To_Offset (Bnd : Bound_Type; Idx : Int64; Loc : Node)
                            return Uns32;

   procedure Synth_Slice_Suffix (Syn_Inst : Synth_Instance_Acc;
                                 Name : Node;
                                 Pfx_Bnd : Bound_Type;
                                 Res_Bnd : out Bound_Type;
                                 Inp : out Net;
                                 Step : out Uns32;
                                 Off : out Int32;
                                 Wd : out Uns32);

   procedure Synth_Indexed_Name (Syn_Inst : Synth_Instance_Acc;
                                 Name : Node;
                                 Pfx_Type : Type_Acc;
                                 Voff : out Net;
                                 Mul : out Uns32;
                                 Off : out Uns32;
                                 W : out Width);
end Synth.Expr;
