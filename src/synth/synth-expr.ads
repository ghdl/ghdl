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

with Ada.Unchecked_Deallocation;

with Types; use Types;

with Netlists; use Netlists;

with Synth.Source;
with Synth.Objtypes; use Synth.Objtypes;
with Synth.Values; use Synth.Values;
with Synth.Context; use Synth.Context;
with Vhdl.Nodes; use Vhdl.Nodes;

package Synth.Expr is
   --  Perform a subtype conversion.  Check constraints.
   function Synth_Subtype_Conversion (Vt : Valtyp;
                                      Dtype : Type_Acc;
                                      Bounds : Boolean;
                                      Loc : Source.Syn_Src)
                                     return Valtyp;

   --  For a static value V, return the value.
   function Get_Static_Discrete (V : Valtyp) return Int64;

   --  Return the memory (as a memtyp) of static value V.
   function Get_Value_Memtyp (V : Valtyp) return Memtyp;

   --  Return True only if discrete value V is known to be positive or 0.
   --  False means either not positive or unknown.
   function Is_Positive (V : Valtyp) return Boolean;

   --  Return the bounds of a one dimensional array/vector type and the
   --  width of the element.
   procedure Get_Onedimensional_Array_Bounds
     (Typ : Type_Acc; Bnd : out Bound_Type; El_Typ : out Type_Acc);

   --  Create an array subtype from bound BND.
   function Create_Onedimensional_Array_Subtype
     (Btyp : Type_Acc; Bnd : Bound_Type) return Type_Acc;

   procedure From_Std_Logic (Enum : Int64; Val : out Uns32; Zx : out Uns32);
   procedure From_Bit (Enum : Int64; Val : out Uns32);
   procedure To_Logic
     (Enum : Int64; Etype : Type_Acc; Val : out Uns32; Zx : out Uns32);

   --  Try to match: clk'event and clk = X
   --            or: clk = X and clk'event
   --  where X is '0' or '1'.
   function Synth_Clock_Edge
     (Syn_Inst : Synth_Instance_Acc; Left, Right : Node) return Net;

   procedure Concat_Array (Arr : in out Net_Array; N : out Net);

   function Synth_Expression_With_Type
     (Syn_Inst : Synth_Instance_Acc; Expr : Node; Expr_Type : Type_Acc)
     return Valtyp;

   function Synth_Expression (Syn_Inst : Synth_Instance_Acc; Expr : Node)
                             return Valtyp;

   --  Use base type of EXPR to synthesize EXPR.  Useful when the type of
   --  EXPR is defined by itself or a range.
   function Synth_Expression_With_Basetype
     (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Valtyp;

   function Synth_Bounds_From_Range (Syn_Inst : Synth_Instance_Acc;
                                     Atype : Node) return Bound_Type;

   function Synth_Array_Bounds (Syn_Inst : Synth_Instance_Acc;
                                Atype : Node;
                                Dim : Dim_Type) return Bound_Type;

   function Synth_Discrete_Range_Expression
     (L : Int64; R : Int64; Dir : Iir_Direction) return Discrete_Range_Type;
   function Synth_Discrete_Range_Expression
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Discrete_Range_Type;
   function Synth_Float_Range_Expression
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Float_Range_Type;

   procedure Synth_Discrete_Range (Syn_Inst : Synth_Instance_Acc;
                                   Bound : Node;
                                   Rng : out Discrete_Range_Type);

   procedure Synth_Slice_Suffix (Syn_Inst : Synth_Instance_Acc;
                                 Name : Node;
                                 Pfx_Bnd : Bound_Type;
                                 El_Typ : Type_Acc;
                                 Res_Bnd : out Bound_Type;
                                 Inp : out Net;
                                 Off : out Value_Offsets);

   --  If VOFF is No_Net then OFF is valid, if VOFF is not No_Net then
   --  OFF is 0.
   procedure Synth_Indexed_Name (Syn_Inst : Synth_Instance_Acc;
                                 Name : Node;
                                 Pfx_Type : Type_Acc;
                                 Voff : out Net;
                                 Off : out Value_Offsets);

   --  Return the type of EXPR (an object) without evaluating it (except when
   --  needed, like bounds of a slice).
   function Synth_Type_Of_Object (Syn_Inst : Synth_Instance_Acc; Expr : Node)
                                 return Type_Acc;

   --  Conversion to logic vector.

   type Digit_Index is new Natural;
   type Logvec_Array is array (Digit_Index range <>) of Logic_32;
   type Logvec_Array_Acc is access Logvec_Array;

   procedure Free_Logvec_Array is new Ada.Unchecked_Deallocation
     (Logvec_Array, Logvec_Array_Acc);

   --  Convert W bits from OFF of VAL to a Logvec_Array.
   --  OFF and W are offset and width in bit representation.
   procedure Value2logvec (Val : Memtyp;
                           Off : Uns32;
                           W : Width;
                           Vec : in out Logvec_Array;
                           Vec_Off : in out Uns32;
                           Has_Zx : in out Boolean);
end Synth.Expr;
