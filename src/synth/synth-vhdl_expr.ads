--  Expressions synthesis.
--  Copyright (C) 2017 Tristan Gingold
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

with Ada.Unchecked_Deallocation;

with Types; use Types;

with PSL.Types;
with Vhdl.Nodes; use Vhdl.Nodes;

with Elab.Vhdl_Context; use Elab.Vhdl_Context;
with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;

with Netlists; use Netlists;
with Netlists.Builders; use Netlists.Builders;

with Synth.Source;

package Synth.Vhdl_Expr is
   --  Perform a subtype conversion.  Check constraints.
   function Synth_Subtype_Conversion (Syn_Inst : Synth_Instance_Acc;
                                      Vt : Valtyp;
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

   procedure From_Std_Logic (Enum : Int64; Val : out Uns32; Zx : out Uns32);
   procedure From_Bit (Enum : Int64; Val : out Uns32);
   procedure To_Logic
     (Enum : Int64; Etype : Type_Acc; Val : out Uns32; Zx : out Uns32);

   --  Try to match: clk'event and clk = X
   --            or: clk = X and clk'event
   --  where X is '0' or '1'.
   function Synth_Clock_Edge
     (Syn_Inst : Synth_Instance_Acc; Left, Right : Node) return Net;

   procedure Concat_Array
     (Ctxt : Context_Acc; Arr : in out Net_Array; N : out Net);

   --  Hook to convert a signal to a value.
   --  If not defined, the signal are not allowed (like in expressions during
   --  elaboration).
   type Hook_Signal_Expr_Acc is access function (Val : Valtyp) return Valtyp;
   Hook_Signal_Expr : Hook_Signal_Expr_Acc;
   Hook_Quantity_Expr : Hook_Signal_Expr_Acc;

   --  Synthesize EXPR.  The expression must be self-constrained.
   --  If EN is not No_Net, the execution is controlled by EN.  This is used
   --  for assertions and checks.
   function Synth_Expression
     (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Valtyp;

   --  Same as Synth_Expression, but the expression may be constrained by
   --  EXPR_TYPE.
   function Synth_Expression_With_Type (Syn_Inst : Synth_Instance_Acc;
                                        Expr : Node;
                                        Expr_Type : Type_Acc) return Valtyp;

   --  For value signal attribute (like 'Event).
   type Hook_Attribute_Acc is access
     function (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Valtyp;
   Hook_Event_Attribute : Hook_Attribute_Acc;
   Hook_Active_Attribute : Hook_Attribute_Acc;
   Hook_Last_Value_Attribute : Hook_Attribute_Acc;
   Hook_Last_Event_Attribute : Hook_Attribute_Acc;
   Hook_Last_Active_Attribute : Hook_Attribute_Acc;
   Hook_Dot_Attribute : Hook_Attribute_Acc;

   --  Use base type of EXPR to synthesize EXPR.  Useful when the type of
   --  EXPR is defined by itself or a range.
   function Synth_Expression_With_Basetype (Syn_Inst : Synth_Instance_Acc;
                                            Expr : Node) return Valtyp;

   function Synth_Type_Conversion (Syn_Inst : Synth_Instance_Acc;
                                   Val : Valtyp;
                                   Conv_Typ : Type_Acc;
                                   Loc : Node) return Valtyp;
   function Synth_Type_Conversion
     (Syn_Inst : Synth_Instance_Acc; Conv : Node) return Valtyp;

   function Synth_PSL_Expression
     (Syn_Inst : Synth_Instance_Acc; Expr : PSL.Types.PSL_Node) return Net;

   function Synth_Array_Bounds (Syn_Inst : Synth_Instance_Acc;
                                Atype : Node;
                                Dim : Dim_Type) return Bound_Type;

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
                                 El_Typ : out Type_Acc;
                                 Voff : out Net;
                                 Off : out Value_Offsets;
                                 Error : out Boolean);

   function Synth_Name (Syn_Inst : Synth_Instance_Acc; Name : Node)
                       return Valtyp;

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
end Synth.Vhdl_Expr;
