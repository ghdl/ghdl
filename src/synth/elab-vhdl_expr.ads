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

with Types; use Types;

with Vhdl.Nodes; use Vhdl.Nodes;

with Elab.Vhdl_Context; use Elab.Vhdl_Context;
with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;

package Elab.Vhdl_Expr is
   --  For a static value V, return the value.
   function Get_Static_Discrete (V : Valtyp) return Int64;

   --  Return the memory (as a memtyp) of static value V.
   function Get_Value_Memtyp (V : Valtyp) return Memtyp;

   --  Return the bounds of a one dimensional array/vector type and the
   --  width of the element.
   procedure Get_Onedimensional_Array_Bounds
     (Typ : Type_Acc; Bnd : out Bound_Type; El_Typ : out Type_Acc);

   --  Create an array subtype from bound BND.
   function Create_Onedimensional_Array_Subtype
     (Btyp : Type_Acc; Bnd : Bound_Type) return Type_Acc;

   --  Return the type of EXPR without evaluating it.
   function Exec_Type_Of_Object (Syn_Inst : Synth_Instance_Acc; Expr : Node)
                                return Type_Acc;

   procedure Exec_Assignment_Prefix (Syn_Inst : Synth_Instance_Acc;
                                     Pfx : Node;
                                     Dest_Base : out Valtyp;
                                     Dest_Typ : out Type_Acc;
                                     Dest_Off : out Value_Offsets);

   function Exec_Name (Syn_Inst : Synth_Instance_Acc; Name : Node)
                      return Valtyp;

   --  Synthesize EXPR.  The expression must be self-constrained.
   --  If EN is not No_Net, the execution is controlled by EN.  This is used
   --  for assertions and checks.
   function Exec_Expression
     (Syn_Inst : Synth_Instance_Acc; Expr : Node) return Valtyp;

   --  Same as Synth_Expression, but the expression may be constrained by
   --  EXPR_TYPE.
   function Exec_Expression_With_Type (Syn_Inst : Synth_Instance_Acc;
                                       Expr : Node;
                                       Expr_Type : Type_Acc) return Valtyp;

   --  Use base type of EXPR to synthesize EXPR.  Useful when the type of
   --  EXPR is defined by itself or a range.
   function Exec_Expression_With_Basetype (Syn_Inst : Synth_Instance_Acc;
                                           Expr : Node) return Valtyp;

   --  Subtype conversion.
   function Exec_Subtype_Conversion (Vt : Valtyp;
                                     Dtype : Type_Acc;
                                     Bounds : Boolean;
                                     Loc : Node)
                                    return Valtyp;

end Elab.Vhdl_Expr;
