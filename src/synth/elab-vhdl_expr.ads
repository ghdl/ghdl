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

with Vhdl.Nodes; use Vhdl.Nodes;

with Elab.Vhdl_Context; use Elab.Vhdl_Context;
with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;

package Elab.Vhdl_Expr is
   --  Return the bounds of a one dimensional array/vector type and the
   --  width of the element.
   procedure Get_Onedimensional_Array_Bounds
     (Typ : Type_Acc; Bnd : out Bound_Type; El_Typ : out Type_Acc);

   --  Create an array subtype from bound BND.
   function Create_Onedimensional_Array_Subtype
     (Btyp : Type_Acc; Bnd : Bound_Type; El_Typ : Type_Acc) return Type_Acc;

   procedure Check_Matching_Bounds (L, R : Type_Acc; Loc : Node);

   --  Get the type of NAME.  No expressions are expected to be evaluated.
   function Exec_Name_Subtype (Syn_Inst : Synth_Instance_Acc; Name : Node)
                              return Type_Acc;

   --  Subtype conversion.
   function Exec_Subtype_Conversion (Vt : Valtyp;
                                     Dtype : Type_Acc;
                                     Bounds : Boolean;
                                     Loc : Node)
                                    return Valtyp;

   function Exec_String_Literal (Syn_Inst : Synth_Instance_Acc;
                                 Str : Node;
                                 Str_Typ : Type_Acc) return Valtyp;

   function Exec_Value_Attribute (Syn_Inst : Synth_Instance_Acc; Attr : Node)
                                 return Valtyp;
   function Exec_Image_Attribute (Syn_Inst : Synth_Instance_Acc; Attr : Node)
                                 return Valtyp;
   function Exec_Instance_Name_Attribute
     (Syn_Inst : Synth_Instance_Acc; Attr : Node) return Valtyp;

   function Exec_Simple_Aggregate (Syn_Inst : Synth_Instance_Acc;
                                   Aggr : Node) return Valtyp;

end Elab.Vhdl_Expr;
