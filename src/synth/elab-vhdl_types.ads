--  Create declarations for synthesis.
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

package Elab.Vhdl_Types is
   --  Get the type of DECL iff it is standalone (not an already existing
   --  subtype).
   function Get_Declaration_Type (Decl : Node) return Node;

   --  True if the element subtype indication of ATYPE needs to be created.
   function Has_Element_Subtype_Indication (Atype : Node) return Boolean;

   function Synth_Discrete_Range_Expression
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Discrete_Range_Type;
   function Synth_Float_Range_Expression
     (Syn_Inst : Synth_Instance_Acc; Rng : Node) return Float_Range_Type;

   function Synth_Array_Attribute (Syn_Inst : Synth_Instance_Acc; Attr : Node)
                                  return Bound_Type;

   procedure Synth_Discrete_Range (Syn_Inst : Synth_Instance_Acc;
                                   Bound : Node;
                                   Rng : out Discrete_Range_Type);
   function Synth_Bounds_From_Range (Rng : Discrete_Range_Type)
                                    return Bound_Type;
   function Synth_Bounds_From_Range (Syn_Inst : Synth_Instance_Acc;
                                     Atype : Node) return Bound_Type;

   function Create_Bounds_From_Length
     (Bounds : Discrete_Range_Type; Len : Iir_Index32) return Bound_Type;

   function Synth_Array_Subtype_Indication
     (Syn_Inst : Synth_Instance_Acc; Atype : Node) return Type_Acc;

   procedure Synth_Subtype_Indication
     (Syn_Inst : Synth_Instance_Acc; Atype : Node);
   function Synth_Subtype_Indication
     (Syn_Inst : Synth_Instance_Acc; Atype : Node) return Type_Acc;

   procedure Elab_Type_Definition (Syn_Inst : Synth_Instance_Acc; Def : Node);
   procedure Elab_Anonymous_Type_Definition
     (Syn_Inst : Synth_Instance_Acc; Def : Node; St : Node);

   --  Exported only for Vhdl.Evaluation to create temporary types.
   function Elab_Enumeration_Type_Definition (Def : Node) return Type_Acc;
   function Elab_Scalar_Type_Definition (Def : Node; St : Node)
                                        return Type_Acc;

   --  Elaborate the type of DECL.
   function Elab_Declaration_Type
     (Syn_Inst : Synth_Instance_Acc; Decl : Node) return Type_Acc;
end Elab.Vhdl_Types;
