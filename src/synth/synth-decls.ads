--  Create declarations for synthesis.
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

with Vhdl.Nodes; use Vhdl.Nodes;

with Synth.Context; use Synth.Context;
with Synth.Objtypes; use Synth.Objtypes;

package Synth.Decls is
   --  Get the type of DECL iff it is standalone (not an already existing
   --  subtype).
   function Get_Declaration_Type (Decl : Node) return Node;

   function Synth_Array_Subtype_Indication
     (Syn_Inst : Synth_Instance_Acc; Atype : Node) return Type_Acc;

   procedure Synth_Subtype_Indication
     (Syn_Inst : Synth_Instance_Acc; Atype : Node);
   function Synth_Subtype_Indication
     (Syn_Inst : Synth_Instance_Acc; Atype : Node) return Type_Acc;

   --  Elaborate the type of DECL.
   procedure Synth_Declaration_Type
     (Syn_Inst : Synth_Instance_Acc; Decl : Node);

   procedure Synth_Declaration (Syn_Inst : Synth_Instance_Acc;
                                Decl : Node;
                                Is_Subprg : Boolean;
                                Last_Type : in out Node);

   procedure Synth_Declarations (Syn_Inst : Synth_Instance_Acc;
                                 Decls : Iir;
                                 Is_Subprg : Boolean := False);

   procedure Finalize_Declarations (Syn_Inst : Synth_Instance_Acc;
                                    Decls : Iir;
                                    Is_Subprg : Boolean := False);

   procedure Synth_Package_Declaration
     (Parent_Inst : Synth_Instance_Acc; Pkg : Node);
   procedure Synth_Package_Body
     (Parent_Inst : Synth_Instance_Acc; Pkg : Node; Bod : Node);

   procedure Synth_Generics_Association (Sub_Inst : Synth_Instance_Acc;
                                         Syn_Inst : Synth_Instance_Acc;
                                         Inter_Chain : Node;
                                         Assoc_Chain : Node);

   procedure Synth_Package_Instantiation
     (Parent_Inst : Synth_Instance_Acc; Pkg : Node);
end Synth.Decls;
