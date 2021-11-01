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

with Netlists; use Netlists;

package Synth.Vhdl_Decls is
   --  Return the Param_Type for ATYPE.
   function Type_To_Param_Type (Atype : Node) return Param_Type;

   --  Convert MT to a Pval.
   function Memtyp_To_Pval (Mt : Memtyp) return Pval;

   procedure Synth_Object_Alias_Declaration
     (Syn_Inst : Synth_Instance_Acc; Decl : Node);

   procedure Synth_Declaration (Syn_Inst : Synth_Instance_Acc;
                                Decl : Node;
                                Is_Subprg : Boolean;
                                Last_Type : in out Node);

   procedure Synth_Declarations (Syn_Inst : Synth_Instance_Acc;
                                 Decls : Node;
                                 Is_Subprg : Boolean := False);

   procedure Synth_Concurrent_Declaration (Syn_Inst : Synth_Instance_Acc;
                                           Decl : Node);
   procedure Synth_Concurrent_Declarations (Syn_Inst : Synth_Instance_Acc;
                                            Decls : Node);

   procedure Finalize_Declaration (Syn_Inst : Synth_Instance_Acc;
                                   Decl : Node;
                                   Is_Subprg : Boolean);
   procedure Finalize_Declarations (Syn_Inst : Synth_Instance_Acc;
                                    Decls : Node;
                                    Is_Subprg : Boolean := False);

   procedure Synth_Package_Declaration
     (Parent_Inst : Synth_Instance_Acc; Pkg : Node);
   procedure Synth_Package_Body
     (Parent_Inst : Synth_Instance_Acc; Pkg : Node; Bod : Node);

   procedure Synth_Package_Instantiation
     (Parent_Inst : Synth_Instance_Acc; Pkg : Node);
end Synth.Vhdl_Decls;
