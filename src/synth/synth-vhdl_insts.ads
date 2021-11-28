--  Instantiation synthesis.
--  Copyright (C) 2019 Tristan Gingold
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
with Netlists;

with Elab.Vhdl_Context; use Elab.Vhdl_Context;

with Vhdl.Nodes; use Vhdl.Nodes;

with Synth.Context; use Synth.Context;
with Synth.Flags; use Synth.Flags;

package Synth.Vhdl_Insts is
   --  Create the declaration of the top entity.
   procedure Synth_Top_Entity (Base : Base_Instance_Acc;
                               Design_Unit : Node;
                               Encoding : Name_Encoding;
                               Syn_Inst : Synth_Instance_Acc);

   --  Synthesize the top entity and all the sub-modules.
   procedure Synth_All_Instances;

   procedure Synth_Design_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node);
   procedure Synth_Blackbox_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node);

   procedure Synth_Component_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node);

   type Synth_Foreign_Module_Acc is access
     function (Base : Base_Instance_Acc;
               M : Int32;
               Vhdl_Inst : Synth_Instance_Acc;
               Vhdl_Decl : Node) return Netlists.Module;

   Synth_Foreign_Module : Synth_Foreign_Module_Acc;
end Synth.Vhdl_Insts;
