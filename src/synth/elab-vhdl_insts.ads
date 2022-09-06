--  Design elaboration
--  Copyright (C) 2021 Tristan Gingold
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

package Elab.Vhdl_Insts is
   function Elab_Top_Unit (Config : Node) return Synth_Instance_Acc;

   procedure Elab_Generics_Association (Sub_Inst : Synth_Instance_Acc;
                                        Syn_Inst : Synth_Instance_Acc;
                                        Inter_Chain : Node;
                                        Assoc_Chain : Node);

   procedure Elab_Ports_Association_Type (Sub_Inst : Synth_Instance_Acc;
                                          Syn_Inst : Synth_Instance_Acc;
                                          Inter_Chain : Node;
                                          Assoc_Chain : Node);

   procedure Elab_Package_Instantiation
     (Parent_Inst : Synth_Instance_Acc; Pkg : Node);

   procedure Elab_Component_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node);
   procedure Elab_Design_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node);

   --  Apply block configuration CFG to BLK.
   --  Must be done before synthesis of BLK.
   --  The synthesis of BLK will clear all configuration of it.
   procedure Apply_Block_Configuration (Cfg : Node; Blk : Node);

   type Elab_Foreign_Instance_Acc is access
     procedure (Syn_Inst : Synth_Instance_Acc;
                Comp_Inst : Synth_Instance_Acc;
                Bind : Node;
                Module : Node);

   Elab_Foreign_Instance : Elab_Foreign_Instance_Acc;
end Elab.Vhdl_Insts;
