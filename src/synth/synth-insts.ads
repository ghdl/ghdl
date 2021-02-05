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

with Vhdl.Nodes; use Vhdl.Nodes;

with Synth.Context; use Synth.Context;
with Synth.Flags; use Synth.Flags;

package Synth.Insts is
   --  Create the declaration of the top entity.
   procedure Synth_Top_Entity (Global_Instance : Synth_Instance_Acc;
                               Arch : Node;
                               Config : Node;
                               Encoding : Name_Encoding;
                               Inst : out Synth_Instance_Acc);

   --  Synthesize the top entity and all the sub-modules.
   procedure Synth_All_Instances;

   --  Apply block configuration CFG to BLK.
   --  Must be done before synthesis of BLK.
   --  The synthesis of BLK will clear all configuration of it.
   procedure Apply_Block_Configuration (Cfg : Node; Blk : Node);

   procedure Synth_Design_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node);
   procedure Synth_Blackbox_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node);

   procedure Synth_Component_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node);
end Synth.Insts;
