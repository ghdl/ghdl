--  Instantiation synthesis.
--  Copyright (C) 2019 Tristan Gingold
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

package Synth.Insts is
   procedure Init;
   procedure Synth_All_Instances;

   procedure Synth_Top_Entity (Global_Instance : Synth_Instance_Acc;
                               Arch : Node;
                               Config : Node;
                               Inst : out Synth_Instance_Acc);

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
