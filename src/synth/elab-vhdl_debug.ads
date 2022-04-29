--  Debug utilities on elaborated design
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
with Vhdl.Nodes_Walk; use Vhdl.Nodes_Walk;

with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Context; use Elab.Vhdl_Context;

package Elab.Vhdl_Debug is
   procedure Disp_Memtyp (M : Memtyp; Vtype : Node);
   function Walk_Declarations (Cb : Walk_Cb) return Walk_Status;

   procedure Disp_Declaration_Objects
     (Instance : Synth_Instance_Acc; Decl_Chain : Iir);

   procedure Disp_Hierarchy
     (Inst : Synth_Instance_Acc; Indent : Natural; With_Objs : Boolean);
end Elab.Vhdl_Debug;
