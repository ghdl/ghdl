--  Predefined procedures
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

with Elab.Vhdl_Context; use Elab.Vhdl_Context;

with Vhdl.Nodes; use Vhdl.Nodes;

package Synth.Vhdl_Static_Proc is
   procedure Synth_Static_Procedure (Syn_Inst : Synth_Instance_Acc;
                                     Imp : Node;
                                     Loc : Node);

   type Hook_Simulation_Acc is access
     procedure (Inst : Synth_Instance_Acc; Imp : Node);
   Hook_Finish : Hook_Simulation_Acc;
end Synth.Vhdl_Static_Proc;
