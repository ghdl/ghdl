--  Disp a netlist in vhdl using the original entity.
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

with Netlists; use Netlists;
with Vhdl.Nodes; use Vhdl.Nodes;
with Synth.Context; use Synth.Context;

package Synth.Disp_Vhdl is
   --  Disp ENT (like the original text) and its content as a wrapper.
   procedure Disp_Vhdl_Wrapper
     (Ent : Node; Top : Module; Inst : Synth_Instance_Acc);
end Synth.Disp_Vhdl;
