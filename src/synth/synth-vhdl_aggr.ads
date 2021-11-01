--  Aggregates synthesis.
--  Copyright (C) 2020 Tristan Gingold
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
with Elab.Vhdl_Values; use Elab.Vhdl_Values;

package Synth.Vhdl_Aggr is
   --  Aggr_Type is the type from the context.
   function Synth_Aggregate (Syn_Inst : Synth_Instance_Acc;
                             Aggr : Node;
                             Aggr_Type : Type_Acc) return Valtyp;
end Synth.Vhdl_Aggr;
