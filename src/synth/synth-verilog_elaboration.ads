--  Verilog elaboration for synthesis
--  Copyright (C) 2023 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GHDL; see the file COPYING.  If not, see:
--  <http://www.gnu.org/licenses>.

with Verilog.Nodes; use Verilog.Nodes;

with Synth.Verilog_Context; use Synth.Verilog_Context;

package Synth.Verilog_Elaboration is
   procedure Elaborate_Global;

   --  Allocate a scope for the parameters of module instance N and compute
   --  the parameters.
   function Elaborate_Sub_Instance_Params
     (Parent_Inst : Synth_Instance_Acc; Module : Node)
     return Synth_Instance_Acc;

   --  Complete sub instance elaboration.
   procedure Elaborate_Sub_Instance_Complete
     (Module : Node; Inst : Synth_Instance_Acc);
end Synth.Verilog_Elaboration;
