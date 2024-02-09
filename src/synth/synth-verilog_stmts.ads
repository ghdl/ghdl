--  Statements synthesis for verilog
--  Copyright (C) 2023 Tristan Gingold
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

with Verilog.Nodes; use Verilog.Nodes;
with Verilog.Storages;

with Synth.Verilog_Context; use Synth.Verilog_Context;

package Synth.Verilog_Stmts is
   procedure Synth_Continuous_Assign (Inst : Synth_Instance_Acc; N : Node);
   procedure Synth_Initial (Inst : Synth_Instance_Acc; N : Node);
   procedure Synth_Always (Inst : Synth_Instance_Acc; N : Node);
   procedure Synth_Always_Comb (Inst : Synth_Instance_Acc; N : Node);

   --  Implicit assignment for net assigned with an expression.
   procedure Synth_Net_Init (Inst : Synth_Instance_Acc; N : Node);

   procedure Synth_Gate (Inst : Synth_Instance_Acc; N : Node);

   use Verilog.Storages;

   --  Assignment procedure for VPI.
   procedure Synth_Blocking_Assign_Vpi
     (Frame : Frame_Ptr; Target : Node; Value : Data_Ptr; Typ : Node);
end Synth.Verilog_Stmts;
