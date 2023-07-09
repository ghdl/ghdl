--  Error handling for synthesis.
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

with Errorout; use Errorout;

with Verilog.Nodes; use Verilog.Nodes;
with Synth.Verilog_Context; use Synth.Verilog_Context;

package Synth.Verilog_Errors is
   --  The error procedures return during synthesis, but not during
   --  simulation (they are fatal).
   --  Debugger may be called.
   procedure Error_Msg_Synth (Inst : Synth_Instance_Acc;
                              Loc : Node;
                              Msg : String;
                              Arg1 : Earg_Type);
   procedure Error_Msg_Synth (Inst : Synth_Instance_Acc;
                              Loc : Node;
                              Msg : String;
                              Args : Earg_Arr := No_Eargs);

   type Verilog_Handler_Access is access procedure (Inst : Synth_Instance_Acc;
                                                    Loc : Node);
   Verilog_Debug_Handler : Verilog_Handler_Access;
end Synth.Verilog_Errors;
