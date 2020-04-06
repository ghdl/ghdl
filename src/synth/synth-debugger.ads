--  Debugging during synthesis.
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

package Synth.Debugger is
   --  If true, debugging is enabled:
   --  * call Debug_Break() before executing the next sequential statement
   --  * call Debug_Leave when a frame is destroyed.
   Flag_Need_Debug : Boolean := False;

   procedure Debug_Init (Top : Node);
   procedure Debug_Break (Inst : Synth_Instance_Acc; Stmt : Node);

   procedure Debug_Leave (Inst : Synth_Instance_Acc);

   --  To be called in case of execution error, like:
   --  * index out of bounds.
   procedure Debug_Error (Inst : Synth_Instance_Acc; Expr : Node);
end Synth.Debugger;
