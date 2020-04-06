--  Debugging during synthesis (not enabled).
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

with Types; use Types;

package body Synth.Debugger is
   procedure Debug_Init (Top : Node) is
   begin
      null;
   end Debug_Init;

   procedure Debug_Break (Inst : Synth_Instance_Acc; Stmt : Node) is
   begin
      raise Internal_Error;
   end Debug_Break;

   procedure Debug_Leave (Inst : Synth_Instance_Acc) is
   begin
      raise Internal_Error;
   end Debug_Leave;

   procedure Debug_Error (Inst : Synth_Instance_Acc; Expr : Node) is
   begin
      null;
   end Debug_Error;
end Synth.Debugger;
