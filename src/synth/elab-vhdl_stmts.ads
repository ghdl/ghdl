--  Elaborate statements
--  Copyright (C) 2021 Tristan Gingold
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
with Elab.Vhdl_Insts; use Elab.Vhdl_Insts;

package Elab.Vhdl_Stmts is
   procedure Elab_Concurrent_Statement (Syn_Inst : Synth_Instance_Acc;
                                        Stmt : Node;
                                        Cfgs : in out Configs_Rec);
   procedure Elab_Concurrent_Statements (Syn_Inst : Synth_Instance_Acc;
                                         Chain : Node;
                                         Cfgs : in out Configs_Rec);
end Elab.Vhdl_Stmts;
