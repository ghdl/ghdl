--  Operations synthesis.
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

with Synth.Objtypes; use Synth.Objtypes;
with Synth.Context; use Synth.Context;

with Vhdl.Nodes; use Vhdl.Nodes;

package Synth.Static_Oper is
   function Synth_Static_Dyadic_Predefined (Syn_Inst : Synth_Instance_Acc;
                                            Imp : Node;
                                            Left : Memtyp;
                                            Right : Memtyp;
                                            Expr : Node) return Memtyp;
   function Synth_Static_Monadic_Predefined (Syn_Inst : Synth_Instance_Acc;
                                             Imp : Node;
                                             Operand : Memtyp;
                                             Expr : Node) return Memtyp;

   function Synth_Static_Predefined_Function_Call
     (Subprg_Inst : Synth_Instance_Acc; Expr : Node) return Memtyp;

end Synth.Static_Oper;
