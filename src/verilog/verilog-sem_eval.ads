--  Verilog semantic analyzer (expressions folder)
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
--  along with this program.  If not, see <gnu.org/licenses>.

with Types; use Types;
with Verilog.Nodes; use Verilog.Nodes;

package Verilog.Sem_Eval is
   --  Analyze and evaluate expressions.

   --  Used for enumeration literals, parameters...
   function Sem_Constant_Expression (Expr : Node; Atype : Node) return Node;

   --  Used for array bounds, repetitions.
   function Sem_Constant_Integer_Expression (Expr : Node) return Int32;
end Verilog.Sem_Eval;
