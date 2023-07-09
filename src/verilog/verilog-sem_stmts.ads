--  Verilog semantic analyzer (statements)
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

with Verilog.Nodes; use Verilog.Nodes;

package Verilog.Sem_Stmts is
   procedure Sem_Statement (Stmt : Node);
   procedure Sem_Statement_Or_Null (Stmt : Node);
   procedure Sem_Statements (Parent : Node);
   procedure Sem_Subroutine_Statements (Rtn : Node);

   --  Analyze variables of a foreach-loop or foreach constraint_expression.
   procedure Sem_Foreach_Variables (Stmt : Node);

   procedure Sem_System_Function_Call (Expr : Node; Etype : Node);
end Verilog.Sem_Stmts;
