--  Verilog semantic analyzer (upward references)
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

--  1800-2017 23.8 Upwards name referencing

package Verilog.Sem_Upwards is
   --  Start with the root instance.
   --  Add ROOT and the submodules.
   procedure Init (Root : Node);

   --  Enter within a scope.
   --  N must be either a subroutine name, a module, program or interface
   --  instance name or a generate block name.
   --  Add the scopes within N.  If N is an instance (module instance,
   --  program instance or interface instance), the name of the module,
   --  program or interface is also added.
   --  There is no check for duplicated names, this is done by the normal
   --  namespace rules (cf sem_scopes).
   procedure Enter_Scope (N : Node);

   --  Leave the last entered scope.  Remove them from the table.
   procedure Leave_Scope;

   --  Find the scope named ID.  Return it or Null_Node if no such scope.
   function Find_Scope (Id : Name_Id) return Node;
end Verilog.Sem_Upwards;
