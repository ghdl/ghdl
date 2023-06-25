--  Verilog semantic analyzer (instances)
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

package Verilog.Sem_Instances is
   --  Clone module in instances of CHAIN nodes.
   procedure Instantiate_Design (Chain : Node);

   function Instantiate_Parameters (Params : Node) return Node;

   --  Finish the instantiation of KLASS.  Only parameters are set.
   procedure Instantiate_Class (Klass : Node; Gen_Class : Node);

   function Instantiate_Generate_Block
     (Items : Node; Old_Parent : Node; New_Parent : Node) return Node;

   type Complete_Foreign_Module_Acc is access procedure (N : Node);

   Complete_Foreign_Module : Complete_Foreign_Module_Acc;
end Verilog.Sem_Instances;
