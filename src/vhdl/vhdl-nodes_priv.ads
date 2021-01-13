--  Internal node type and operations.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with Types; use Types;

package Vhdl.Nodes_Priv is
   pragma Preelaborate (Vhdl.Nodes_Priv);

   type Node_Type is new Int32;
   for Node_Type'Size use 32;

   Null_Node : constant Node_Type := 0;
   Error_Node : constant Node_Type := 1;
end Vhdl.Nodes_Priv;
