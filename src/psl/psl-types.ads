--  Base types for PSL.
--  Copyright (C) 2019 Tristan Gingold
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
with PSL.Nodes_Priv;

package PSL.Types is
   --  PSL Node.
   subtype PSL_Node is PSL.Nodes_Priv.PSL_Node;
   function "=" (L, R : PSL_Node) return Boolean
     renames PSL.Nodes_Priv."=";
   Null_PSL_Node : constant PSL_Node := PSL.Nodes_Priv.Null_PSL_Node;

   --  PSL NFA
   subtype PSL_NFA is PSL.Nodes_Priv.PSL_NFA;
   function "=" (L, R : PSL_NFA) return Boolean
     renames PSL.Nodes_Priv."=";
end PSL.Types;
