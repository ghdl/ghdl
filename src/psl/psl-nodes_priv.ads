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
with Types; use Types;

package PSL.Nodes_Priv is
   --  PSL Node.
   type PSL_Node is new Int32;
   Null_PSL_Node : constant PSL_Node := 0;

   --  PSL NFA
   type PSL_NFA is new Int32;
end PSL.Nodes_Priv;
