--  PSL - HDL interface.
--  Copyright (C) 2002-2016 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

with Types; use Types;
with PSL.Nodes; use PSL.Nodes;

package PSL.Hash is
   --  Initialize the package.
   procedure Init;

   --  Get the PSL node for node HDL.
   --  Only one PSL node is created for an HDL node.
   function Get_PSL_Node (Hdl : Int32) return Node;
end PSL.Hash;
