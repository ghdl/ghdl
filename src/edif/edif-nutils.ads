--  EDIF utils.
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

with Edif.Nodes; use Edif.Nodes;

package Edif.Nutils is
   type Constr_Type is limited private;

   procedure Init_Constr (Constr : out Constr_Type);
   procedure Append_Node (Constr : in out Constr_Type; N : Node);
   function Get_Constr_Chain (Constr : Constr_Type) return Node;

private
   type Constr_Type is record
      First : Node;
      Last : Node;
   end record;
end Edif.Nutils;
