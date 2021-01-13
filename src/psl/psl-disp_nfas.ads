--  PSL - Disp nodes
--  Copyright (C) 2002-2016 Tristan Gingold
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

with PSL.NFAs; use PSL.NFAs;
with PSL.Nodes; use PSL.Nodes;

package PSL.Disp_NFAs is
   procedure Disp_Head (Name : String);
   procedure Disp_Tail;
   procedure Disp_Body (N : NFA);

   procedure Disp_State (S : NFA_State);

   procedure Disp_NFA (N : NFA; Name : String := "nfa");
end PSL.Disp_NFAs;
