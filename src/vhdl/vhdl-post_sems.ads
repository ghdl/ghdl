--  Global checks after analyze pass.
--  Copyright (C) 2002 - 2016 Tristan Gingold
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
with Vhdl.Nodes; use Vhdl.Nodes;

package Vhdl.Post_Sems is
   --  Do post analyze checks, such as VITAL checks.
   --  This procedure is also used to extract declarations from ieee
   --  packages.
   procedure Post_Sem_Checks (Unit : Iir_Design_Unit);
end Vhdl.Post_Sems;
