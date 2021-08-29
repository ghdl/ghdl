--  Synthesis checks
--  Copyright (C) 2021-2023 Ondrej Ille, Hipólito Guzmán-Miranda, T. Gingold.
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

package Vhdl.Sensitivity_Checks is

   -- Check for issues which cause simulation synthesis mismatches
   --  - check incomplete and over-specified sensitivity lists
   procedure Check_Sensitivity_List (Proc : Iir);

end Vhdl.Sensitivity_Checks;
