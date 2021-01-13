--  Analysis for translation.
--  Copyright (C) 2009 Tristan Gingold
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

package Trans_Analyzes is
   --  Extract a list of drivers from PROC.
   function Extract_Drivers (Proc : Iir) return Iir_List;

   --  Free the list.
   procedure Free_Drivers_List (List : in out Iir_List);

   --  Dump list of drivers (LIST) for process PROC.
   procedure Dump_Drivers (Proc : Iir; List : Iir_List);

end Trans_Analyzes;
