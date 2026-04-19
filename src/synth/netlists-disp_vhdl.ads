--  Disp a netlist in vhdl.
--  Copyright (C) 2019 Tristan Gingold
--
--  This file is part of GHDL.
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

package Netlists.Disp_Vhdl is
   --  Simulation flavour: avoid potential overflow/bound errors
   Flag_Flavour_Sim : Boolean := True;

   --  Display all the submodules of TOP (but not TOP).
   procedure Disp_Vhdl_Submodules (Top : Module);

   --  Display the top user module and its submodules.
   --  (TOP is the Id_Top module).
   procedure Disp_Vhdl_Hierarchy (Top : Module);

   procedure Disp_Architecture_Declarations (M : Module);
   procedure Disp_Architecture_Statements (M : Module);

   procedure Put_Type (W : Width);
end Netlists.Disp_Vhdl;
