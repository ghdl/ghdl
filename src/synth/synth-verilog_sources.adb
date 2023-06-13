--  Source utilities for verilog
--  Copyright (C) 2023 Tristan Gingold
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

with Netlists.Locations; use Netlists.Locations;

package body Synth.Verilog_Sources is
   procedure Set_Location2 (N : Net; Src : Node) is
   begin
      Set_Location (N, Get_Location (Src));
   end Set_Location2;

   procedure Set_Location (N : Net; Src : Node) is
   begin
      if Flag_Locations then
         Set_Location2 (N, Src);
      end if;
   end Set_Location;
end Synth.Verilog_Sources;
