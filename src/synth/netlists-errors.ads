--  Error handling for synthesis.
--  Copyright (C) 2017 Tristan Gingold
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

with Errorout; use Errorout;
with Netlists.Locations;

package Netlists.Errors is
   --  For instances:
   --  %n : name
   --  %i : instance number
   function "+" (N : Instance) return Earg_Type;

   function "+" (N : Net) return Earg_Type;
   function "+" (N : Sname) return Earg_Type;
   function "+" (N : Instance) return Location_Type
     renames Netlists.Locations.Get_Location;
   procedure Initialize;
end Netlists.Errors;
