
-- Copyright (C) 2002 Morgan Kaufmann Publishers, Inc

-- This file is part of VESTs (Vhdl tESTs).

-- VESTs is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version. 

-- VESTs is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
-- for more details. 

-- You should have received a copy of the GNU General Public License
-- along with VESTs; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 

library ieee_proposed;
use ieee_proposed.electrical_systems.all;
use ieee_proposed.thermal_systems.all;

entity temperature_dependent_resistor is
  port ( terminal n1, n2 : electrical;
         quantity temp : in temperature );
end entity temperature_dependent_resistor;

architecture linear_approx of temperature_dependent_resistor is
  constant resistance_at_0 : real := 1.0E6;
  constant resistance_drop_per_kelvin : real := 100.0;
  quantity resistance : real;
  quantity V across I through n1 to n2;
begin
  resistance == resistance_at_0 - temp * resistance_drop_per_kelvin;
  V == I * resistance;
end architecture linear_approx;
