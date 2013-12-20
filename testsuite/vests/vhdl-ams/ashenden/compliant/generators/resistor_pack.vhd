
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
use ieee_proposed.electrical_systems.all, ieee_proposed.thermal_systems.all;

entity resistor_pack is
  generic ( resistances_at_298K : real_vector;
            temperature_coeff : real := 0.0 );
  port ( terminal p1, p2 : electrical_vector(1 to resistances_at_298K'length);
         quantity package_temp : in temperature );
end entity resistor_pack;

----------------------------------------------------------------

architecture coupled of resistor_pack is

  quantity v across i through p1 to p2;
  quantity effective_resistance : real_vector(1 to resistances_at_298K'length);

begin

  resistor_array : for index in 1 to resistances_at_298K'length generate

    effective_resistance(index)
      == resistances_at_298K(index)
          + ( package_temp - 298.0 ) * temperature_coeff;

    v(index ) == i(index) * effective_resistance(index);

  end generate resistor_array;

end architecture coupled;
