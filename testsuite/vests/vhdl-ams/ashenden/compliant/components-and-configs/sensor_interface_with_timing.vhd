
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

configuration sensor_interface_with_timing of sensor_interface is

  for structural

    for sensor_adc : adc
      use entity work.successive_approx_adc(struct)
        generic map ( t_setup => 200 ps, t_hold => 150 ps, t_pd => 150 ps,
                      width => width );
    end for;

    -- ...

  end for;

end configuration sensor_interface_with_timing;
