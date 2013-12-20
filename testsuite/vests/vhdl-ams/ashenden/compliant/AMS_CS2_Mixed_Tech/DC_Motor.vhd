
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
use ieee_proposed.mechanical_systems.all;
use ieee_proposed.electrical_systems.all;

entity DC_Motor is
  generic ( r_wind : resistance;  -- motor winding resistance [ohm]
            kt : real;		  -- torque coefficient [N*m/amp]
            l : inductance;	  -- winding inductance [henrys]
            d : real;		  -- damping coefficient [N*m/(rad/sec)]
            j : mmoment_i );	  -- moment of inertia [kg*meter**2]
  port ( terminal p1, p2 : electrical;
         terminal shaft_rotv : rotational_v);
end entity DC_Motor;

----------------------------------------------------------------

architecture basic of DC_Motor is

  quantity v across i through p1 to p2;
  quantity w across torq through shaft_rotv to rotational_v_ref;

begin

  torq == -1.0 * kt * i + d * w + j * w'dot;
  v  == kt * w + i * r_wind + l * i'dot;

end architecture basic;
