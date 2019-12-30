
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

library ieee_proposed;  use ieee_proposed.mechanical_systems.all;

entity stop_r is
  generic ( k_stop : real := 1.0e6;
            ang_max : real := 1.05;
            ang_min : real := -1.05;
            damp_stop : real := 1.0e2 );
  port ( terminal ang1, ang2 : rotational );
end entity stop_r;

----------------------------------------------------------------

architecture ideal of stop_r is

  quantity qvelocity : velocity;
  quantity ang across trq through ang1 to ang2;

begin

  qvelocity == ang'dot;

  if ang > ang_max use	   -- Hit upper stop, generate opposing torque
    trq == k_stop * (ang - ang_max) + (damp_stop * qvelocity);
  elsif ang > ang_min use  -- Between stops, no opposing torque
    trq   == 0.0;
  else		           -- Hit lower stop, generate opposing torque
    trq   == k_stop * (ang - ang_min) + (damp_stop * qvelocity);
  end use;

  break on ang'above(ang_min), ang'above(ang_max);

end architecture ideal;
