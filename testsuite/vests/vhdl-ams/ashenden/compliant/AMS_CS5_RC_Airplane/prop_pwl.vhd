
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

entity prop_pwl is
  generic ( ydata : real_vector;    -- torque data points
            xdata : real_vector );  -- velocity data points
  port ( terminal shaft1 : rotational_v );
end entity prop_pwl;

----------------------------------------------------------------

architecture ideal of prop_pwl is

  use work.pwl_functions.all;

  quantity w across torq through shaft1 to rotational_v_ref;

begin

  torq == pwl_dim1_extrap(w, xdata, ydata);

end architecture ideal;
