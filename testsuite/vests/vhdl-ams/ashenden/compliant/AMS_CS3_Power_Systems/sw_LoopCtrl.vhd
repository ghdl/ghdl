
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

library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity sw_LoopCtrl is
  generic ( r_open : resistance := 1.0e6;
            r_closed : resistance := 1.0e-3;
            sw_state : integer range 1 to 2 := 1 );
  port ( terminal c, p1, p2 : electrical );
end entity sw_LoopCtrl;

----------------------------------------------------------------

architecture ideal of sw_LoopCtrl is

  quantity v1 across i1 through c to p1;
  quantity v2 across i2 through c to p2;
  quantity r1, r2 : resistance;

begin

  sw1 : if sw_state = 1 generate
    r1 == r_closed;
    r2 == r_open;
  end generate sw1;
  
  sw2 : if sw_state = 2 generate
    r1 == r_open;
    r2 == r_closed;
  end generate sw2;

  v1 == r1 * i1;
  v2 == r2 * i2;

end architecture ideal;
