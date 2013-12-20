
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
                        
entity ball is
end entity ball;

----------------------------------------------------------------

architecture bouncer of ball is
  quantity v : velocity := 0.0;
  quantity s : displacement := 10.0;
  constant g : real := 9.81;
  constant air_res : real := 0.1;
begin
  
  if v'above(0.0) use
    v'dot == -g - v**2*air_res;
  else
    v'dot == -g + v**2*air_res;
  end use;
        
  reversal_tester : process is
  begin
    wait on s'above(0.0);
    break v => -v when s < 0.0;
  end process reversal_tester;
        
  s'dot == v;
        
end architecture bouncer;
