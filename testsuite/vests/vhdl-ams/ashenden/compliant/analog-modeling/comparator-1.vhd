
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

library ieee;  use ieee.std_logic_1164.all;
library IEEE_proposed;  use IEEE_proposed.electrical_systems.all;
                        
entity comparator is
  port ( terminal plus_in, minus_in : electrical;
         signal output : out std_ulogic );
end entity comparator;

----------------------------------------------------------------

architecture hysteresis of comparator is
  
  constant threshold_margin : real := 0.2;
  quantity v_in across plus_in to minus_in;
        
begin
  
  comp_behavior : process is
    variable threshold : real := threshold_margin;
  begin
    if v_in > threshold then
      output <= '1' after 10 ns;
      threshold := -threshold_margin;
    else
      output <= '0' after 10 ns;
      threshold := threshold_margin;
    end if;
    wait on v_in'above(threshold);
  end process comp_behavior;
                                
end architecture hysteresis;
