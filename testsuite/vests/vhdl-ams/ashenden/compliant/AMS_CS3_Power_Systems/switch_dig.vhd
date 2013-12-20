
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
library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity switch_dig is
  generic ( r_open : resistance := 1.0e6;
            r_closed : resistance := 1.0e-3;
            trans_time : real := 1.0e-9 );
  port ( sw_state : in std_logic;
         terminal p1, p2 : electrical );
end entity switch_dig;

----------------------------------------------------------------

architecture linear of switch_dig is
  
  signal r_sig : resistance := r_open;
  quantity v across i through p1 to p2;
  quantity r : resistance;
  
begin
  
  -- detect switch state and assign resistance value to r_sig
  DetectState: process (sw_state)
  begin
    if (sw_state'event and sw_state = '0') then
      r_sig <= r_open;
    elsif (sw_state'event and sw_state = '1') then
      r_sig <= r_closed;
    end if;
  end process DetectState;

  r == r_sig'ramp(trans_time, trans_time);
  v == r * i;

end architecture linear;
