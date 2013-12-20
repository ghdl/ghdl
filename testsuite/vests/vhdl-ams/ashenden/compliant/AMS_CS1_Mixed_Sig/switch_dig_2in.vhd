
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
                        
entity switch_dig_2in is
  port ( sw_state : in std_ulogic;                     -- Digital control input 
         terminal p_in1, p_in2, p_out : electrical );  -- Analog output
end entity switch_dig_2in;

----------------------------------------------------------------

architecture ideal of switch_dig_2in is
  
  constant r_open : resistance := 1.0e6; 	 -- Open switch resistance
  constant r_closed : resistance := 0.001;	 -- Closed switch resistance
  constant trans_time : real := 0.00001;	 -- Transition time to each position
  
  signal r_sig1 : resistance := r_closed; 	 -- Closed switch resistance variable
  signal r_sig2 : resistance := r_open;	 	 -- Open switch resistance variable
  
  quantity v1 across i1 through p_in1 to p_out;  -- V & I for in1 to out
  quantity v2 across i2 through p_in2 to p_out;  -- V & I for in2 to out
  quantity r1 : resistance; 			 -- Time-varying resistance for in1 to out
  quantity r2 : resistance; 			 -- Time-varying resistance for in2 to out
        
begin
  
  process (sw_state) is  -- Sensitivity to digital control input
  begin
    if sw_state = '0' or sw_state = 'L' then 	 -- Close sig1, open sig2
      r_sig1 <= r_closed;
      r_sig2 <= r_open;
    elsif sw_state = '1' or sw_state = 'H' then  -- Open sig1, close sig2
      r_sig1 <= r_open;
      r_sig2 <= r_closed;
    end if;
  end process;
        
  r1 == r_sig1'ramp(trans_time, trans_time); 	 -- Ensure resistance continuity
  r2 == r_sig2'ramp(trans_time, trans_time); 	 -- Ensure resistance continuity
        
  v1 == r1 * i1;  -- Apply Ohm's law to in1
  v2 == r2 * i2;  -- Apply Ohm's law to in2
        
end architecture ideal;
