
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
                        
entity dac_10_bit is
  port ( signal bus_in : in std_ulogic_vector(9 downto 0); 
         signal clk : in std_ulogic;
         terminal analog_out :  electrical );
end entity dac_10_bit;

----------------------------------------------------------------

architecture behavioral of dac_10_bit is
  
  constant v_max : real := 5.0;
  signal s_out : real := 0.0;
  quantity v_out across i_out through analog_out to electrical_ref;
        
begin
  
  convert : process is
    variable v_sum : real;
    variable delta_v : real;
  begin
    wait until clk'event and (clk = '1' or clk = 'H');
    v_sum  := 0.0;
    delta_v  := v_max;
    for i in bus_in'range loop
      delta_v  := delta_v / 2.0;
      if bus_in(i) = '1' or bus_in(i) = 'H' then
        v_sum := v_sum + delta_v;
      end if;
    end loop;
    s_out <= v_sum;
  end process convert;
        
  v_out == s_out'ramp(100.0E-9);
        
end architecture behavioral;
