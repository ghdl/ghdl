
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
                        
entity dac_12_bit is
  port ( signal  bus_in : in std_ulogic_vector (11 downto 0); 
         terminal analog_out :  electrical );
end entity dac_12_bit;

----------------------------------------------------------------

architecture behavioral of dac_12_bit is
  
  constant v_max : real := 3.3;
  signal s_out : real := 0.0;
  quantity v_out across i_out through analog_out to electrical_ref;
        
begin
  
  convert : process ( bus_in ) is
    variable sum : natural;
  begin
    sum  := 0;
    for i in bus_in'range loop
      sum := sum * 2 + boolean'pos( bus_in(i) = '1' or bus_in(i) = 'H' );
    end loop;
    s_out <= v_max * real(sum) / real(2**12 - 1);
  end process convert;
                                     
  v_out == s_out'ramp(1.0E-6);
                                     
end architecture behavioral;
