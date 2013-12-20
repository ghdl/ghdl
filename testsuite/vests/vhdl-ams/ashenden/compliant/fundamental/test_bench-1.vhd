
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

entity test_bench is
end entity test_bench;

architecture example of test_bench is
  
  signal clk, reset: bit;
  signal rpm : natural;
  signal forward : bit;
        
begin
  dut : entity work.propulsion(mixed)
    port map ( clk, reset, rpm, forward );
  
  stimulus: process is
  begin
    clk <= '1'; reset <= '0'; rpm <= 0; forward <= '1'; wait for 10 sec;
    clk <= '0'; wait for 10 sec;
    clk <= '1'; rpm <= 50; wait for 20 sec;
    clk <= '0'; wait for 20 sec;
    clk <= '1'; rpm <= 0; wait for 20 sec;
    clk <= '0'; wait for 20 sec;
    clk <= '1'; rpm <= 50; forward <= '0'; wait for 20 sec;
    clk <= '0'; wait for 20 sec;
    -- ...
    wait;
  end process stimulus;
  
end architecture example;
