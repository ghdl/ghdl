
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

entity lead_lag_diff is
  port ( signal clk : in std_logic;  -- clock
         quantity input : in real;
         quantity output : out real );
end entity lead_lag_diff;

----------------------------------------------------------------

architecture bhv of lead_lag_diff is
  
  constant k : real := 400.0;  -- normalize gain
  signal z_out : real := 0.0;
        
begin
  
  proc : process (clk)
    variable zi_dly1 : real := 0.0;  -- input delayed 1 clk cycle
    variable zo_dly1 : real := 0.0;  -- output delayed 1 clk cycle
    variable z_new : real := 0.0;    -- new output value this clk cycle
  begin
    zo_dly1 := z_out;  -- store previous output value
    z_new := 0.6163507 * input - 0.6144184 * zi_dly1 + 0.2307692 * zo_dly1;
    zi_dly1 := input;  -- store previous input value
    z_out <= z_new;
  end process;
        
  output == k * z_out'ramp(100.0e-9);  -- ensure continuous transitions on output
        
end bhv;
