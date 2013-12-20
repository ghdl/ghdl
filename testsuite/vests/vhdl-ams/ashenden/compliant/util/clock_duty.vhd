
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

-- This digital clock allows user to specify the duty cycle using
-- the parameters "on_time" and "off_time"

library ieee;  use ieee.std_logic_1164.all;

entity clock_duty is

  generic ( on_time : time := 20 us;
            off_time : time := 19.98 ms ); 
  
  port ( clock_out    : out std_logic := 'Z' );
  
end entity clock_duty;


architecture ideal of clock_duty is

begin

  process
  begin
    wait for 1 us;
    clock_out <= '1';
    wait for on_time;
    clock_out <= '0';
    wait for off_time;
  end process;

end architecture ideal;
