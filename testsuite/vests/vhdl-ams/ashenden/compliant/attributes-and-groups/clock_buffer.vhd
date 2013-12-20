
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

package constraints is

  -- code from book (in text)

  group port_pair is ( signal, signal );

  attribute max_prop_delay : time;

  -- end code from book

end package constraints;



-- code from book

library ieee;  use ieee.std_logic_1164.all;
use work.constraints.port_pair, work.constraints.max_prop_delay;

entity clock_buffer is
  port ( clock_in : in std_logic;
         clock_out1, clock_out2, clock_out3 : out std_logic );

  group clock_to_out1 : port_pair ( clock_in, clock_out1 );
  group clock_to_out2 : port_pair ( clock_in, clock_out2 );
  group clock_to_out3 : port_pair ( clock_in, clock_out3 );

  attribute max_prop_delay of clock_to_out1 : group is 2 ns;
  attribute max_prop_delay of clock_to_out2 : group is 2 ns;
  attribute max_prop_delay of clock_to_out3 : group is 2 ns;

end entity clock_buffer;

-- end code from book
