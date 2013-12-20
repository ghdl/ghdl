
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

package timing_attributes is

  attribute max_wire_delay : delay_length;

end package timing_attributes;


entity sequencer is
end entity sequencer;


-- code from book

library ieee;  use ieee.std_logic_1164.all;
use work.timing_attributes.all;

architecture structural of sequencer is

  signal recovered_clk1, recovered_clk2 : std_logic;
  signal test_enable : std_logic;
  signal test_data : std_logic_vector(0 to 15);

  attribute max_wire_delay of
    recovered_clk1, recovered_clk2 : signal is 100 ps;

  attribute max_wire_delay of others : signal is 200 ps;

  -- . . .

begin
  -- . . .
  -- not in book
  assert false report time'image(recovered_clk1'max_wire_delay) severity note;
  assert false report time'image(recovered_clk2'max_wire_delay) severity note;
  assert false report time'image(test_enable'max_wire_delay) severity note;
  assert false report time'image(test_data'max_wire_delay) severity note;
  -- end not in book
end architecture structural;

-- code from book
