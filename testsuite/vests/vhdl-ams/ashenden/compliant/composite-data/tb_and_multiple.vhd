
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

entity tb_and_multiple is

end entity tb_and_multiple;


----------------------------------------------------------------


architecture test_behavioral of tb_and_multiple is

  -- code from book:

  signal count_value : bit_vector(7 downto 0);
  signal terminal_count : bit;

  -- end of code from book

begin

  -- code from book:

  tc_gate : entity work.and_multiple(behavioral)
    port map ( i => count_value, y => terminal_count);

  -- end of code from book

  stumulus : process is
  begin
    			wait for 10 ns;
    count_value <= "10000000";	wait for 10 ns;
    count_value <= "11111110";	wait for 10 ns;
    count_value <= "01111111";	wait for 10 ns;
    count_value <= "11111111";	wait for 10 ns;
    count_value <= "00000000";	wait for 10 ns;

    wait;
  end process stumulus;

end architecture test_behavioral;
