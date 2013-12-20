
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

entity inline_02 is
end entity inline_02;


architecture test of inline_02 is

  signal unbuffered_clock : std_logic;
  signal buffered_clock_array : std_logic_vector(0 to 7);

begin

  -- code from book (in text)

  clock_buffer_tree : entity work.fanout_tree(recursive)
    generic map ( height => 3 )
    port map ( input => unbuffered_clock,
               output => buffered_clock_array );

  -- end code from book

  clock_gen : process is
  begin
    unbuffered_clock <= '1' after 5 ns, '0' after 10 ns;
    wait for 10 ns;
  end process clock_gen;

end architecture test;
