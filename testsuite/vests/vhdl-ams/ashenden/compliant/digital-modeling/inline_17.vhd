
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

entity inline_17 is

end entity inline_17;


----------------------------------------------------------------


architecture test of inline_17 is

  signal s, r, q, q_n : bit := '0';

begin

  q <= '1' when s = '1' else
       '0' when r = '1';

  q_n <= '0' when s = '1' else
         '1' when r = '1';


  -- code from book:

  check : process is
  begin
    assert not (s = '1' and r = '1')
      report "Incorrect use of S_R_flip_flop: s and r both '1'";
    wait on s, r;
  end process check;

  -- end of code from book


  stimulus : process is
  begin
    wait for 10 ns;
    s <= '1';	wait for 10 ns;
    s <= '0';	wait for 10 ns;
    r <= '1';	wait for 10 ns;
    r <= '0';	wait for 10 ns;
    s <= '1';	wait for 10 ns;
    r <= '1';	wait for 10 ns;
    s <= '0';	wait for 10 ns;
    r <= '0';	wait for 10 ns;

    wait;
  end process stimulus;


end architecture test;
