
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

entity inline_06 is

end entity inline_06;


----------------------------------------------------------------


architecture test of inline_06 is

  signal y : bit := '0';
  signal or_a_b : bit := '0';
  signal clk : bit := '0';

begin


  process_3_a : process is
  begin

    -- code from book:

    y <= not or_a_b after 5 ns;

    -- end of code from book

    wait on or_a_b;
  end process process_3_a;


  stimulus_3_a : process is
  begin
    or_a_b <= '1' after 20 ns,
              '0' after 40 ns;
    wait;
  end process stimulus_3_a;


  process_3_b : process is
    constant T_pw : delay_length := 10 ns;
  begin

    -- code from book:

    clk <= '1' after T_pw, '0' after 2*T_pw;

    -- end of code from book

    wait for 2*T_pw;
  end process process_3_b;


end architecture test;
