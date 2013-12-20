
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

entity hold_time_checker is
end entity hold_time_checker;



architecture test of hold_time_checker is

  constant Thold_d_clk : delay_length := 3 ns;

  signal clk, d : bit := '0';

begin

  -- code from book

  hold_time_checker : process ( clk, d ) is
    variable last_clk_edge_time : time := 0 fs;
  begin
    if clk'event and clk = '1' then
      last_clk_edge_time := now;
    end if;
    if d'event then
      assert now - last_clk_edge_time >= Thold_d_clk
        report "hold time violation";
    end if;
  end process hold_time_checker;

  -- end code from book

  clk_gen : clk <= '1' after 10 ns, '0' after 20 ns when clk = '0';

  stimulus : d <= '1' after 15 ns,
                  '0' after 53 ns,
                  '1' after 72 ns;

end architecture test;
