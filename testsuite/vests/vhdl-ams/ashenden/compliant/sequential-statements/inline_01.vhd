
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

entity inline_01 is

end entity inline_01;


----------------------------------------------------------------


architecture test of inline_01 is

  signal en : bit := '0';
  signal data_in : integer := 0;

begin

  process_1_a : process (en, data_in) is

    variable stored_value : integer := 0;

  begin

    -- code from book:

    if en = '1' then
      stored_value := data_in;
    end if;

    -- end of code from book

  end process process_1_a;

  stimulus : process is
  begin
    en <= '1' after 10 ns, '0' after 20 ns;
    data_in <= 1 after 5 ns, 2 after 15 ns, 3 after 25 ns;
    wait;
  end process stimulus;

end architecture test;
