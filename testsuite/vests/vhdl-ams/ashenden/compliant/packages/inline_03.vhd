
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

entity inline_03 is

end entity inline_03;


----------------------------------------------------------------


library ieee;

architecture test of inline_03 is
begin


  process_3_a : process is

    -- code from book:

    use work.cpu_types;

    variable data_word : cpu_types.word;
    variable next_address : cpu_types.address;

    -- end of code from book

  begin
   wait;
  end process process_3_a;


  ----------------


  process_3_b : process is

    -- code from book:

    use work.cpu_types.word, work.cpu_types.address;

    variable data_word : word;
    variable next_address : address;

    -- end of code from book

  begin
    wait;
  end process process_3_b;


end architecture test;
