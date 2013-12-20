
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

entity inline_15 is

end entity inline_15;


----------------------------------------------------------------


architecture test of inline_15 is
begin


  process_3_c : process is

    -- code from book:

    subtype name is string(1 to 20);
    type display_string is array (integer range 0 to 19) of character;

    variable item_name : name;
    variable display : display_string;

    --

    subtype big_endian_upper_halfword is bit_vector(0 to 15);
    subtype little_endian_upper_halfword is bit_vector(31 downto 16);

    variable big : big_endian_upper_halfword;
    variable little : little_endian_upper_halfword;

    -- end of code from book

  begin

    -- error: Incompatible types for assignment
    -- display := item_name;  -- ilegal

    item_name := (others => 'A');

    little := x"AAAA";

    -- code from book:

    display := display_string(item_name);

    --

    big := little;
    little := big;

    -- end of code from book

    wait;
  end process process_3_c;


  ----------------


end architecture test;
