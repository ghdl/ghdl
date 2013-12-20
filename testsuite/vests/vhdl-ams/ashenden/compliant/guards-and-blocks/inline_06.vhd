
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

  subtype word is bit_vector(0 to 31);
  type word_array is array (integer range <>) of word;

  function resolve_words ( words : word_array ) return word is
  begin
    if words'length > 0 then
      return words(words'left);
    else
      return X"00000000";
    end if;
  end function resolve_words;

  subtype resolved_word is resolve_words word;

  signal source_bus_1, source_bus_2 : resolved_word bus;
  signal address_bus : resolved_word bus;

  -- code from book:

  disconnect address_bus : resolved_word after 3 ns;

  disconnect others : resolved_word after 2 ns;

  -- end of code from book

  signal s : word;
  signal g : boolean;

begin


  b : block (g) is
  begin
    source_bus_1 <= guarded s after 4 ns;
    source_bus_2 <= guarded s after 4 ns;
    address_bus <= guarded s after 4 ns;
  end block b;

  stimulus : process is
  begin
    s <= X"DDDDDDDD";
    wait for 10 ns;
    g <= true;
    wait for 10 ns;
    s <= X"AAAAAAAA";
    wait for 10 ns;
    g <= false;
    wait for 10 ns;
    s <= X"11111111";

    wait;
  end process stimulus;

end architecture test;
