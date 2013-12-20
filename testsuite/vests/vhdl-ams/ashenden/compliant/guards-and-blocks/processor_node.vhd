
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

entity processor_node is
end entity processor_node;


-- code from book

architecture dataflow of processor_node is

  -- not in book

  subtype word is bit_vector(31 downto 0);
  type word_vector is array (natural range <>) of word;

  function resolve_unique ( drivers : word_vector ) return word is
  begin
    if drivers'length > 0 then
      return drivers(drivers'left);
    else
      return X"00000000";
    end if;
  end function resolve_unique;

  -- end not in book

  signal address_bus : resolve_unique word bus;
  -- . . .

  -- not in book
  signal cache_miss, dirty, replace_section,
         snoop_hit, flag_update : bit := '0';
  constant tag_section0 : bit_vector(11 downto 0) := X"000";
  constant tag_section1 : bit_vector(11 downto 0) := X"001";
  constant set_index : bit_vector(15 downto 0) := X"6666";
  constant snoop_address : word := X"88888888";
  -- end not in book

begin

  cache_to_address_buffer : block ( cache_miss = '1' and dirty = '1' ) is
  begin
    address_bus <= guarded
      tag_section0 & set_index & B"0000" when replace_section = '0' else
      tag_section1 & set_index & B"0000";
  end block cache_to_address_buffer;

  snoop_to_address_buffer : block ( snoop_hit = '1' and flag_update = '1' ) is
  begin
    address_bus <= guarded snoop_address(31 downto 4) & B"0000";
  end block snoop_to_address_buffer;

  -- . . .

  -- not in book

  stimulus : process is
  begin
    wait for 10 ns;
    dirty <= '0';  cache_miss <= '1', '0' after 5 ns;  wait for 10 ns;
    dirty <= '1';  cache_miss <= '1', '0' after 5 ns;  wait for 10 ns;
    replace_section <= '1';
    cache_miss <= '1', '0' after 5 ns;  wait for 10 ns;
    flag_update <= '0'; snoop_hit <= '1', '0' after 5 ns;  wait for 10 ns;
    flag_update <= '1'; snoop_hit <= '1', '0' after 5 ns;  wait for 10 ns;

    wait;
  end process stimulus;

  -- end not in book

end architecture dataflow;

-- end code from book
