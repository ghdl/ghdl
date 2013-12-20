
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

entity computer_system_abstract is
end entity computer_system_abstract;


-- code from book

architecture abstract of computer_system_abstract is

  -- not in book

  subtype word is bit_vector(31 downto 0);
  type word_vector is array (natural range <>) of word;

  function resolve_word ( drivers : word_vector ) return word is
  begin
    if drivers'length > 0 then
      return drivers(drivers'left);
    else
      return X"00000000";
    end if;
  end function resolve_word;

  -- end not in book

  -- . . .

  signal address_bus : resolve_word word bus;
  signal hold_req : bit;
  -- . . .

  -- not in book
  signal clk : bit := '0';
  -- end not in book

begin

  cpu : block is

    signal guard : boolean := false;
    signal cpu_internal_address : word;
    -- . . .

  begin

    cpu_address_driver:
      address_bus <= guarded cpu_internal_address;

    -- . . .    -- other bus drivers

    controller : process is
      -- . . .
    begin
      -- . . .
      -- . . .    -- determine when to disable cpu bus drivers
      guard <= false;
      wait on clk until hold_req = '0' and clk = '1';
      guard <= true;  -- reenable cpu bus drivers
      -- . . .
      -- not in book
      wait until clk = '1';
      -- end not in book
    end process controller;

    -- . . .    -- cpu data-path processes

    -- not in book
    cpu_internal_address <= X"11111111";
    -- end not in book

  end block cpu;

  -- . . .    -- blocks for DMA and other modules

  -- not in book
  clk <= '1' after 10 ns, '0' after 20 ns when clk = '0';
  -- end not in book

end architecture abstract;

-- end code from book
