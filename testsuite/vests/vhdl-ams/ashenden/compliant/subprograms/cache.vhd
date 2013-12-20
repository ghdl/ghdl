
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

-- not in book

entity cache is
end entity cache;

-- end not in book



architecture behavioral of cache is
  -- not in book
  subtype word is bit_vector(0 to 31);
  signal mem_addr : natural;
  signal mem_data_in : word;
  signal mem_read, mem_ack : bit := '0';
  -- end not in book
begin

  behavior : process is

    -- not in book
    constant block_size : positive := 4;
    type cache_block is array (0 to block_size - 1) of word;
    type store_array is array (0 to 15) of cache_block;
    variable data_store : store_array;
    variable entry_index : natural := 1;
    variable miss_base_address : natural := 16;
    -- end not in book

    -- . . .

    procedure read_block( start_address : natural;
                          entry : out cache_block ) is

      variable memory_address_reg : natural;
      variable memory_data_reg : word;

      procedure read_memory_word is
      begin
        mem_addr <= memory_address_reg;
        mem_read <= '1';
        wait until mem_ack = '1';
        memory_data_reg := mem_data_in;
        mem_read <= '0';
        wait until mem_ack = '0';
      end procedure read_memory_word;

    begin  -- read_block
      for offset in 0 to block_size - 1 loop
        memory_address_reg := start_address + offset;
        read_memory_word;
        entry(offset) := memory_data_reg;
      end loop;
    end procedure read_block;

  begin  -- behavior
     -- . . .
    read_block( miss_base_address, data_store(entry_index) );
     -- . . .
    -- not in book
    wait;
    -- end not in book
  end process behavior;


  -- not in book

  memory : process is

    type store_array is array (0 to 31) of word;
    constant store : store_array :=
      ( X"00000000", X"00000001", X"00000002", X"00000003",
        X"00000004", X"00000005", X"00000006", X"00000007",
        X"00000008", X"00000009", X"0000000a", X"0000000b",
        X"0000000c", X"0000000d", X"0000000e", X"0000000f",
        X"00000010", X"00000011", X"00000012", X"00000013",
        X"00000014", X"00000015", X"00000016", X"00000017",
        X"00000018", X"00000019", X"0000001a", X"0000001b",
        X"0000001c", X"0000001d", X"0000001e", X"0000001f" );

  begin
    wait until mem_read = '1';
    mem_data_in <= store(mem_addr);
    mem_ack <= '1';
    wait until mem_read = '0';
    mem_ack <= '0';
  end process memory;

  -- end not in book


end architecture behavioral;
