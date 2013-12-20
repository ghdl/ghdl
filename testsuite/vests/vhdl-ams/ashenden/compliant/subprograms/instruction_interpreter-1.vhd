
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

entity instruction_interpreter is
end entity instruction_interpreter;


architecture test of instruction_interpreter is

  subtype word is bit_vector(31 downto 0);

  signal address_bus, data_bus_in : word := X"0000_0000";
  signal mem_read, mem_request, mem_ready, reset : bit := '0';

begin

  -- code from book

  instruction_interpreter : process is

    -- . . .

    -- not in book
    variable mem_address_reg, mem_data_reg : word;
    -- end not in book

    procedure read_memory is
    begin
      address_bus <= mem_address_reg;
      mem_read <= '1';
      mem_request <= '1';
      wait until mem_ready = '1' or reset = '1';
      if reset = '1' then
        return;
      end if;
      mem_data_reg := data_bus_in;
      mem_request <= '0';
      wait until mem_ready = '0';
    end procedure read_memory;

  begin
    -- . . .    -- initialization
    -- not in book
    if reset = '1' then
      wait until reset = '0';
    end if;
    -- end not in book
    loop
      -- . . .
      read_memory;
      exit when reset = '1';
        -- . . .
    end loop;
  end process instruction_interpreter;

  -- end code from book


  memory : process is
  begin
    wait until mem_request = '1';
    data_bus_in <= X"1111_1111";
    mem_ready <= '1' after 10 ns;
    wait until mem_request = '0';
    mem_ready <= '0' after 10 ns;
  end process memory;

  reset <= '1' after 85 ns;

end architecture test;
