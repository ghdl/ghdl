
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


library ieee;  use ieee.numeric_bit.all;

architecture test of instruction_interpreter is

  subtype word is unsigned(31 downto 0);

  signal address_bus, data_bus_in : word := X"0000_0000";
  signal mem_read, mem_request, mem_ready : bit := '0';

begin

  -- code from book

  instruction_interpreter : process is

    variable mem_address_reg, mem_data_reg,
             prog_counter, instr_reg, accumulator, index_reg : word;
    -- . . .
    -- not in book
    type opcode_type is (load_mem);
    constant opcode : opcode_type := load_mem;
    constant displacement : word := X"0000_0010";
    -- end not in book

    procedure read_memory is
    begin
      address_bus <= mem_address_reg;
      mem_read <= '1';
      mem_request <= '1';
      wait until mem_ready = '1';
      mem_data_reg := data_bus_in;
      mem_request <= '0';
      wait until mem_ready = '0';
    end procedure read_memory;

  begin
    -- . . .  -- initialization
    loop
      -- fetch next instruction
      mem_address_reg := prog_counter;
      read_memory;                           -- call procedure
      instr_reg := mem_data_reg;
      -- . . .
      case opcode is
        -- . . .
        when load_mem =>
          mem_address_reg := index_reg + displacement;
          read_memory;                        -- call procedure
          accumulator := mem_data_reg;
        -- . . .
      end case;
    end loop;
  end process instruction_interpreter;

  -- end code from book


  memory : process is
  begin
    wait until mem_request = '1';
    data_bus_in <= X"1111_1111";
    mem_ready <= '1';
    wait until mem_request = '0';
    mem_ready <= '0';
  end process memory;

end architecture test;
