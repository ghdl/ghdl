
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

-- not in book:

entity computer is

end entity computer;

-- end not in book


architecture system_level of computer is

  type opcodes is (add, sub, addu, subu, jmp, breq, brne, ld, st, -- . . .);
  -- not in book:
                                                                  nop);
  -- end not in book
  type reg_number is range 0 to 31;
  constant r0 : reg_number := 0;  constant r1 : reg_number := 1;  -- . . .
  -- not in book:
  constant r2 : reg_number := 2;
  -- end not in book

  type instruction is record
      opcode : opcodes;
      source_reg1, source_reg2, dest_reg : reg_number;
      displacement : integer;
    end record instruction;

  type word is record
      instr : instruction;
      data : bit_vector(31 downto 0);
    end record word;

  signal address : natural;
  signal read_word, write_word : word;
  signal mem_read, mem_write : bit := '0';
  signal mem_ready : bit := '0';

begin

  cpu : process is
    variable instr_reg : instruction;
    variable PC : natural;
    -- . . .    -- other declarations for register file, etc.
  begin
    address <= PC;
    mem_read <= '1';
    wait until mem_ready = '1';
    instr_reg := read_word.instr;
    mem_read <= '0';
    -- not in book:
    wait until mem_ready = '0';
    -- end not in book
    PC := PC + 4;
    case instr_reg.opcode is  -- execute the instruction
      -- . . .
      -- not in book:
      when others => null;
      -- end not in book
    end case;
  end process cpu;

  memory : process is
    subtype address_range is natural range 0 to 2**14 - 1;
    type memory_array is array (address_range) of word;
    variable store : memory_array :=
      (  0  => ( ( ld, r0, r0, r2, 40 ), X"00000000" ),
        1  => ( ( breq, r2, r0, r0, 5 ), X"00000000" ),
        -- . . .
        40  => ( ( nop, r0, r0, r0, 0 ), X"FFFFFFFE"),
        others => ( ( nop, r0, r0, r0, 0 ), X"00000000") );
  begin
    -- . . .
    -- not in book:
    wait until mem_read = '1';
    read_word <= store(address);
    mem_ready <= '1';
    wait until mem_read = '0';
    mem_ready <= '0';
    -- end not in book
  end process memory;

end architecture system_level;
