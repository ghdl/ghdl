
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

entity inline_08 is

end entity inline_08;


----------------------------------------------------------------


architecture test of inline_08 is
begin

  process_2_c : process is

    -- code from book:

    type opcodes is
      (nop, add, subtract, load, store, jump, jumpsub, branch, halt);

    subtype control_transfer_opcodes is opcodes range jump to branch;

    -- end of code from book

    variable opcode : opcodes;
    variable operand : integer;
    constant memory_operand : integer := 1;
    constant address_operand : integer := 2;

  begin

    for i in opcodes loop
      opcode := i;

      -- code from book:

      case opcode is
        when load | add | subtract =>
          operand := memory_operand;
        when store | jump | jumpsub | branch =>
          operand := address_operand;
        when others =>
          operand := 0;
      end case;

      --

      case opcode is
        when add to load =>
          operand := memory_operand;
        when branch downto store =>
          operand := address_operand;
        when others =>
          operand := 0;
      end case;

      -- end of code from book

      case opcode is
        when add to load =>
          operand := memory_operand;
        -- code from book:
        when control_transfer_opcodes | store =>
          operand := address_operand;
        -- end of code from book
        when others =>
          operand := 0;
      end case;

    end loop;

    wait;
  end process process_2_c;


end architecture test;
