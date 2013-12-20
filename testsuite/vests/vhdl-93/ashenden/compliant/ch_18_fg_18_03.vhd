
-- Copyright (C) 1996 Morgan Kaufmann Publishers, Inc

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

-- ---------------------------------------------------------------------
--
-- $Id: ch_18_fg_18_03.vhd,v 1.3 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library bv_utilities;

package CPU_types is

  subtype word is bit_vector(0 to 31);
  subtype byte is bit_vector(0 to 7);

  alias convert_to_natural is
    bv_utilities.bv_arithmetic.bv_to_natural [ bit_vector return natural ];

  constant halt_opcode : byte := "00000000";

  type code_array is array (natural range <>) of word;
  constant code : code_array := ( X"01000000", X"01000000", X"02000000",
				  X"01000000", X"01000000", X"02000000",
				  X"00000000" );

end package CPU_types;

use work.CPU_types.all;

entity CPU is
end entity CPU;

-- code from book

architecture instrumented of CPU is

  type count_file is file of natural;
  file instruction_counts : count_file open write_mode is "instructions";

begin

  interpreter : process is

                          variable IR : word;
                        alias opcode : byte is IR(0 to 7);
                        variable opcode_number : natural;
                        type counter_array is array (0 to 2**opcode'length - 1) of natural;
                        variable counters : counter_array := (others => 0);
                        -- . . .

                        -- not in book
                        variable code_index : natural := 0;
                        -- end not in book

  begin

    -- . . .    -- initialize the instruction set interpreter

    instruction_loop : loop

      -- . . .    -- fetch the next instruction into IR

      -- not in book
      IR := code(code_index);
      code_index := code_index + 1;
      -- end not in book

      -- decode the instruction
      opcode_number := convert_to_natural(opcode);
      counters(opcode_number) := counters(opcode_number) + 1;
      -- . . .

      -- execute the decoded instruction
      case opcode is
        -- . . .
        when halt_opcode => exit instruction_loop;
                            -- . . .
                            -- not in book
        when others => null;
                       -- end not in book
      end case;

    end loop instruction_loop;

    for index in counters'range loop
      write(instruction_counts, counters(index));
    end loop;
    wait;  -- program finished, wait forever

  end process interpreter;

end architecture instrumented;

-- code from book

