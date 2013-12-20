
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
-- $Id: ch_05_fg_05_09.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

-- not in book

entity computer_system is
end entity computer_system;

-- end not in book


architecture abstract of computer_system is

  subtype word is bit_vector(31 downto 0);

  signal address : natural;
  signal read_data, write_data : word;
  signal mem_read, mem_write : bit := '0';
  signal mem_ready : bit := '0';

begin

  cpu : process is
                  variable instr_reg : word;
                variable PC : natural;
                -- . . .    -- other declarations
  begin
    loop
      address <= PC;
      mem_read <= '1';
      wait until mem_ready = '1';
      instr_reg := read_data;
      mem_read <= '0';
      wait until mem_ready = '0';
      PC := PC + 4;
      -- . . .    -- execute the instruction
    end loop;
  end process cpu;

  memory : process is
                     type memory_array is array (0 to 2**14 - 1) of word;
                   variable store : memory_array := (
                     -- . . .
                     -- not in book
                     0 => X"0000_0000",
                     1 => X"0000_0004",
                     2 => X"0000_0008",
                     3 => X"0000_000C",
                     4 => X"0000_0010",
                     5 => X"0000_0014",
                     others => X"0000_0000"
				        -- end not in book
                     );
  begin
    wait until mem_read = '1' or mem_write = '1';
    if mem_read = '1' then
      read_data <= store( address / 4 );
      mem_ready <= '1';
      wait until mem_read = '0';
      mem_ready <= '0';
    else
      -- . . .    -- perform write access
    end if;
  end process memory;

end architecture abstract;
