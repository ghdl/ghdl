
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
-- $Id: ch_08_fg_08_09.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

-- not in book

entity cpu is
end entity cpu;

-- end not in book




architecture behavioral of cpu is
begin

  interpreter : process is

                          use work.cpu_types.all;

                        variable instr_reg : word;
                        variable instr_opcode : opcode;

  begin
    -- . . .    -- initialize
    loop
      -- . . .    -- fetch instruction
      instr_opcode := extract_opcode ( instr_reg );
      case instr_opcode is
        when op_nop => null;
        when op_breq => -- . . .
          -- . . .
          -- not in book
        when others => null;
                       -- end not in book
      end case;
    end loop;
  end process interpreter;

end architecture behavioral;
