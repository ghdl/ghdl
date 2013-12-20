
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
-- $Id: ch_08_fg_08_06.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

package cpu_types is

  constant word_size : positive := 16;
  constant address_size : positive := 24;

  subtype word is bit_vector(word_size - 1 downto 0);
  subtype address is bit_vector(address_size - 1 downto 0);

  type status_value is ( halted, idle, fetch, mem_read, mem_write,
                         io_read, io_write, int_ack );

  subtype opcode is bit_vector(5 downto 0);

  function extract_opcode ( instr_word : word ) return opcode;

  constant op_nop : opcode := "000000";
  constant op_breq : opcode := "000001";
  constant op_brne : opcode := "000010";
  constant op_add : opcode := "000011";
  -- . . .

end package cpu_types;



-- not in book

package body cpu_types is

  function extract_opcode ( instr_word : word ) return opcode is
  begin
    return work.cpu_types.op_nop;
  end function extract_opcode;

end package body cpu_types;

-- end not in book
