
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
-- $Id: ch_16_fg_16_01.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

-- not in book

entity computer_system is
end entity computer_system;

-- end not in book


architecture top_level of computer_system is

  function resolve_bits ( bits : bit_vector ) return bit is
    variable result : bit := '0';
  begin
    for index in bits'range loop
      result := result or bits(index);
      exit when result = '1';
    end loop;
    return result;
  end function resolve_bits;

  signal write_en : resolve_bits bit bus;
  -- . . .

  -- not in book
  constant Tpd : delay_length := 2 ns;
  signal clock, hold_req : bit := '0';
  -- end not in book

begin

  CPU : process is
                  -- . . .
  begin
    write_en <= '0' after Tpd;
    -- . . .
    loop
      wait until clock = '1';
      if hold_req = '1' then
        write_en <= null after Tpd;
        wait on clock until clock = '1' and hold_req = '0';
        write_en <= '0' after Tpd;
      end if;
      -- . . .
    end loop;
  end process CPU;

  -- . . .

  -- not in book

  clock_gen : clock <= '1' after 5 ns, '0' after 10 ns when clock = '0';

  stimulus : hold_req <= '1' after 40 ns, '0' after 80 ns;

  process is
  begin
    write_en <= null, '1' after 50 ns, '0' after 60 ns, null after 70 ns;
    wait;
  end process;

  -- end not in book

end architecture top_level;
