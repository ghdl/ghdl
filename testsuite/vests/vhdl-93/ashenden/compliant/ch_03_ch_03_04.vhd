
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
-- $Id: ch_03_ch_03_04.vhd,v 1.3 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

entity ch_03_04 is
end entity ch_03_04;

architecture test of ch_03_04 is

  type opcode_type is (opcode_1, opcode_2, halt_opcode);
  signal opcode : opcode_type := opcode_1;

  signal halt_indicator : boolean := false;

begin

  process_3_1_d : process (opcode) is

                                     variable PC : integer := 0;
                                   constant effective_address : integer := 1;
                                   variable executing : boolean := true;

  begin

    -- code from book:

    if opcode = halt_opcode then
      PC := effective_address;
      executing := false;
      halt_indicator <= true;
    end if;

    -- end of code from book

  end process process_3_1_d;

  stimulus : process is
  begin
    opcode <= opcode_2 after 100 ns, halt_opcode after 200 ns;
    wait;
  end process stimulus;

end architecture test;
