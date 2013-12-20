
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
-- $Id: ch_03_ch_03_10.vhd,v 1.3 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

entity ch_03_10 is
end entity ch_03_10;

architecture test of ch_03_10 is

  type opcode_type is (nop, add, subtract);

  signal opcode : opcode_type := nop;

begin

  process_3_3_a : process (opcode) is

                                     variable Acc : integer := 0;
                                   constant operand : integer := 1;

  begin

    -- code from book:

    case opcode is
      when add =>
        Acc := Acc + operand;
      when subtract =>
        Acc := Acc - operand;
      when nop =>
        null;
    end case;

    -- end of code from book

  end process process_3_3_a;

  stimulus : process is
  begin
    opcode <= add after 10 ns, subtract after 20 ns, nop after 30 ns;
    wait;
  end process stimulus;

end architecture test;
