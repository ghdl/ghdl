
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
-- $Id: ch_04_ch_04_07.vhd,v 1.2 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_04_07 is

end entity ch_04_07;


----------------------------------------------------------------


architecture test of ch_04_07 is
begin


  process_04_3_a : process is

                             -- code from book:

                             subtype pixel_row is bit_vector (0 to 15);
                           variable current_row, mask : pixel_row;

                           -- end of code from book

  begin

    current_row := "0000000011111111";
    mask := "0000111111110000";

    -- code from book:

    current_row := current_row and not mask;
    current_row := current_row xor X"FFFF";

    -- end of code from book

    -- code from book (conditions only):

    assert B"10001010" sll 3  =  B"01010000";
    assert B"10001010" sll -2  =  B"00100010";

    assert B"10010111" srl 2  = B"00100101";
    assert B"10010111" srl -6  =  B"11000000";

    assert B"01001011" sra 3  =  B"00001001";
    assert B"10010111" sra 3  =  B"11110010";
    assert B"00001100" sla 2  =  B"00110000";
    assert B"00010001" sla 2  =  B"01000111";

    assert B"00010001" sra -2  =  B"01000111";
    assert B"00110000" sla -2  =  B"00001100";

    assert B"10010011" rol 1  =  B"00100111";
    assert B"10010011" ror 1  =  B"11001001";

    assert "abc" & 'd'  =  "abcd";
    assert 'w' & "xyz"  =  "wxyz";
    assert 'a' & 'b'  =  "ab";

    -- end of code from book

    wait;
  end process process_04_3_a;


end architecture test;
