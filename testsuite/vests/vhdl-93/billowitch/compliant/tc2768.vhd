
-- Copyright (C) 2001 Bill Billowitch.

-- Some of the work to develop this test suite was done with Air Force
-- support.  The Air Force and Bill Billowitch assume no
-- responsibilities for this software.

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
-- $Id: tc2768.vhd,v 1.1.1.1 2001-08-22 18:20:52 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

-- Dale Martin updated the bit_vectors in this file with bit_vector'()
-- qualification to make it VHDL '93 compliant. (It's still '87 compliant
-- as well.)

ENTITY c13s07b00x00p08n01i02768ent IS
END c13s07b00x00p08n01i02768ent;

ARCHITECTURE c13s07b00x00p08n01i02768arch OF c13s07b00x00p08n01i02768ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT( bit_vector'(O"0") = "000"   and 
		bit_vector'(O"1") = "001"   and
		bit_vector'(O"2") = "010"   and
		bit_vector'(O"3") = "011"   and
		bit_vector'(O"4") = "100"   and
		bit_vector'(O"5") = "101"   and
		bit_vector'(O"6") = "110"   and
		bit_vector'(O"7") = "111"   and
		bit_vector'(O"01")    = "000001"   and
		bit_vector'(O"10")    = "001000"   and
		bit_vector'(O"0_1") = "000001"   and
		bit_vector'(X"0") = "0000"   and  
		bit_vector'(X"1") = "0001"   and
		bit_vector'(X"2") = "0010"   and
		bit_vector'(X"3") = "0011"   and
		bit_vector'(X"4") = "0100"   and
		bit_vector'(X"5") = "0101"   and
		bit_vector'(X"6") = "0110"   and
		bit_vector'(X"7") = "0111"   and
		bit_vector'(X"8") = "1000"   and
		bit_vector'(X"9") = "1001"   and
		bit_vector'(X"A") = "1010"   and
                bit_vector'(X"a") = "1010"   and
		bit_vector'(X"B") = "1011"   and
                bit_vector'(X"b") = "1011"   and
		bit_vector'(X"C") = "1100"   and
                bit_vector'(X"c") = "1100"   and
		bit_vector'(X"D") = "1101"   and
                bit_vector'(X"d") = "1101"   and
		bit_vector'(X"E") = "1110"   and
                bit_vector'(X"e") = "1110"   and
		bit_vector'(X"F") = "1111"   and
                bit_vector'(X"f") = "1111"   and
		bit_vector'(X"01") = "00000001"   and
		bit_vector'(X"10") = "00010000"   and
		bit_vector'(X"0_1") = "00000001"   and
		bit_vector'(X"E_7") = "11100111"   and
		bit_vector'(X"DEAD_BEEF") = B"1101_1110_1010_1101_1011_1110_1110_1111")
      report "***PASSED TEST: c13s07b00x00p08n01i02768"
      severity NOTE;
    assert ( bit_vector'(O"0") = "000"   and 
	     bit_vector'(O"1") = "001"   and
	     bit_vector'(O"2") = "010"   and
	     bit_vector'(O"3") = "011"   and
	     bit_vector'(O"4") = "100"   and
	     bit_vector'(O"5") = "101"   and
	     bit_vector'(O"6") = "110"   and
	     bit_vector'(O"7") = "111"   and
	     bit_vector'(O"01") = "000001"   and
	     bit_vector'(O"10") = "001000"   and
	     bit_vector'(O"0_1") = "000001"   and
	     bit_vector'(X"0") = "0000"   and  
	     bit_vector'(X"1") = "0001"   and
	     bit_vector'(X"2") = "0010"   and
	     bit_vector'(X"3") = "0011"   and
	     bit_vector'(X"4") = "0100"   and
	     bit_vector'(X"5") = "0101"   and
	     bit_vector'(X"6") = "0110"   and
	     bit_vector'(X"7") = "0111"   and
	     bit_vector'(X"8") = "1000"   and
	     bit_vector'(X"9") = "1001"   and
	     bit_vector'(X"A") = "1010"   and
	     bit_vector'(X"a") = "1010"   and
	     bit_vector'(X"B") = "1011"   and
	     bit_vector'(X"b") = "1011"   and
	     bit_vector'(X"C") = "1100"   and
	     bit_vector'(X"c") = "1100"   and
	     bit_vector'(X"D") = "1101"   and
	     bit_vector'(X"d") = "1101"   and
	     bit_vector'(X"E") = "1110"   and
	     bit_vector'(X"e") = "1110"   and
	     bit_vector'(X"F") = "1111"   and
	     bit_vector'(X"f") = "1111"   and
	     bit_vector'(X"01") = "00000001"   and
	     bit_vector'(X"10") = "00010000"   and
	     bit_vector'(X"0_1") = "00000001"   and
	     bit_vector'(X"E_7") = "11100111"   and
	     bit_vector'(X"DEAD_BEEF") = B"1101_1110_1010_1101_1011_1110_1110_1111")
      report "***FAILED TEST: c13s07b00x00p08n01i02768 - Bit string literal and base specifier 'O' and 'X' value transfer test failed." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c13s07b00x00p08n01i02768arch;
