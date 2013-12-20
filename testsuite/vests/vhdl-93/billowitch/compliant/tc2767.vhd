
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
-- $Id: tc2767.vhd,v 1.1.1.1 2001-08-22 18:20:52 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

-- Dale Martin modified this file to make the bit string literal comparisons
-- valid for VHDL 93, by qualifying them with bit_vector'()

ENTITY c13s07b00x00p08n01i02767ent IS
END c13s07b00x00p08n01i02767ent;

ARCHITECTURE c13s07b00x00p08n01i02767arch OF c13s07b00x00p08n01i02767ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT( ( bit_vector'(B"1111_0101_1101_1010") = bit_vector'(X"F5DA") )   and
		(bit_vector'(B"101_110_001_111") = bit_vector'(O"5617")))
      report "***PASSED TEST: c13s07b00x00p08n01i02767"
      severity NOTE;
    assert ( ( bit_vector'(B"1111_0101_1101_1010") = bit_vector'(X"F5DA"))   and
	     (bit_vector'(B"101_110_001_111") = bit_vector'(O"5617")))
      report "***FAILED TEST: c13s07b00x00p08n01i02767 - Bit value test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c13s07b00x00p08n01i02767arch;
