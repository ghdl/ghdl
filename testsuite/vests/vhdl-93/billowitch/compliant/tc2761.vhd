
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
-- $Id: tc2761.vhd,v 1.1.1.1 2001-08-22 18:20:52 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

ENTITY c13s07b00x00p05n01i02761ent IS
END c13s07b00x00p05n01i02761ent;

-- Dale Martin modified this file to make the bit string literal comparisons
-- VHDL '93 compliant, by qualifying them with bit_string_literal'(

ARCHITECTURE c13s07b00x00p05n01i02761arch OF c13s07b00x00p05n01i02761ent IS
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT( ( bit_vector'(B"01_111_101") = B"0111_1101" )
		and ( bit_vector'(O"17_5") = O"1_75")
		and ( bit_vector'(X"7D") = X"7_D")) 
      report "***PASSED TEST: c13s07b00x00p05n01i02761"
      severity NOTE;
    assert ( ( bit_vector'(B"01_111_101") = B"0111_1101" )
	     and ( bit_vector'(O"17_5")=O"1_75")
	     and ( bit_vector'(X"7D")=X"7_D")) 
      report "***FAILED TEST: c13s07b00x00p05n01i02761 - Underline character should not affect the value of the bit string literal." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c13s07b00x00p05n01i02761arch;
