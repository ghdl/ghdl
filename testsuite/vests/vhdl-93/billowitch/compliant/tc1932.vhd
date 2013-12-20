
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
-- $Id: tc1932.vhd,v 1.2 2001-10-26 16:29:44 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b01x00p01n02i01932ent IS
END c07s02b01x00p01n02i01932ent;

ARCHITECTURE c07s02b01x00p01n02i01932arch OF c07s02b01x00p01n02i01932ent IS
  SUBTYPE bit_8 is bit_vector(0 to 7);
  SUBTYPE bit_4 is bit_vector(0 to 3);
BEGIN
  TESTING: PROCESS
    CONSTANT slice_8a : bit_8 := B"1010_0011";
    VARIABLE slice_8b : bit_8 := B"1110_1001";
    VARIABLE target_1 : bit_4;
    VARIABLE target_2 : bit_4;
    VARIABLE target_3 : bit_4;
    VARIABLE target_4 : bit_4;
    VARIABLE target_5 : bit_4;
    VARIABLE target_6 : bit_4;
  BEGIN
    target_1 := slice_8a (3 to 6) AND slice_8b (4 to 7);

    target_2 := slice_8a (3 to 6) OR slice_8b (4 to 7);

    target_3 := slice_8a (3 to 6) NOR slice_8b (4 to 7);

    target_4 := slice_8a (3 to 6) NAND slice_8b (4 to 7);

    target_5 := slice_8a (3 to 6) XOR slice_8b (4 to 7);
    
    target_6 := NOT slice_8b (0 to 3);

    assert NOT(
      target_1 = B"0001"   and
      target_2 = B"1001"   and
      target_3 = B"0110"   and
      target_4 = B"1110"   and
      target_5 = B"1000"   and
      target_6 = B"0001"   )
      report "***PASSED TEST: c07s02b01x00p01n02i01932" 
      severity NOTE;
    assert (
      target_1 = B"0001"   and
      target_2 = B"1001"   and
      target_3 = B"0110"   and
      target_4 = B"1110"   and
      target_5 = B"1000"   and
      target_6 = B"0001"   )
      report "***FAILED TEST: c07s02b01x00p01n02i01932 - Logical operators are valid for bit slice operations."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b01x00p01n02i01932arch;
