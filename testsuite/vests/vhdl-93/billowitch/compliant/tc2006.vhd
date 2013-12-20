
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
-- $Id: tc2006.vhd,v 1.2 2001-10-26 16:29:44 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b02x00p08n02i02006ent IS
END c07s02b02x00p08n02i02006ent;

ARCHITECTURE c07s02b02x00p08n02i02006arch OF c07s02b02x00p08n02i02006ent IS

  TYPE    real_vector is array (integer range <>) of REAL;
  SUBTYPE real_8 is real_vector(0 to 7);
  SUBTYPE real_4 is real_vector(0 to 3);

BEGIN
  TESTING: PROCESS

    CONSTANT slice_8a : real_8 := (1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0);
    VARIABLE slice_8b : real_8 := (1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0);
    VARIABLE target_1 : boolean;
    VARIABLE target_2 : boolean;

  BEGIN
    target_1 := slice_8a (3 to 3)  = slice_8b (3 to 3);
    target_2 := slice_8a (3 to 3) /= slice_8b (4 to 4);

    assert NOT(target_1 and target_2) 
      report "***PASSED TEST: c07s02b02x00p08n02i02006"
      severity NOTE;
    assert (target_1 and target_2) 
      report "***FAILED TEST: c07s02b02x00p08n02i02006 - Two single element REAL slices are operable over the set of relational operations." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b02x00p08n02i02006arch;
