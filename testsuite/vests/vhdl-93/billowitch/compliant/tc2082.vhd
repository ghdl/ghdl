
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
-- $Id: tc2082.vhd,v 1.2 2001-10-26 16:29:45 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b04x00p20n01i02082ent IS
END c07s02b04x00p20n01i02082ent;

ARCHITECTURE c07s02b04x00p20n01i02082arch OF c07s02b04x00p20n01i02082ent IS
  TYPE real_vector is array (INTEGER range <>) of REAL;
BEGIN
  TESTING: PROCESS
    VARIABLE target  : real_vector (1 to 7) ;
    VARIABLE slice_1 : real_vector (1 to 4) := (1.0,2.0,3.0,4.0);
    VARIABLE slice_2 : real_vector (-2 to 4) :=
      (5.0,6.0,7.0,8.0,9.0,10.0,11.0);
  BEGIN

    target := slice_1 ( 1 to 3 ) & slice_2 ( -1 to 2 );

    assert NOT(target=(1.0,2.0,3.0,6.0,7.0,8.0,9.0))
      report "***PASSED TEST: c07s02b04x00p20n01i02082"
      severity NOTE;
    assert (target=(1.0,2.0,3.0,6.0,7.0,8.0,9.0))
      report "***FAILED TEST: c07s02b04x00p20n01i02082 - One dimensional array of REAL type concatenation into a larger ARRAY failed." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b04x00p20n01i02082arch;
