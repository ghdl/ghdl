
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
-- $Id: tc2083.vhd,v 1.2 2001-10-26 16:29:45 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b04x00p20n01i02083ent IS
END c07s02b04x00p20n01i02083ent;

ARCHITECTURE c07s02b04x00p20n01i02083arch OF c07s02b04x00p20n01i02083ent IS
  TYPE int_vector is array (INTEGER range <>) of INTEGER;
BEGIN
  TESTING: PROCESS
    VARIABLE target  : int_vector (1 to 7) ;
    VARIABLE slice_1 : int_vector (1 to 4) := (1,2,3,4);
    VARIABLE slice_2 : int_vector (-2 to 4) := (5,6,7,8,9,10,11);
  BEGIN

    target := slice_1 ( 1 to 3 ) & slice_2 ( -1 to 2 );

    assert NOT(target=(1,2,3,6,7,8,9))
      report "***PASSED TEST: c07s02b04x00p20n01i02083"
      severity NOTE;
    assert (target=(1,2,3,6,7,8,9))
      report "***FAILED TEST: c07s02b04x00p20n01i02083 - One dimensional array of INTEGER type concatenation into a larger ARRAY failed." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b04x00p20n01i02083arch;
