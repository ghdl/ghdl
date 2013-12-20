
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
-- $Id: tc2081.vhd,v 1.2 2001-10-26 16:29:45 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b04x00p20n01i02081ent IS
END c07s02b04x00p20n01i02081ent;

ARCHITECTURE c07s02b04x00p20n01i02081arch OF c07s02b04x00p20n01i02081ent IS

BEGIN
  TESTING: PROCESS
    VARIABLE target  : string (1 to 10) ;
    VARIABLE slice_1 : string (1 to 10) := "0123456789";
    VARIABLE slice_2 : string (1 to 10) := "abcdefghji";
  BEGIN

    target (2 to 8 ):= slice_1 ( 1 to 3 ) & slice_2 ( 4 to 7 );

    assert NOT(target(2 to 8) = "012defg") 
      report "***PASSED TEST: c07s02b04x00p20n01i02081"
      severity NOTE;
    assert (target(2 to 8) = "012defg") 
      report "***FAILED TEST: c07s02b04x00p20n01i02081 - One dimensional array of STRING type concatenation failed." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b04x00p20n01i02081arch;
