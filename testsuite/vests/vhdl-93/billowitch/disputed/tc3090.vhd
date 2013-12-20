
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
-- $Id: tc3090.vhd,v 1.2 2001-10-26 16:30:04 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c05s01b00x00p02n01i03090ent IS
END c05s01b00x00p02n01i03090ent;

ARCHITECTURE c05s01b00x00p02n01i03090arch OF c05s01b00x00p02n01i03090ent IS
  type       a       is range 1 to 10;
  attribute    arbitrary   : integer;
  attribute    arbitrary    of a : type is 5;  -- No_Failure_here
BEGIN
  TESTING: PROCESS
  BEGIN
    wait for 5 ns;
    assert NOT( a'arbitrary = 5 )
      report "***PASSED TEST: c05s01b00x00p02n01i03090"
      severity NOTE;
    assert ( a'arbitrary = 5 )
      report "***FAILED TEST: c05s01b00x00p02n01i03090 - Attribute specification syntax test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c05s01b00x00p02n01i03090arch;
