
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
-- $Id: tc182.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s04b00x00p04n02i00182ent IS
END c04s04b00x00p04n02i00182ent;

ARCHITECTURE c04s04b00x00p04n02i00182arch OF c04s04b00x00p04n02i00182ent IS
  type COORDINATE is
    record
      X, Y: INTEGER;
    end record;
  attribute    LOCATION         : COORDINATE;
  signal       loc1, loc2    : COORDINATE;
  attribute LOCATION    of loc1   : signal is (10, 15);
  attribute LOCATION    of others : signal is (25, 77);
BEGIN
  TESTING: PROCESS
  BEGIN
    wait for 5 ns;
    assert NOT(    loc1'LOCATION = (10, 15)   and
                   loc2'LOCATION = (25, 77)   )
      report "***PASSED TEST: c04s04b00x00p04n02i00182"
      severity NOTE;
    assert (    loc1'LOCATION = (10, 15)   and
                loc2'LOCATION = (25, 77)   )
      report "***FAILED TEST: c04s04b00x00p04n02i00182 - Attribute associated with a signal test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s04b00x00p04n02i00182arch;
