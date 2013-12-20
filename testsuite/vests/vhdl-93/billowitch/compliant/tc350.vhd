
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
-- $Id: tc350.vhd,v 1.2 2001-10-26 16:29:53 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x01p01n01i00350ent IS
END c03s02b01x01p01n01i00350ent;

ARCHITECTURE c03s02b01x01p01n01i00350arch OF c03s02b01x01p01n01i00350ent IS

BEGIN
  TESTING: PROCESS
    variable    V1 :    STRING(1 to 20);
    subtype    ST is    STRING(1 to 10);
    variable    V2 :    ST;
  BEGIN
    assert V1'LEFT    = 1;
    assert V1'RIGHT = 20;
    assert ST'LEFT    = 1;
    assert ST'RIGHT = 10;
    assert V2'LEFT    = 1;
    assert V2'RIGHT = 10;
    assert NOT(     V1'LEFT  = 1    and
                    V1'RIGHT = 20   and
                    ST'LEFT  = 1   and
                    ST'RIGHT = 10   and
                    V2'LEFT  = 1   and
                    V2'RIGHT = 10   )
      report "***PASSED TEST: c03s02b01x01p01n01i00350"
      severity NOTE;
    assert (     V1'LEFT  = 1    and
                 V1'RIGHT = 20   and
                 ST'LEFT  = 1   and
                 ST'RIGHT = 10   and
                 V2'LEFT  = 1   and
                 V2'RIGHT = 10   )
      report "***FAILED TEST: c03s02b01x01p01n01i00350 - Index constraint test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p01n01i00350arch;
