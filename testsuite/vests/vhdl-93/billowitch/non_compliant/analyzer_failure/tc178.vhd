
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
-- $Id: tc178.vhd,v 1.2 2001-10-26 16:30:12 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s04b00x00p01n05i00178ent IS
END c04s04b00x00p01n05i00178ent;

ARCHITECTURE c04s04b00x00p01n05i00178arch OF c04s04b00x00p01n05i00178ent IS
  signal S1 : INTEGER;
  signal S2 : BOOLEAN;   
BEGIN
  TESTING: PROCESS
  BEGIN
    S1'DELAYED <= S2;  -- Failure_here
    -- ERROR - predefined signal attribute must not be driven
    S1'STABLE <= S2;  -- Failure_here
    -- ERROR - predefined signal attribute must not be driven
    S1'QUIET <= S2; -- Failure_here
    -- ERROR - predefined signal attribute must not be driven
    assert FALSE 
      report "***FAILED TEST: c04s04b00x00p01n05i00178 - Predefined atttribute DELAYED can not be driven."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s04b00x00p01n05i00178arch;
