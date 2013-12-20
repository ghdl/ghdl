
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
-- $Id: tc2727.vhd,v 1.2 2001-10-26 16:30:21 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c13s04b02x00p07n01i02727ent IS
END c13s04b02x00p07n01i02727ent;

ARCHITECTURE c13s04b02x00p07n01i02727arch OF c13s04b02x00p07n01i02727ent IS

BEGIN
  TESTING: PROCESS
    variable total_time : real;
  BEGIN
    total_time := 6#6589.55#;   --Failure_here
    assert FALSE
      report "***FAILED TEST: c13s04b02x00p07n01i02727 - The value of each digit in a based literal must be less than that of the base."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c13s04b02x00p07n01i02727arch;
