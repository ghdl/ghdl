
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
-- $Id: tc294.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s01b03x01p01n02i00294ent IS
END c03s01b03x01p01n02i00294ent;

ARCHITECTURE c03s01b03x01p01n02i00294arch OF c03s01b03x01p01n02i00294ent IS
  signal T1 : TIME := -2147483647 ns;
  signal T2 : TIME := +2147483467 ns;   -- no_failure_here
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT(T1=-2147483647 ns and T2=+2147483467 ns)
      report "***PASSED TEST:c03s01b03x01p01n02i00294"
      severity NOTE;
    assert (T1=-2147483647 ns and T2=+2147483467 ns)
      report "***FAILED TEST: c03s01b03x01p01n02i00294 - The range of TIME is guaranteed to include the range -2147483647 to +2147483467."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s01b03x01p01n02i00294arch;
