
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
-- $Id: tc929.vhd,v 1.2 2001-10-26 16:30:28 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c10s04b00x00p01n01i00929pkg is
  type p2 is (a, b);
end c10s04b00x00p01n01i00929pkg;

use work.all;
ENTITY c10s04b00x00p01n01i00929ent IS
END c10s04b00x00p01n01i00929ent;

ARCHITECTURE c10s04b00x00p01n01i00929arch OF c10s04b00x00p01n01i00929ent IS
  signal s: p2;   -- Failure_here
  -- should report an error as the type p2 is not visible.
BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c10s04b00x00p01n01i00929 - Type definition does not exist in scope of declaration region for architecture." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c10s04b00x00p01n01i00929arch;
