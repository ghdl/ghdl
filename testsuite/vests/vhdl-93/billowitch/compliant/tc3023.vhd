
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
-- $Id: tc3023.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

library WORK, STD;

ENTITY c11s02b00x00p05n01i03023ent IS
END c11s02b00x00p05n01i03023ent;

ARCHITECTURE c11s02b00x00p05n01i03023arch OF c11s02b00x00p05n01i03023ent IS
  signal S1 : STD.STANDARD.bit;   -- No_failure_here
BEGIN
  TESTING: PROCESS
  BEGIN
    S1 <= '1' after 20 ns;
    wait for 30 ns;
    assert NOT( S1 = '1' )
      report "***PASSED TEST: c11s02b00x00p05n01i03023"
      severity NOTE;
    assert ( S1 = '1' )
      report "***FAILED TEST: c11s02b00x00p05n01i03023 - Library logical name may be referenced in the design unit."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c11s02b00x00p05n01i03023arch;
