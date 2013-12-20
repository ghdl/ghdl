
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
-- $Id: tc3085.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s06b03x00p03n01i03085ent IS
END c12s06b03x00p03n01i03085ent;

ARCHITECTURE c12s06b03x00p03n01i03085arch OF c12s06b03x00p03n01i03085ent IS
  signal S1 : BIT;
BEGIN
  S1 <= transport '1' after 5 ns;
  TESTING: PROCESS
  BEGIN
    wait on S1;
    assert ( S1'STABLE )
      report "***PASSED TEST: c12s06b03x00p03n01i03085"
      severity NOTE;
    assert NOT( S1'STABLE )
      report "***FAILED TEST: c12s06b03x00p03n01i03085 - An event occurred on S in this simulation cycle and the current value of the signal is modified."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c12s06b03x00p03n01i03085arch;
