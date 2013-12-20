
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
-- $Id: tc3174.vhd,v 1.2 2001-10-26 16:29:52 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c14s01b00x00p22n01i03174ent IS
END c14s01b00x00p22n01i03174ent;

ARCHITECTURE c14s01b00x00p22n01i03174arch OF c14s01b00x00p22n01i03174ent IS
  signal gate    : BOOLEAN;
  signal s    : CHARACTER := NUL;
BEGIN
  TESTING: PROCESS
  BEGIN
    gate <= s < CHARACTER'HIGH after 2 ns;
    wait for 5 ns;
    assert NOT( gate = TRUE )
      report "***PASSED TEST: c14s01b00x00p22n01i03174"
      severity NOTE;
    assert ( gate = TRUE )
      report "***FAILED TEST: c14s01b00x00p22n01i03174 - Predefined attribute HIGH test for character type failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c14s01b00x00p22n01i03174arch;
