
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
-- $Id: tc3164.vhd,v 1.2 2001-10-26 16:29:52 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c14s01b00x00p12n01i03164ent IS
END c14s01b00x00p12n01i03164ent;

ARCHITECTURE c14s01b00x00p12n01i03164arch OF c14s01b00x00p12n01i03164ent IS
  type color is (red, green, blue);
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT( color'left = red )
      report "***PASSED TEST: c14s01b00x00p12n01i03164"
      severity NOTE;
    assert ( color'left = red )
      report "***FAILED TEST: c14s01b00x00p12n01i03164 - Predefined attribute LEFT for enumeration type test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c14s01b00x00p12n01i03164arch;
