
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
-- $Id: tc187.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s04b00x00p12n01i00187ent IS
  attribute ATE             : INTEGER;
  attribute ATE of  c04s04b00x00p12n01i00187ent   : entity is 2;
  --Correct placement in interface declaration
END c04s04b00x00p12n01i00187ent;

ARCHITECTURE c04s04b00x00p12n01i00187arch OF c04s04b00x00p12n01i00187ent IS

BEGIN
  TESTING: PROCESS
    variable S : integer;
  BEGIN
    S := c04s04b00x00p12n01i00187ent'ATE;
    assert NOT( S = 2 )
      report "***PASSED TEST: c04s04b00x00p12n01i00187"
      severity NOTE;
    assert ( S = 2 )
      report "***FAILED TEST: c04s04b00x00p12n01i00187 - Attribute specification of the entity test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s04b00x00p12n01i00187arch;
