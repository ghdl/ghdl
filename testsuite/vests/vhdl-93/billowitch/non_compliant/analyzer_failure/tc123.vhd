
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
-- $Id: tc123.vhd,v 1.2 2001-10-26 16:30:07 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x00p29n15i00123ent IS
  port (PT: linkage BOOLEAN);
END c04s03b02x00p29n15i00123ent;

ARCHITECTURE c04s03b02x00p29n15i00123arch OF c04s03b02x00p29n15i00123ent IS

BEGIN

  TESTING: PROCESS
    Variable I2 : BOOLEAN;
  BEGIN
    I2 := PT'QUIET;      -- Failure_here
    -- ERROR: ATTRIBUTES OF INTERFACE ELEMENTS OF MODE LINKAGE CANNOT BE READ
    assert FALSE
      report "***FAILED TEST: c04s03b02x00p29n15i00123 - Attributes of interface elements of mode linkage can not be read."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x00p29n15i00123arch;
