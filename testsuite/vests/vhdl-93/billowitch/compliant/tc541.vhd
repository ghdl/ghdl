
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
-- $Id: tc541.vhd,v 1.2 2001-10-26 16:29:56 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s03b02x00p06n01i00541ent IS
END c03s03b02x00p06n01i00541ent;

ARCHITECTURE c03s03b02x00p06n01i00541arch OF c03s03b02x00p06n01i00541ent IS

BEGIN
  TESTING: PROCESS
    -- Enumerated types.
    type    SWITCH_LEVEL is ('0', '1', 'X');
    type    AC1 is access SWITCH_LEVEL;

    -- Declare a variable.
    variable VAR : AC1;
  BEGIN
    -- Perform the test.
    assert( VAR = NULL );
    DEALLOCATE( VAR );
    assert NOT( VAR = NULL )
      report "***PASSED TEST: c03s03b02x00p06n01i00541"
      severity NOTE;
    assert ( VAR = NULL )
      report "***FAILED TEST: c03s03b02x00p06n01i00541 - DEALLOCATE operation test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s03b02x00p06n01i00541arch;
