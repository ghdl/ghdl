
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
-- $Id: tc86.vhd,v 1.2 2001-10-26 16:30:01 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b01x03p05n02i00086ent IS
END c04s03b01x03p05n02i00086ent;

ARCHITECTURE c04s03b01x03p05n02i00086arch OF c04s03b01x03p05n02i00086ent IS

BEGIN
  TESTING: PROCESS
    type some_type is (Sunday,Monday,Tuesday,Wed,Thur,Fri);
    variable X : some_type;    -- No_failure_here
    -- no default value declared.
  BEGIN

    assert NOT( X=Sunday )
      report "***PASSED TEST: c04s03b01x03p05n02i00086"
      severity NOTE;
    assert ( X=Sunday )
      report "***FAILED TEST: c04s03b01x03p05n02i00086 - Variable default assignment failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b01x03p05n02i00086arch;
