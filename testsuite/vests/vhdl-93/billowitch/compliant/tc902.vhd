
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
-- $Id: tc902.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c10s03b00x00p05n01i00902ent IS
  type work is (foo,bar); -- No_Failure_here
END c10s03b00x00p05n01i00902ent;

ARCHITECTURE c10s03b00x00p05n01i00902arch OF c10s03b00x00p05n01i00902ent IS

BEGIN
  TESTING: PROCESS
    variable var : work := foo;
  BEGIN
    wait for 5 ns;
    assert NOT( var = foo )
      report "***PASSED TEST: c10s03b00x00p05n01i00902"
      severity NOTE;
    assert ( var = foo )
      report "***FAILED TEST: c10s03b00x00p05n01i00902 - The declaration should be visible in the architecture."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c10s03b00x00p05n01i00902arch;
