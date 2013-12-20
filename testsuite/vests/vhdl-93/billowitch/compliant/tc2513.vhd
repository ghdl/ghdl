
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
-- $Id: tc2513.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b05x00p02n01i02513ent IS
END c07s03b05x00p02n01i02513ent;

ARCHITECTURE c07s03b05x00p02n01i02513arch OF c07s03b05x00p02n01i02513ent IS

BEGIN
  TESTING: PROCESS
    variable k1 : integer    := 65;
    variable k2 : real   := 1.2;
  BEGIN
    k1 := integer(k2);
    wait for 1 ns;
    assert NOT(k1 = 1) 
      report "***PASSED TEST: c07s03b05x00p02n01i02513" 
      severity NOTE;
    assert (k1 = 1) 
      report "***FAILED TEST: c07s03b05x00p02n01i02513 - Missing expression."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b05x00p02n01i02513arch;
