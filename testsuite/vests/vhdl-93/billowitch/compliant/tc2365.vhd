
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
-- $Id: tc2365.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b01x00p01n01i02365ent IS
END c07s03b01x00p01n01i02365ent;

ARCHITECTURE c07s03b01x00p01n01i02365arch OF c07s03b01x00p01n01i02365ent IS
  type    MVL is ('0','1','X','Z') ;
  signal    S2  : MVL := '0';
BEGIN
  TESTING: PROCESS
  BEGIN
    S2 <= 'X';
    wait for 1 ns;
    assert NOT(S2 = 'X')
      report "***PASSED TEST: c07s03b01x00p01n01i02365"
      severity NOTE;
    assert (S2 = 'X')
      report "***FAILED TEST: c07s03b01x00p01n01i02365 - A literal is an enumeration literal."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b01x00p01n01i02365arch;
