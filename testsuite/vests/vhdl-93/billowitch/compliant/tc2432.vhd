
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
-- $Id: tc2432.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x02p01n01i02432ent IS
END c07s03b02x02p01n01i02432ent;

ARCHITECTURE c07s03b02x02p01n01i02432arch OF c07s03b02x02p01n01i02432ent IS

BEGIN
  TESTING: PROCESS
    type arr is array (1 to 3) of integer;
    variable x: arr;
  BEGIN
    x := (1 => 1, 2 => 12, 3 => 24);  -- No_failure_here
    assert NOT(x(1)=1 and x(2)=12 and x(3)=24) 
      report "***PASSED TEST: c07s03b02x02p01n01i02432" 
      severity NOTE;
    assert (x(1)=1 and x(2)=12 and x(3)=24) 
      report "***FAILED TEST: c07s03b02x02p01n01i02432 - Expression of each element association must be of the element type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x02p01n01i02432arch;
