
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
-- $Id: tc2431.vhd,v 1.2 2001-10-26 16:30:18 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x02p01n01i02431ent IS
END c07s03b02x02p01n01i02431ent;

ARCHITECTURE c07s03b02x02p01n01i02431arch OF c07s03b02x02p01n01i02431ent IS

BEGIN
  TESTING: PROCESS
    type array_three is array (1 to 6) of integer;
    variable x : array_three := (1=>2,2=>3,3=>4,4=>6.32,5=>6,6=>7);  -- Failure_here
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c07s03b02x02p01n01i02431 - Expression of each element association must be of the element type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x02p01n01i02431arch;
