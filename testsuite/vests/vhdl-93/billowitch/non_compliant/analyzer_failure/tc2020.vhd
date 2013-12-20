
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
-- $Id: tc2020.vhd,v 1.2 2001-10-26 16:30:15 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b04x00p01n01i02020ent IS
END c07s02b04x00p01n01i02020ent;

ARCHITECTURE c07s02b04x00p01n01i02020arch OF c07s02b04x00p01n01i02020ent IS

BEGIN
  TESTING: PROCESS
    type array_type is array (1 to 10) of integer;
    variable x : array_type := (1,2,3,4,5,6,7,8,9,10);
    variable y : array_type := (1,2,3,4,5,6,7,8,9,0);
    variable z : array_type;
  BEGIN
    z := (x - y); -- Failure_here
    assert FALSE 
      report "***FAILED TEST: c07s02b04x00p01n01i02020 - The adding operators are predefined only for numeric types." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b04x00p01n01i02020arch;
