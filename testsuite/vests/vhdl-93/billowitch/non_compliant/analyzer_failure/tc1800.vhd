
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
-- $Id: tc1800.vhd,v 1.2 2001-10-26 16:30:13 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s01b00x00p03n01i01800ent IS
END c07s01b00x00p03n01i01800ent;

ARCHITECTURE c07s01b00x00p03n01i01800arch OF c07s01b00x00p03n01i01800ent IS

BEGIN
  TESTING: PROCESS
    variable x : real := 4;
    variable y : real := 6.7;
    variable z : real := 4.8;
    variable p : real;
  BEGIN
    if  y = x = z then   -- Failure_here
      -- only a single relational operator allowed.
      p := y + z + x;
    end if;
    assert FALSE 
      report "***FAILED TEST: c07s01b00x00p03n01i01800 - Only a single relational operator is used to combine expressions and form realtions."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s01b00x00p03n01i01800arch;
