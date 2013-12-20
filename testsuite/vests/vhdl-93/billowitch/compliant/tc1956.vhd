
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
-- $Id: tc1956.vhd,v 1.2 2001-10-26 16:29:44 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b01x00p02n02i01956ent IS
END c07s02b01x00p02n02i01956ent;

ARCHITECTURE c07s02b01x00p02n02i01956arch OF c07s02b01x00p02n02i01956ent IS

BEGIN
  TESTING: PROCESS
    variable a : boolean := TRUE;
    variable b : boolean := TRUE;
    variable c : boolean;
  BEGIN
    c := a or b;   
    assert NOT(c=TRUE)
      report "***PASSED TEST: c07s02b01x00p02n02i01956"
      severity NOTE;
    assert ( c=TRUE )
      report "***FAILED TEST: c07s02b01x00p02n02i01956 - Logical operation of 'OR'."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b01x00p02n02i01956arch;
