
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
-- $Id: tc2573.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c13s02b00x00p02n01i02573ent IS
  type ONE    is range 10#1#    to 1;
  type TWO    is range 2    to 2;
  type THREE    is range 3    to 3;
  type FOUR    is range 4    to 4;
  type A1    is array(FOUR range<>)of FOUR;
  type FIVE    is range 1    to 5;
  type U1    is range 1    to 200
    units
      SINGLE;
      EVEN   =2 SINGLE;
      DOUBLE   =10#1#E1 EVEN;
      QUAD   =2E1 EVEN;
    end units;
END c13s02b00x00p02n01i02573ent;

ARCHITECTURE c13s02b00x00p02n01i02573arch OF c13s02b00x00p02n01i02573ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***PASSED TEST: c13s02b00x00p02n01i02573"
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c13s02b00x00p02n01i02573arch;
