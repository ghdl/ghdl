
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
-- $Id: tc340.vhd,v 1.2 2001-10-26 16:30:25 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x00p08n01i00340ent IS
  PORT ( ii: INOUT integer);
  TYPE A IS ARRAY (NATURAL RANGE <>) OF INTEGER;
  TYPE Z IS ARRAY (NATURAL RANGE <>,NATURAL RANGE <>,NATURAL RANGE <>) OF INTEGER;
  SUBTYPE A8 IS A (1 TO 8,1 TO 8,1 TO 8);
  SUBTYPE Z3 IS Z (1 TO 3,1 TO 3);
  SUBTYPE Z6 IS Z (1 TO 6,1 TO 6,1 TO 6);
  FUNCTION func1 (a,b : INTEGER := 3) RETURN Z6 IS
  BEGIN
    RETURN (OTHERS=>(OTHERS=>(1,2,3,4,5,6)));
  END;
END c03s02b01x00p08n01i00340ent;

ARCHITECTURE c03s02b01x00p08n01i00340arch OF c03s02b01x00p08n01i00340ent IS

BEGIN
  TESTING: PROCESS
    VARIABLE q : A8;
    VARIABLE r : Z3;
  BEGIN
    assert FALSE
      report "***FAILED TEST: c03s02b01x00p08n01i00340 - Array subtype has fewer dimensions than base type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x00p08n01i00340arch;
