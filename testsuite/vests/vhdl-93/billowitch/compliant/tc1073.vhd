
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
-- $Id: tc1073.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s04b00x00p03n01i01073ent IS
  PORT ( ii: INOUT integer);
  TYPE A IS ARRAY (NATURAL RANGE <>) OF INTEGER;
  TYPE Z IS ARRAY (NATURAL RANGE <>,NATURAL RANGE <>,NATURAL RANGE <>) OF INTEGER;
  SUBTYPE A8 IS A (1 TO 8);
  SUBTYPE Z3 IS Z (1 TO 3,1 TO 3,1 TO 3);
  SUBTYPE Z6 IS Z (1 TO 6,1 TO 6,1 TO 6);

  FUNCTION func1 (a,b : INTEGER := 3) RETURN Z6 IS
  BEGIN
    IF (a=3) AND (b=3) THEN
      RETURN (OTHERS=>(OTHERS=>(1,2,3,4,5,6)));
    ELSE
      IF (a=3) THEN
        RETURN (OTHERS=>(OTHERS=>(11,22,33,44,55,66)));
      ELSE
        RETURN (OTHERS=>(OTHERS=>(111,222,333,444,555,666)));
      END IF;
    END IF;
  END;
END c06s04b00x00p03n01i01073ent;

ARCHITECTURE c06s04b00x00p03n01i01073arch OF c06s04b00x00p03n01i01073ent IS
BEGIN
  TESTING: PROCESS
    VARIABLE q : A8;
  BEGIN
    q(1) := func1(3,0)(1,1,1);
    q(2) := func1(0,3)(2,2,2);
    q(3) := func1(0,0)(3,3,3);
    q(4) := func1(4,4,4); -- Indexed name - function params defaulted
    q(5) := func1(5,5,5);
    q(6) := func1(6,6,6);
    q(7) := func1(3,3,3);
    q(8) := func1(1,1,1);
    WAIT FOR 1 ns;
    assert NOT(q(1 TO 8) = (1=>11,2=>222,3=>333,4=>4,5=>5,6=>6,7=>3,8=>1))
      report "***PASSED TEST: c06s04b00x00p03n01i01073" 
      severity NOTE;
    assert (q(1 TO 8) = (1=>11,2=>222,3=>333,4=>4,5=>5,6=>6,7=>3,8=>1))
      report "***FAILED TEST:c06s04b00x00p03n01i01073 - Index on functin call test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s04b00x00p03n01i01073arch;
