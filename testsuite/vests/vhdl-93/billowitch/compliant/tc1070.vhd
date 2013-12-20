
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
-- $Id: tc1070.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s04b00x00p03n01i01070ent IS
  PORT ( ii: INOUT integer);

  TYPE    A  IS ARRAY (NATURAL RANGE <>) OF INTEGER;
  SUBTYPE    A6 IS A (1 TO 6);
  SUBTYPE    A8 IS A (1 TO 8);

  FUNCTION func1 (a,b : INTEGER := 3) RETURN A6 IS
  BEGIN
    IF (a=3) AND (b=3) THEN
      RETURN (1,2,3,4,5,6);
    ELSE
      IF (a=3) THEN
        RETURN (11,22,33,44,55,66);
      ELSE
        RETURN (111,222,333,444,555,666);
      END IF;
    END IF;
  END;
END c06s04b00x00p03n01i01070ent;

ARCHITECTURE c06s04b00x00p03n01i01070arch OF c06s04b00x00p03n01i01070ent IS
BEGIN
  TESTING: PROCESS
    VARIABLE q : A8;
  BEGIN
    q(1) := func1(3,3)(1);
    q(2) := func1(0,3)(2);
    q(3) := func1(3,0)(3);
    q(4) := func1(0,3)(4);
    q(5) := func1(3,3)(5);
    q(6) := func1(3,0)(6);
    q(7) := func1(3,3)(3);
    q(8) := func1(0,3)(1);
    WAIT FOR 1 ns;
    assert NOT(q(1 TO 8) = (1=>1,2=>222,3=>33,4=>444,5=>5,6=>66,7=>3,8=>111))
      report "***PASSED TEST: c06s04b00x00p03n01i01070" 
      severity NOTE;
    assert (q(1 TO 8) = (1=>1,2=>222,3=>33,4=>444,5=>5,6=>66,7=>3,8=>111))
      report "***FAILED TEST: c06s04b00x00p03n01i01070 - Index on functin call test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s04b00x00p03n01i01070arch;
