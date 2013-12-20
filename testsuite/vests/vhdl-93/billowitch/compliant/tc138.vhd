
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
-- $Id: tc138.vhd,v 1.2 2001-10-26 16:29:40 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x02p09n01i00138ent IS
  CONSTANT a,b    : INTEGER := 2;
  CONSTANT c       : INTEGER := 3; 

  PROCEDURE addup (i1,i2,i3:IN INTEGER:=a**b+c-a;SIGNAL i4:OUT INTEGER) IS
  BEGIN
    i4 <= (i1+i2+i3);
  END;
END c04s03b02x02p09n01i00138ent;

ARCHITECTURE c04s03b02x02p09n01i00138arch OF c04s03b02x02p09n01i00138ent IS
  SIGNAL a1 : INTEGER := 57;
  SIGNAL a2 : INTEGER := 68;
  SIGNAL a3 : INTEGER := 77;
  SIGNAL a11: INTEGER := 77;
  SIGNAL a12: INTEGER := 77;
  SIGNAL a13: INTEGER := 77;
BEGIN
  TESTING: PROCESS
  BEGIN
    WAIT FOR 1 ns;
    addup(i2=>a1,i1=>a1,i4=>a1);
    WAIT FOR 1 ns;
    IF (a1 = 119) THEN
      ASSERT false REPORT "PASS: Function call uses same actual twice plus default" SEVERITY note;
    ELSE
      ASSERT false REPORT "FAIL: Function call fails" SEVERITY error;
    END IF;
    
    WAIT FOR 1 ns;
    addup(i3=>a2,i1=>a2,i2=>a3,i4=>a11);
    WAIT FOR 1 ns;
    IF (a11 = 213) THEN
      ASSERT false REPORT "PASS: Function call uses same actual twice" SEVERITY note;
    ELSE
      ASSERT false REPORT "FAIL: Function call fails" SEVERITY error;
    END IF;
    
    WAIT FOR 1 ns;
    addup(i3=>a3,i2=>a3,i1=>a3,i4=>a12);
    WAIT FOR 1 ns;
    IF (a12 = 231) THEN
      ASSERT false REPORT "PASS: Function call uses same actual thrice" SEVERITY note;
    ELSE
      ASSERT false REPORT "FAIL: Function call fails" SEVERITY error;
    END IF;
    
    WAIT FOR 1 ns;
    addup(i4=>a13);
    WAIT FOR 1 ns;
    IF (a13 = 15) THEN
      ASSERT false REPORT "PASS: All parameters defaulted to same value" SEVERITY note;
    ELSE
      ASSERT false REPORT "FAIL: Function call fails" SEVERITY error;
    END IF;
    wait for 5 ns;
    assert NOT(   a1 = 119   and
                  a11= 213   and
                  a12= 231   and
                  a13= 15      )   
      report "***PASSED TEST: c04s03b02x02p09n01i00138"
      severity NOTE;
    assert (   a1 = 119   and
               a11= 213   and
               a12= 231   and
               a13= 15      )   
      report "***FAILED TEST: c04s03b02x02p09n01i00138 - Named association where 2 or more named formal signals are associated with the same actual signal test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x02p09n01i00138arch;
