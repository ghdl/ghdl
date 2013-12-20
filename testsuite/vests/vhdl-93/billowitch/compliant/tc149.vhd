
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
-- $Id: tc149.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x02p14n01i00149ent IS
  PORT ( ii: INOUT integer);
  PROCEDURE addup (i1,i2,i3:IN INTEGER;add:IN BOOLEAN;VARIABLE i4:OUT INTEGER) IS
  BEGIN
    IF add THEN
      i4 := (i1+i2+i3);
    ELSE
      i4 := (i1-i2)-i3;
    END IF;
  END;
END c04s03b02x02p14n01i00149ent;

ARCHITECTURE c04s03b02x02p14n01i00149arch OF c04s03b02x02p14n01i00149ent IS

BEGIN
  TESTING: PROCESS
    VARIABLE a1 : INTEGER := 57;
    VARIABLE a11: INTEGER := 57;
    VARIABLE a12: INTEGER := 57;
    VARIABLE a13: INTEGER := 57;
    VARIABLE a2 : INTEGER := 68;
    VARIABLE a3 : INTEGER := 77;
    VARIABLE b1 : BIT := '1';
    VARIABLE b2 : BIT := '0';
    FUNCTION convb (inp:IN INTEGER) RETURN BOOLEAN IS
    BEGIN
      IF (inp > 0) THEN
        RETURN (TRUE);
      ELSE
        RETURN (FALSE);
      END IF;
    END;
    FUNCTION conv1 (inp:IN BIT) RETURN INTEGER IS
    BEGIN
      IF (inp = '1') THEN
        RETURN (22);
      ELSE
        RETURN (23);
      END IF;
    END;

  BEGIN
    WAIT FOR 1 ns;
    addup(i2=>conv1(b1),add=>convb(INTEGER'HIGH),i1=>conv1(b2),i3=>a1,i4=>a1);
    WAIT FOR 1 ns;
    IF (a1 = 102) THEN
      ASSERT false REPORT "PASS: Function call uses function to convert type of actual" SEVERITY note;
    ELSE
      ASSERT false REPORT "FAIL: Function call fails" SEVERITY note;
    END IF;
    
    WAIT FOR 1 ns;
    addup(add=>convb(-33),i3=>2,i1=>a3,i2=>a2,i4=>a11);
    WAIT FOR 1 ns;
    IF (a11 = 7) THEN
      ASSERT false REPORT "PASS: Function call uses function to convert actual to false" SEVERITY note;
    ELSE
      ASSERT false REPORT "FAIL: Function call fails" SEVERITY note;
    END IF;
    
    WAIT FOR 1 ns;
    addup(add=>TRUE,i3=>conv1('1'),i2=>conv1('1'),i1=>conv1('0'),i4=>a12);
    WAIT FOR 1 ns;
    IF (a12 = 67) THEN
      ASSERT false REPORT "PASS: Function call uses same actual twice" SEVERITY note;
    ELSE
      ASSERT false REPORT "FAIL: Function call fails" SEVERITY note;
    END IF;
    
    WAIT FOR 1 ns;
    addup(15,5,5,convb(-1),a13);
    WAIT FOR 1 ns;
    IF (a13 = 5) THEN
      ASSERT false REPORT "PASS: No named association used" SEVERITY note;
    ELSE
      ASSERT false REPORT "FAIL: Function call fails" SEVERITY note;
    END IF;
    WAIT FOR 1 ns;

    assert NOT(   a1 = 102   and
                  a11= 7      and
                  a12= 67      and
                  a13= 5      )
      report "***PASSED TEST: c04s03b02x02p14n01i00149"
      severity NOTE;
    assert (   a1 = 102   and
               a11= 7      and
               a12= 67      and
               a13= 5      )
      report "***FAILED TEST: c04s03b02x02p14n01i00149 - Function call uses function to convert type of actual."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x02p14n01i00149arch;
