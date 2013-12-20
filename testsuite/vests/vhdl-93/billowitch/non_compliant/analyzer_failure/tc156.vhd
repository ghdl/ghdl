
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
-- $Id: tc156.vhd,v 1.2 2001-10-26 16:30:11 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x02p17n01i00156ent IS
  PORT ( ii: INOUT integer);
  PROCEDURE addup (i1,i2,i3:IN INTEGER;add:IN BOOLEAN;VARIABLE i4:OUT INTEGER) IS
  BEGIN
    IF add THEN
      i4 := (i1+i2+i3);
    ELSE
      i4 := (i1-i2)-i3;
    END IF;
  END;
END c04s03b02x02p17n01i00156ent;

ARCHITECTURE c04s03b02x02p17n01i00156arch OF c04s03b02x02p17n01i00156ent IS

BEGIN
  TESTING: PROCESS
    VARIABLE a1 : INTEGER := 57;
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
    addup(i2=>conv1(b1),add=>conv1(a2),i1=>conv1(b2),i3=>a1,i4=>a1);    
    WAIT FOR 1 ns;
    assert FALSE
      report "***FAILED TEST: c04s03b02x02p17n01i00156 - Type coversion return wrong type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x02p17n01i00156arch;
