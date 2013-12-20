
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
-- $Id: tc1529.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s09b00x00p08n01i01529ent IS
END c08s09b00x00p08n01i01529ent;

ARCHITECTURE c08s09b00x00p08n01i01529arch OF c08s09b00x00p08n01i01529ent IS

BEGIN
  TESTING: PROCESS
    variable INTV   : INTEGER := 0;
    variable COUNTV : INTEGER := 0;
    variable I      : INTEGER := 0;
    variable   k      : integer := 0;
  BEGIN

    -- While condition is FALSE, so no stmts are executed.
    while (FALSE) loop
      assert (FALSE)
        report "First loop was executed when it should not have been.";
      k := 1;
    end loop;

    -- While condition is FALSE, so no stmts are executed.
    while (I /= 0) loop
      assert (FALSE)
        report "Second loop was executed when it should not have been.";
      k := 1;
    end loop;

    -- Verify that loop is executed right number of times.
    COUNTV := 0;
    while (I /= 10) loop
      I := I + 1;
      COUNTV := COUNTV + 1;
    end loop;
    if (I /= 10 and COUNTV /= 10) then
      k := 1;
    end if;
    assert (I = 10);
    assert (COUNTV = 10);

    assert NOT(k=0) 
      report "***PASSED TEST: c08s09b00x00p08n01i01529"
      severity NOTE;
    assert (k=0) 
      report "***FAILED TEST: c08s09b00x00p08n01i01529 - while condition is not boolean expression" 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s09b00x00p08n01i01529arch;
