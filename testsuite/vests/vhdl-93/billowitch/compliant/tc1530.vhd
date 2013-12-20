
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
-- $Id: tc1530.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s09b00x00p09n01i01530ent IS
END c08s09b00x00p09n01i01530ent;

ARCHITECTURE c08s09b00x00p09n01i01530arch OF c08s09b00x00p09n01i01530ent IS

BEGIN
  TESTING: PROCESS
    variable VAR    : REAL := 0.0;
    variable k : integer := 0;
  BEGIN
    -- Outer scope's declaration of VAR is of type REAL.
    assert (VAR = 0.0);
    if (VAR /= 0.0) then
      k := 1;
    end if;

    -- Loop using VAR as an integer.
    for VAR in 0 to 10 loop
      -- Verify that inner declaration is of type INTEGER.
      assert (VAR <= 10);
      if (VAR > 10) then
        k := 1;
      end if;
    end loop;

    -- Outer scope's declaration of VAR is of type REAL.
    assert (VAR = 0.0);
    if (VAR /= 0.0) then
      k := 1;
    end if;

    assert NOT( k=0 )
      report "***PASSED TEST: c08s09b00x00p09n01i01530"
      severity NOTE;
    assert ( k=0 )
      report "***FAILED TEST: c08s09b00x00p09n01i01530 - The loop parameter specification is the declaration of the loop parameter with a given identifier."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s09b00x00p09n01i01530arch;
