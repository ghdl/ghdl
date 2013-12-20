
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
-- $Id: tc1550.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s09b00x00p10n01i01550ent IS
END c08s09b00x00p10n01i01550ent;

ARCHITECTURE c08s09b00x00p10n01i01550arch OF c08s09b00x00p10n01i01550ent IS

BEGIN
  TESTING: PROCESS
    type colors is (red, yellow, blue);
    variable k : integer := 0;
  BEGIN
    --
    -- Test for loop; loop should initialize
    -- the loop variable and sequence through
    -- all three colors if implemented correctly
    --
    -- 'c' is declared in the loop parameter spec.
    --
    L1: for c in red to blue loop
      case c is
        when red =>
          k := k + 1;
        when yellow =>
          k := k + 10;
        when blue =>
          k := k + 100;
        when others =>
          k := 0;
      end case;
    end loop L1;

    assert NOT( k=111 )
      report "***PASSED TEST: c08s09b00x00p10n01i01550"
      severity NOTE;
    assert ( k=111 )
      report "***FAILED TEST: c08s09b00x00p10n01i01550 - The loop parameter is declared by its appearance in the loop parameter specification and its scope is limited to the loop statement." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s09b00x00p10n01i01550arch;
