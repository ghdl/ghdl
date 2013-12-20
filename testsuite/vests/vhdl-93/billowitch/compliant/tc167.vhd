
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
-- $Id: tc167.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b03x00p01n01i00167ent IS
END c04s03b03x00p01n01i00167ent;

ARCHITECTURE c04s03b03x00p01n01i00167arch OF c04s03b03x00p01n01i00167ent IS

BEGIN
  TESTING: PROCESS
    constant    C1 : INTEGER := 1;
    alias       a1 : INTEGER is C1;

    constant    C2 : STRING := "Hello";
    alias       a2 : STRING(4 downto 1) is C2(1 to 4);
    alias       a3 : STRING(1 to 5)    is C2;

    alias       a4 : CHARACTER is C2(2);
  BEGIN
    assert C1 = 1;
    assert A1 = 1;
    assert C2 = "Hello";
    assert A2 = "Hell";
    assert A3 = "Hello";
    assert A4 = 'e';
    assert NOT(    C1 = 1   and
                   A1 = 1   and
                   C2 = "Hello"   and
                   A2 = "Hell"   and
                   A3 = "Hello"   and
                   A4 = 'e'   )
      report "***PASSED TEST: c04s03b03x00p01n01i00167"
      severity NOTE;
    assert (    C1 = 1   and
                A1 = 1   and
                C2 = "Hello"   and
                A2 = "Hell"   and
                A3 = "Hello"   and
                A4 = 'e'   )
      report "***FAILED TEST: c04s03b03x00p01n01i00167 - Alias for constant object test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b03x00p01n01i00167arch;
