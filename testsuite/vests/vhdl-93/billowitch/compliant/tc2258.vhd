
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
-- $Id: tc2258.vhd,v 1.2 2001-10-26 16:29:46 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b06x00p05n01i02258ent IS
END c07s02b06x00p05n01i02258ent;

ARCHITECTURE c07s02b06x00p05n01i02258arch OF c07s02b06x00p05n01i02258ent IS

BEGIN
  TESTING: PROCESS
    constant mul11 : integer := (1 - 4) * (1 - 4);
    constant mul12 : integer := (1 - 4) * (2 - 4);
    constant mul13 : integer := (1 - 4) * (3 - 4);
    constant mul14 : integer := (1 - 4) * (4 - 4);
    constant mul15 : integer := (1 - 4) * (5 - 4);
    constant mul16 : integer := (1 - 4) * (6 - 4);
    constant mul17 : integer := (1 - 4) * (7 - 4);
    constant mul18 : integer := (1 - 4) * (8 - 4);
    constant mul19 : integer := (1 - 4) * (9 - 4);
    constant mul41 : integer := (4 - 4) * (1 - 4);
    constant mul42 : integer := (4 - 4) * (2 - 4);
    constant mul43 : integer := (4 - 4) * (3 - 4);
    constant mul44 : integer := (4 - 4) * (4 - 4);
    constant mul45 : integer := (4 - 4) * (5 - 4);
    constant mul46 : integer := (4 - 4) * (6 - 4);
    constant mul47 : integer := (4 - 4) * (7 - 4);
    constant mul48 : integer := (4 - 4) * (8 - 4);
    constant mul49 : integer := (4 - 4) * (9 - 4);
    constant mul61 : integer := (6 - 4) * (1 - 4);
    constant mul62 : integer := (6 - 4) * (2 - 4);
    constant mul63 : integer := (6 - 4) * (3 - 4);
    constant mul64 : integer := (6 - 4) * (4 - 4);
    constant mul65 : integer := (6 - 4) * (5 - 4);
    constant mul66 : integer := (6 - 4) * (6 - 4);
    constant mul67 : integer := (6 - 4) * (7 - 4);
    constant mul68 : integer := (6 - 4) * (8 - 4);
    constant mul69 : integer := (6 - 4) * (9 - 4);

    variable four : integer := 4;

  BEGIN

    assert mul11 = (1 - four) * (1 - four);
    assert mul12 = (1 - four) * (2 - four);
    assert mul13 = (1 - four) * (3 - four);
    assert mul14 = (1 - four) * (4 - four);
    assert mul15 = (1 - four) * (5 - four);
    assert mul16 = (1 - four) * (6 - four);
    assert mul17 = (1 - four) * (7 - four);
    assert mul18 = (1 - four) * (8 - four);
    assert mul19 = (1 - four) * (9 - four);
    assert mul41 = (4 - four) * (1 - four);
    assert mul42 = (4 - four) * (2 - four);
    assert mul43 = (4 - four) * (3 - four);
    assert mul44 = (4 - four) * (4 - four);
    assert mul45 = (4 - four) * (5 - four);
    assert mul46 = (4 - four) * (6 - four);
    assert mul47 = (4 - four) * (7 - four);
    assert mul48 = (4 - four) * (8 - four);
    assert mul49 = (4 - four) * (9 - four);
    assert mul61 = (6 - four) * (1 - four);
    assert mul62 = (6 - four) * (2 - four);
    assert mul63 = (6 - four) * (3 - four);
    assert mul64 = (6 - four) * (4 - four);
    assert mul65 = (6 - four) * (5 - four);
    assert mul66 = (6 - four) * (6 - four);
    assert mul67 = (6 - four) * (7 - four);
    assert mul68 = (6 - four) * (8 - four);
    assert mul69 = (6 - four) * (9 - four);

    assert NOT(( mul11 = (1 - four) * (1 - four))   and
               ( mul12 = (1 - four) * (2 - four))   and
               ( mul13 = (1 - four) * (3 - four))   and
               ( mul14 = (1 - four) * (4 - four))   and
               ( mul15 = (1 - four) * (5 - four))   and
               ( mul16 = (1 - four) * (6 - four))   and
               ( mul17 = (1 - four) * (7 - four))   and
               ( mul18 = (1 - four) * (8 - four))   and
               ( mul19 = (1 - four) * (9 - four))   and
               ( mul41 = (4 - four) * (1 - four))   and
               ( mul42 = (4 - four) * (2 - four))   and
               ( mul43 = (4 - four) * (3 - four))   and
               ( mul44 = (4 - four) * (4 - four))   and
               ( mul45 = (4 - four) * (5 - four))   and
               ( mul46 = (4 - four) * (6 - four))   and
               ( mul47 = (4 - four) * (7 - four))   and
               ( mul48 = (4 - four) * (8 - four))   and
               ( mul49 = (4 - four) * (9 - four))   and
               ( mul61 = (6 - four) * (1 - four))   and
               ( mul62 = (6 - four) * (2 - four))   and
               ( mul63 = (6 - four) * (3 - four))   and
               ( mul64 = (6 - four) * (4 - four))   and
               ( mul65 = (6 - four) * (5 - four))   and
               ( mul66 = (6 - four) * (6 - four))   and
               ( mul67 = (6 - four) * (7 - four))   and
               ( mul68 = (6 - four) * (8 - four))   and
               ( mul69 = (6 - four) * (9 - four))   )
      report "***PASSED TEST: c07s02b06x00p05n01i02258"
      severity NOTE;
    assert (( mul11 = (1 - four) * (1 - four))   and
            ( mul12 = (1 - four) * (2 - four))   and
            ( mul13 = (1 - four) * (3 - four))   and
            ( mul14 = (1 - four) * (4 - four))   and
            ( mul15 = (1 - four) * (5 - four))   and
            ( mul16 = (1 - four) * (6 - four))   and
            ( mul17 = (1 - four) * (7 - four))   and
            ( mul18 = (1 - four) * (8 - four))   and
            ( mul19 = (1 - four) * (9 - four))   and
            ( mul41 = (4 - four) * (1 - four))   and
            ( mul42 = (4 - four) * (2 - four))   and
            ( mul43 = (4 - four) * (3 - four))   and
            ( mul44 = (4 - four) * (4 - four))   and
            ( mul45 = (4 - four) * (5 - four))   and
            ( mul46 = (4 - four) * (6 - four))   and
            ( mul47 = (4 - four) * (7 - four))   and
            ( mul48 = (4 - four) * (8 - four))   and
            ( mul49 = (4 - four) * (9 - four))   and
            ( mul61 = (6 - four) * (1 - four))   and
            ( mul62 = (6 - four) * (2 - four))   and
            ( mul63 = (6 - four) * (3 - four))   and
            ( mul64 = (6 - four) * (4 - four))   and
            ( mul65 = (6 - four) * (5 - four))   and
            ( mul66 = (6 - four) * (6 - four))   and
            ( mul67 = (6 - four) * (7 - four))   and
            ( mul68 = (6 - four) * (8 - four))   and
            ( mul69 = (6 - four) * (9 - four))   )
      report "***FAILED TEST: c07s02b06x00p05n01i02258 - Constant integer type multiplication test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b06x00p05n01i02258arch;
