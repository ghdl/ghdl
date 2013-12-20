
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
-- $Id: tc2259.vhd,v 1.2 2001-10-26 16:29:46 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b06x00p05n01i02259ent IS
END c07s02b06x00p05n01i02259ent;

ARCHITECTURE c07s02b06x00p05n01i02259arch OF c07s02b06x00p05n01i02259ent IS
BEGIN
  TESTING: PROCESS
    constant mod11 : integer := (1 - 4) mod (1 - 4);
    constant mod12 : integer := (1 - 4) mod (2 - 4);
    constant mod13 : integer := (1 - 4) mod (3 - 4);
    constant mod15 : integer := (1 - 4) mod (5 - 4);
    constant mod16 : integer := (1 - 4) mod (6 - 4);
    constant mod17 : integer := (1 - 4) mod (7 - 4);
    constant mod18 : integer := (1 - 4) mod (8 - 4);
    constant mod19 : integer := (1 - 4) mod (9 - 4);
    constant mod41 : integer := (4 - 4) mod (1 - 4);
    constant mod42 : integer := (4 - 4) mod (2 - 4);
    constant mod43 : integer := (4 - 4) mod (3 - 4);
    constant mod45 : integer := (4 - 4) mod (5 - 4);
    constant mod46 : integer := (4 - 4) mod (6 - 4);
    constant mod47 : integer := (4 - 4) mod (7 - 4);
    constant mod48 : integer := (4 - 4) mod (8 - 4);
    constant mod49 : integer := (4 - 4) mod (9 - 4);
    constant mod61 : integer := (6 - 4) mod (1 - 4);
    constant mod62 : integer := (6 - 4) mod (2 - 4);
    constant mod63 : integer := (6 - 4) mod (3 - 4);
    constant mod65 : integer := (6 - 4) mod (5 - 4);
    constant mod66 : integer := (6 - 4) mod (6 - 4);
    constant mod67 : integer := (6 - 4) mod (7 - 4);
    constant mod68 : integer := (6 - 4) mod (8 - 4);
    constant mod69 : integer := (6 - 4) mod (9 - 4);

    variable four : integer := 4;

  BEGIN
    assert mod11 = (1 - four) mod (1 - four);
    assert mod12 = (1 - four) mod (2 - four);
    assert mod13 = (1 - four) mod (3 - four);
    assert mod15 = (1 - four) mod (5 - four);
    assert mod16 = (1 - four) mod (6 - four);
    assert mod17 = (1 - four) mod (7 - four);
    assert mod18 = (1 - four) mod (8 - four);
    assert mod19 = (1 - four) mod (9 - four);
    assert mod41 = (4 - four) mod (1 - four);
    assert mod42 = (4 - four) mod (2 - four);
    assert mod43 = (4 - four) mod (3 - four);
    assert mod45 = (4 - four) mod (5 - four);
    assert mod46 = (4 - four) mod (6 - four);
    assert mod47 = (4 - four) mod (7 - four);
    assert mod48 = (4 - four) mod (8 - four);
    assert mod49 = (4 - four) mod (9 - four);
    assert mod61 = (6 - four) mod (1 - four);
    assert mod62 = (6 - four) mod (2 - four);
    assert mod63 = (6 - four) mod (3 - four);
    assert mod65 = (6 - four) mod (5 - four);
    assert mod66 = (6 - four) mod (6 - four);
    assert mod67 = (6 - four) mod (7 - four);
    assert mod68 = (6 - four) mod (8 - four);
    assert mod69 = (6 - four) mod (9 - four);

    assert NOT((mod11 = (1 - four) mod (1 - four))   and
               ( mod12 = (1 - four) mod (2 - four))   and
               ( mod13 = (1 - four) mod (3 - four))   and
               ( mod15 = (1 - four) mod (5 - four))   and
               ( mod16 = (1 - four) mod (6 - four))   and
               ( mod17 = (1 - four) mod (7 - four))   and
               ( mod18 = (1 - four) mod (8 - four))   and
               ( mod19 = (1 - four) mod (9 - four))   and
               ( mod41 = (4 - four) mod (1 - four))   and
               ( mod42 = (4 - four) mod (2 - four))   and
               ( mod43 = (4 - four) mod (3 - four))   and
               ( mod45 = (4 - four) mod (5 - four))   and
               ( mod46 = (4 - four) mod (6 - four))   and
               ( mod47 = (4 - four) mod (7 - four))   and
               ( mod48 = (4 - four) mod (8 - four))   and
               ( mod49 = (4 - four) mod (9 - four))   and
               ( mod61 = (6 - four) mod (1 - four))   and
               ( mod62 = (6 - four) mod (2 - four))   and
               ( mod63 = (6 - four) mod (3 - four))   and
               ( mod65 = (6 - four) mod (5 - four))   and
               ( mod66 = (6 - four) mod (6 - four))   and
               ( mod67 = (6 - four) mod (7 - four))   and
               ( mod68 = (6 - four) mod (8 - four))   and
               ( mod69 = (6 - four) mod (9 - four))   )
      report "***PASSED TEST: c07s02b06x00p05n01i02259"
      severity NOTE;
    assert (( mod11 = (1 - four) mod (1 - four))   and
            ( mod12 = (1 - four) mod (2 - four))   and
            ( mod13 = (1 - four) mod (3 - four))   and
            ( mod15 = (1 - four) mod (5 - four))   and
            ( mod16 = (1 - four) mod (6 - four))   and
            ( mod17 = (1 - four) mod (7 - four))   and
            ( mod18 = (1 - four) mod (8 - four))   and
            ( mod19 = (1 - four) mod (9 - four))   and
            ( mod41 = (4 - four) mod (1 - four))   and
            ( mod42 = (4 - four) mod (2 - four))   and
            ( mod43 = (4 - four) mod (3 - four))   and
            ( mod45 = (4 - four) mod (5 - four))   and
            ( mod46 = (4 - four) mod (6 - four))   and
            ( mod47 = (4 - four) mod (7 - four))   and
            ( mod48 = (4 - four) mod (8 - four))   and
            ( mod49 = (4 - four) mod (9 - four))   and
            ( mod61 = (6 - four) mod (1 - four))   and
            ( mod62 = (6 - four) mod (2 - four))   and
            ( mod63 = (6 - four) mod (3 - four))   and
            ( mod65 = (6 - four) mod (5 - four))   and
            ( mod66 = (6 - four) mod (6 - four))   and
            ( mod67 = (6 - four) mod (7 - four))   and
            ( mod68 = (6 - four) mod (8 - four))   and
            ( mod69 = (6 - four) mod (9 - four))   )
      report "***FAILED TEST: c07s02b06x00p05n01i02259 - Constant integer type mod test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b06x00p05n01i02259arch;
