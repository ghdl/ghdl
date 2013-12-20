
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
-- $Id: tc2257.vhd,v 1.2 2001-10-26 16:29:46 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b06x00p05n01i02257ent IS
END c07s02b06x00p05n01i02257ent;

ARCHITECTURE c07s02b06x00p05n01i02257arch OF c07s02b06x00p05n01i02257ent IS

BEGIN
  TESTING: PROCESS
    constant div11 : integer := (1 - 4) / (1 - 4);
    constant div12 : integer := (1 - 4) / (2 - 4);
    constant div13 : integer := (1 - 4) / (3 - 4);
    constant div15 : integer := (1 - 4) / (5 - 4);
    constant div16 : integer := (1 - 4) / (6 - 4);
    constant div17 : integer := (1 - 4) / (7 - 4);
    constant div18 : integer := (1 - 4) / (8 - 4);
    constant div19 : integer := (1 - 4) / (9 - 4);
    constant div41 : integer := (4 - 4) / (1 - 4);
    constant div42 : integer := (4 - 4) / (2 - 4);
    constant div43 : integer := (4 - 4) / (3 - 4);
    constant div45 : integer := (4 - 4) / (5 - 4);
    constant div46 : integer := (4 - 4) / (6 - 4);
    constant div47 : integer := (4 - 4) / (7 - 4);
    constant div48 : integer := (4 - 4) / (8 - 4);
    constant div49 : integer := (4 - 4) / (9 - 4);
    constant div61 : integer := (6 - 4) / (1 - 4);
    constant div62 : integer := (6 - 4) / (2 - 4);
    constant div63 : integer := (6 - 4) / (3 - 4);
    constant div65 : integer := (6 - 4) / (5 - 4);
    constant div66 : integer := (6 - 4) / (6 - 4);
    constant div67 : integer := (6 - 4) / (7 - 4);
    constant div68 : integer := (6 - 4) / (8 - 4);
    constant div69 : integer := (6 - 4) / (9 - 4);

    variable four : integer := 4;

  BEGIN

    assert div11 = (1 - four) / (1 - four);
    assert div12 = (1 - four) / (2 - four);
    assert div13 = (1 - four) / (3 - four);
    assert div15 = (1 - four) / (5 - four);
    assert div16 = (1 - four) / (6 - four);
    assert div17 = (1 - four) / (7 - four);
    assert div18 = (1 - four) / (8 - four);
    assert div19 = (1 - four) / (9 - four);
    assert div41 = (4 - four) / (1 - four);
    assert div42 = (4 - four) / (2 - four);
    assert div43 = (4 - four) / (3 - four);
    assert div45 = (4 - four) / (5 - four);
    assert div46 = (4 - four) / (6 - four);
    assert div47 = (4 - four) / (7 - four);
    assert div48 = (4 - four) / (8 - four);
    assert div49 = (4 - four) / (9 - four);
    assert div61 = (6 - four) / (1 - four);
    assert div62 = (6 - four) / (2 - four);
    assert div63 = (6 - four) / (3 - four);
    assert div65 = (6 - four) / (5 - four);
    assert div66 = (6 - four) / (6 - four);
    assert div67 = (6 - four) / (7 - four);
    assert div68 = (6 - four) / (8 - four);
    assert div69 = (6 - four) / (9 - four);

    assert NOT((div11 = (1 - four) / (1 - four))   and
               ( div12 = (1 - four) / (2 - four))   and
               ( div13 = (1 - four) / (3 - four))   and
               ( div15 = (1 - four) / (5 - four))   and
               ( div16 = (1 - four) / (6 - four))   and
               ( div17 = (1 - four) / (7 - four))   and
               ( div18 = (1 - four) / (8 - four))   and
               ( div19 = (1 - four) / (9 - four))   and
               ( div41 = (4 - four) / (1 - four))   and
               ( div42 = (4 - four) / (2 - four))   and
               ( div43 = (4 - four) / (3 - four))   and
               ( div45 = (4 - four) / (5 - four))   and
               ( div46 = (4 - four) / (6 - four))   and
               ( div47 = (4 - four) / (7 - four))   and
               ( div48 = (4 - four) / (8 - four))   and
               ( div49 = (4 - four) / (9 - four))   and
               ( div61 = (6 - four) / (1 - four))   and
               ( div62 = (6 - four) / (2 - four))   and
               ( div63 = (6 - four) / (3 - four))   and
               ( div65 = (6 - four) / (5 - four))   and
               ( div66 = (6 - four) / (6 - four))   and
               ( div67 = (6 - four) / (7 - four))   and
               ( div68 = (6 - four) / (8 - four))   and
               ( div69 = (6 - four) / (9 - four))   )
      report "***PASSED TEST: c07s02b06x00p05n01i02257"
      severity NOTE;
    assert (( div11 = (1 - four) / (1 - four))   and
            ( div12 = (1 - four) / (2 - four))   and
            ( div13 = (1 - four) / (3 - four))   and
            ( div15 = (1 - four) / (5 - four))   and
            ( div16 = (1 - four) / (6 - four))   and
            ( div17 = (1 - four) / (7 - four))   and
            ( div18 = (1 - four) / (8 - four))   and
            ( div19 = (1 - four) / (9 - four))   and
            ( div41 = (4 - four) / (1 - four))   and
            ( div42 = (4 - four) / (2 - four))   and
            ( div43 = (4 - four) / (3 - four))   and
            ( div45 = (4 - four) / (5 - four))   and
            ( div46 = (4 - four) / (6 - four))   and
            ( div47 = (4 - four) / (7 - four))   and
            ( div48 = (4 - four) / (8 - four))   and
            ( div49 = (4 - four) / (9 - four))   and
            ( div61 = (6 - four) / (1 - four))   and
            ( div62 = (6 - four) / (2 - four))   and
            ( div63 = (6 - four) / (3 - four))   and
            ( div65 = (6 - four) / (5 - four))   and
            ( div66 = (6 - four) / (6 - four))   and
            ( div67 = (6 - four) / (7 - four))   and
            ( div68 = (6 - four) / (8 - four))   and
            ( div69 = (6 - four) / (9 - four))   )
      report "***FAILED TEST: c07s02b06x00p05n01i02257 - Constant integer type division test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b06x00p05n01i02257arch;
