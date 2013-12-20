
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
-- $Id: tc2260.vhd,v 1.2 2001-10-26 16:29:46 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b06x00p05n01i02260ent IS
END c07s02b06x00p05n01i02260ent;

ARCHITECTURE c07s02b06x00p05n01i02260arch OF c07s02b06x00p05n01i02260ent IS

BEGIN
  TESTING: PROCESS
    constant rem11 : integer := (1 - 4) rem (1 - 4);
    constant rem12 : integer := (1 - 4) rem (2 - 4);
    constant rem13 : integer := (1 - 4) rem (3 - 4);
    constant rem15 : integer := (1 - 4) rem (5 - 4);
    constant rem16 : integer := (1 - 4) rem (6 - 4);
    constant rem17 : integer := (1 - 4) rem (7 - 4);
    constant rem18 : integer := (1 - 4) rem (8 - 4);
    constant rem19 : integer := (1 - 4) rem (9 - 4);
    constant rem41 : integer := (4 - 4) rem (1 - 4);
    constant rem42 : integer := (4 - 4) rem (2 - 4);
    constant rem43 : integer := (4 - 4) rem (3 - 4);
    constant rem45 : integer := (4 - 4) rem (5 - 4);
    constant rem46 : integer := (4 - 4) rem (6 - 4);
    constant rem47 : integer := (4 - 4) rem (7 - 4);
    constant rem48 : integer := (4 - 4) rem (8 - 4);
    constant rem49 : integer := (4 - 4) rem (9 - 4);
    constant rem61 : integer := (6 - 4) rem (1 - 4);
    constant rem62 : integer := (6 - 4) rem (2 - 4);
    constant rem63 : integer := (6 - 4) rem (3 - 4);
    constant rem65 : integer := (6 - 4) rem (5 - 4);
    constant rem66 : integer := (6 - 4) rem (6 - 4);
    constant rem67 : integer := (6 - 4) rem (7 - 4);
    constant rem68 : integer := (6 - 4) rem (8 - 4);
    constant rem69 : integer := (6 - 4) rem (9 - 4);

    variable four : integer := 4;

  BEGIN
    assert rem11 = (1 - four) rem (1 - four);
    assert rem12 = (1 - four) rem (2 - four);
    assert rem13 = (1 - four) rem (3 - four);
    assert rem15 = (1 - four) rem (5 - four);
    assert rem16 = (1 - four) rem (6 - four);
    assert rem17 = (1 - four) rem (7 - four);
    assert rem18 = (1 - four) rem (8 - four);
    assert rem19 = (1 - four) rem (9 - four);
    assert rem41 = (4 - four) rem (1 - four);
    assert rem42 = (4 - four) rem (2 - four);
    assert rem43 = (4 - four) rem (3 - four);
    assert rem45 = (4 - four) rem (5 - four);
    assert rem46 = (4 - four) rem (6 - four);
    assert rem47 = (4 - four) rem (7 - four);
    assert rem48 = (4 - four) rem (8 - four);
    assert rem49 = (4 - four) rem (9 - four);
    assert rem61 = (6 - four) rem (1 - four);
    assert rem62 = (6 - four) rem (2 - four);
    assert rem63 = (6 - four) rem (3 - four);
    assert rem65 = (6 - four) rem (5 - four);
    assert rem66 = (6 - four) rem (6 - four);
    assert rem67 = (6 - four) rem (7 - four);
    assert rem68 = (6 - four) rem (8 - four);
    assert rem69 = (6 - four) rem (9 - four);

    assert NOT((rem11 = (1 - four) rem (1 - four))   and
               ( rem12 = (1 - four) rem (2 - four))   and
               ( rem13 = (1 - four) rem (3 - four))   and
               ( rem15 = (1 - four) rem (5 - four))   and
               ( rem16 = (1 - four) rem (6 - four))   and
               ( rem17 = (1 - four) rem (7 - four))   and
               ( rem18 = (1 - four) rem (8 - four))   and
               ( rem19 = (1 - four) rem (9 - four))   and
               ( rem41 = (4 - four) rem (1 - four))   and
               ( rem42 = (4 - four) rem (2 - four))   and
               ( rem43 = (4 - four) rem (3 - four))   and
               ( rem45 = (4 - four) rem (5 - four))   and
               ( rem46 = (4 - four) rem (6 - four))   and
               ( rem47 = (4 - four) rem (7 - four))   and
               ( rem48 = (4 - four) rem (8 - four))   and
               ( rem49 = (4 - four) rem (9 - four))   and
               ( rem61 = (6 - four) rem (1 - four))   and
               ( rem62 = (6 - four) rem (2 - four))   and
               ( rem63 = (6 - four) rem (3 - four))   and
               ( rem65 = (6 - four) rem (5 - four))   and
               ( rem66 = (6 - four) rem (6 - four))   and
               ( rem67 = (6 - four) rem (7 - four))   and
               ( rem68 = (6 - four) rem (8 - four))   and
               ( rem69 = (6 - four) rem (9 - four))   )
      report "***PASSED TEST: c07s02b06x00p05n01i02260"
      severity NOTE;
    assert (( rem11 = (1 - four) rem (1 - four))   and
            ( rem12 = (1 - four) rem (2 - four))   and
            ( rem13 = (1 - four) rem (3 - four))   and
            ( rem15 = (1 - four) rem (5 - four))   and
            ( rem16 = (1 - four) rem (6 - four))   and
            ( rem17 = (1 - four) rem (7 - four))   and
            ( rem18 = (1 - four) rem (8 - four))   and
            ( rem19 = (1 - four) rem (9 - four))   and
            ( rem41 = (4 - four) rem (1 - four))   and
            ( rem42 = (4 - four) rem (2 - four))   and
            ( rem43 = (4 - four) rem (3 - four))   and
            ( rem45 = (4 - four) rem (5 - four))   and
            ( rem46 = (4 - four) rem (6 - four))   and
            ( rem47 = (4 - four) rem (7 - four))   and
            ( rem48 = (4 - four) rem (8 - four))   and
            ( rem49 = (4 - four) rem (9 - four))   and
            ( rem61 = (6 - four) rem (1 - four))   and
            ( rem62 = (6 - four) rem (2 - four))   and
            ( rem63 = (6 - four) rem (3 - four))   and
            ( rem65 = (6 - four) rem (5 - four))   and
            ( rem66 = (6 - four) rem (6 - four))   and
            ( rem67 = (6 - four) rem (7 - four))   and
            ( rem68 = (6 - four) rem (8 - four))   and
            ( rem69 = (6 - four) rem (9 - four))   )
      report "***FAILED TEST: c07s02b06x00p05n01i02260 - Constant integer type rem test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b06x00p05n01i02260arch;
