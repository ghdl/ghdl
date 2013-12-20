
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
-- $Id: tc1129.vhd,v 1.2 2001-10-26 16:30:06 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s05b00x00p04n01i01129ent IS
END c06s05b00x00p04n01i01129ent;

ARCHITECTURE c06s05b00x00p04n01i01129arch OF c06s05b00x00p04n01i01129ent IS

BEGIN
  TESTING: PROCESS
    type ENUM1 is (M1, M2, M3, M4, M5);
    type ENUM2 is (N1, N2, N3, N4, N5);
    type FIVE1 is range 1 to 5;
    type FIVE2 is range 1 to 5;

    type    A1B is array (ENUM1 range <>) of BOOLEAN;
    subtype    A1  is A1B(ENUM1);
    type    A2B is array (ENUM2 range <>) of A1;

    variable V1: A1 ;
    constant FIVE2_2: FIVE2 := 2;
    constant FIVE2_4: FIVE2 := 4;
  BEGIN
    V1(M2 to M4) := V1(N1 to N5);
    -- SEMANTIC ERROR: DISCRETE RANGE INCOMPATIBLE WITH INDEX TYPE
    assert FALSE 
      report "***FAILED TEST: c06s05b00x00p04n01i01129 - Discrete range incompatible with index type." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s05b00x00p04n01i01129arch;
