
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
-- $Id: tc1133.vhd,v 1.2 2001-10-26 16:30:06 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s05b00x00p04n01i01133ent IS
END c06s05b00x00p04n01i01133ent;

ARCHITECTURE c06s05b00x00p04n01i01133arch OF c06s05b00x00p04n01i01133ent IS

BEGIN
  TESTING: PROCESS
    type ENUM1 is (M1, M2, M3, M4, M5);
    type ENUM2 is (N1, N2, N3, N4, N5);
    type FIVE1 is range 1 to 5;
    type FIVE2 is range 1 to 5;

    type    A3B is array (FIVE1 range <>) of BOOLEAN;
    subtype    A3  is A3B(FIVE1);

    variable V3: A3 ;

    constant FIVE2_2: FIVE2 := 2;
    constant FIVE2_4: FIVE2 := 4;
  BEGIN
    V3(2 to 4) := V3(FIVE2_2 to FIVE2_4);
    -- SEMANTIC ERROR: DISCRETE RANGE INCOMPATIBLE WITH INDEX TYPE
    assert FALSE 
      report "***FAILED TEST: c06s05b00x00p04n01i01133 - Discrete range incompatible with index type." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s05b00x00p04n01i01133arch;
