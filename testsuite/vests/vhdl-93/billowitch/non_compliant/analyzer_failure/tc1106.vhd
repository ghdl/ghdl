
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
-- $Id: tc1106.vhd,v 1.2 2001-10-26 16:30:06 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s05b00x00p03n01i01106ent IS
END c06s05b00x00p03n01i01106ent;

ARCHITECTURE c06s05b00x00p03n01i01106arch OF c06s05b00x00p03n01i01106ent IS

BEGIN
  TESTING: PROCESS
    subtype FIVE    is INTEGER range 1 to 5;
    subtype THREE    is INTEGER range 1 to 3;
    subtype ONE    is INTEGER range 1 to 1;
    type A0       is array (INTEGER range <>) of BOOLEAN;
    subtype A1 is A0 (FIVE);
    subtype A2 is A0 (ONE);
    subtype A3 is A0 (THREE);
    subtype A5 is A0 (FIVE);
    variable V2: A2;
    variable V3: A3;
  BEGIN
    V2 := A5'(1=>TRUE, 2=>TRUE, 3=>TRUE, 4=>TRUE, 5=>TRUE) (3 to 3);
    -- SYNTAX ERROR: PREFIX OF SLICE NAME CANNOT BE AN AGGREGATE
    assert FALSE 
      report "***FAILED TEST: c06s05b00x00p03n01i01106 - Prefix of a slice name cannot be an aggregate." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s05b00x00p03n01i01106arch;
