
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
-- $Id: tc1142.vhd,v 1.2 2001-10-26 16:30:06 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s05b00x00p05n02i01142ent IS
END c06s05b00x00p05n02i01142ent;

ARCHITECTURE c06s05b00x00p05n02i01142arch OF c06s05b00x00p05n02i01142ent IS

BEGIN
  TESTING: PROCESS
    type ENUM1 is (M1, M2, M3, M4, M5, M6);
    type A     is array ( ENUM1 range <> ) of BOOLEAN;
    subtype A1 is A(M1 to M6) ;
    subtype A2 is A(M6 downto M1) ;
    variable V1: A1 ;
    variable V2: A2 ;
  BEGIN
    V2(M4 downto M2) := V2(M2 to M4);
    --ERROR: discrete range ascending when the prefix     
    --type was declared with a descending range results in a null
    --slice, which is incompatible with non-null slice
    assert FALSE 
      report "***FAILED TEST: c06s05b00x00p05n02i01142 - Null slice is not compatible with non-null slice." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s05b00x00p05n02i01142arch;
