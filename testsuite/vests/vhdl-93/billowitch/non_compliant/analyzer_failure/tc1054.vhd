
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
-- $Id: tc1054.vhd,v 1.2 2001-10-26 16:30:05 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s04b00x00p03n02i01054ent IS
END c06s04b00x00p03n02i01054ent;

ARCHITECTURE c06s04b00x00p03n02i01054arch OF c06s04b00x00p03n02i01054ent IS

BEGIN
  TESTING: PROCESS
    type ENUM1 is (EN1, EN2, EN3);

    type A12 is array (ENUM1) of BOOLEAN;
    
    variable V1: BOOLEAN;
    variable V12: A12 ;
  BEGIN
    V1 := V12(EN3, EN2);             -- ONE MORE
    -- SEMANTIC ERROR: ACTUAL INDEX POSITIONS DO NOT CORRESPOND TO
    -- INDEX POSITIONS IN TYPE DECLARATION
    assert FALSE 
      report "***FAILED TEST: c06s04b00x00p03n02i01054 - The expresion should be the same type as the corresponding index." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s04b00x00p03n02i01054arch;
