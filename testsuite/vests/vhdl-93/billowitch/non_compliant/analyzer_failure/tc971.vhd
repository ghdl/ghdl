
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
-- $Id: tc971.vhd,v 1.2 2001-10-26 16:30:29 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s03b00x00p05n01i00971ent IS
END c06s03b00x00p05n01i00971ent;

ARCHITECTURE c06s03b00x00p05n01i00971arch OF c06s03b00x00p05n01i00971ent IS

BEGIN
  TESTING: PROCESS
    type R1 is record
                 RE1: BOOLEAN;
                 RE2: INTEGER;
                 RE3: BIT;
                 RE4: SEVERITY_LEVEL;
                 RE5: REAL;
                 RE6: CHARACTER;
                 RE7: TIME; 
               end record;
    variable V1: BOOLEAN;
    variable V2: INTEGER;
    variable V3: BIT;
    variable V4: SEVERITY_LEVEL;
    variable V5: REAL;
    variable V6: CHARACTER;
    variable V7: TIME;
  BEGIN
    V1 := RE1;
    V2 := RE2;
    V3 := RE3;
    V4 := RE4;
    V5 := RE5;
    V6 := RE6;
    V7 := RE7;
    -- ERROR: RECORD ELEMENT NAME CANNOT BE USED BY ITSELF
    assert FALSE 
      report "***FAILED TEST: c06s03b00x00p05n01i00971 - Record element name cannot be used by itself as an expression." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s03b00x00p05n01i00971arch;
