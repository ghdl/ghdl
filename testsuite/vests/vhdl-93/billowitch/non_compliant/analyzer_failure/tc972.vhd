
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
-- $Id: tc972.vhd,v 1.2 2001-10-26 16:30:29 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s03b00x00p05n01i00972ent IS
END c06s03b00x00p05n01i00972ent;

ARCHITECTURE c06s03b00x00p05n01i00972arch OF c06s03b00x00p05n01i00972ent IS

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
    variable V2 : R1;
  BEGIN
    V2.RE1 := RE1;
    V2.RE2 := RE2;
    V2.RE3 := RE3;
    V2.RE4 := RE4;
    V2.RE5 := RE5;
    V2.RE6 := RE6;
    V2.RE7 := RE7;
    -- ERROR: RECORD ELEMENT NAME CANNOT BE USED BY ITSELF
    assert FALSE 
      report "***FAILED TEST: c06s03b00x00p05n01i00972 - Record element name cannot be used by itself." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s03b00x00p05n01i00972arch;
