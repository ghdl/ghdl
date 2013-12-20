
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
-- $Id: tc949.vhd,v 1.2 2001-10-26 16:30:28 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s01b00x00p10n01i00949ent IS
END c06s01b00x00p10n01i00949ent;

ARCHITECTURE c06s01b00x00p10n01i00949arch OF c06s01b00x00p10n01i00949ent IS

BEGIN
  TESTING: PROCESS
    type A1 is array (1 to 2) of BOOLEAN;
    type R3 is record
                 RE3: A1;
               end record;
    variable V1: BOOLEAN;
  BEGIN
    V1 := (RE3=>(1=>TRUE,2=>TRUE)).RE3(1);
    -- SYNTAX ERROR: PREFIX OF SELECTED NAME CANNOT BE AN AGGREGATE
    assert FALSE 
      report "***FAILED TEST: c06s01b00x00p10n01i00949 - Prefix of a selected name cannot be an aggregate."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s01b00x00p10n01i00949arch;
