
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
-- $Id: tc947.vhd,v 1.2 2001-10-26 16:30:28 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s01b00x00p10n01i00947ent IS
END c06s01b00x00p10n01i00947ent;

ARCHITECTURE c06s01b00x00p10n01i00947arch OF c06s01b00x00p10n01i00947ent IS

BEGIN
  TESTING: PROCESS
    type R1 is record
                 RE1: BOOLEAN;
               end record;
    variable V1: BOOLEAN;
  BEGIN
    V1 := R1'(RE1=>TRUE).RE1;
    -- SYNTAX ERROR: PREFIX OF SELECTED NAME CANNOT BE AN AGGREGATE
    assert FALSE 
      report "***FAILED TEST: c06s01b00x00p10n01i00947 - Prefix of a selected name cannot be an aggregate."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s01b00x00p10n01i00947arch;
