
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
-- $Id: tc952.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s01b00x00p10n01i00952ent IS
END c06s01b00x00p10n01i00952ent;

ARCHITECTURE c06s01b00x00p10n01i00952arch OF c06s01b00x00p10n01i00952ent IS

BEGIN
  TESTING: PROCESS
    type TWO is range 1 to 2;
    type R1 is record
                 X1: TWO;
                 RE1: BOOLEAN;
               end record;
    type A1  is array (TWO) of R1;
    type A2  is array (TWO) of A1;
    variable V1: BOOLEAN;
    variable V3: A2 ;
  BEGIN
    V1 := V3(2)(1).RE1;
    assert NOT(V1 = false) 
      report "***PASSED TEST: c06s01b00x00p10n01i00952" 
      severity NOTE;
    assert (V1 = false) 
      report "***FAILED TEST: c06s01b00x00p10n01i00952 - Prefix of a selected name cannot be an aggregate."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s01b00x00p10n01i00952arch;
