
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
-- $Id: tc19.vhd,v 1.2 2001-10-26 16:30:14 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s02b00x00p09n03i00019ent IS
END c04s02b00x00p09n03i00019ent;

ARCHITECTURE c04s02b00x00p09n03i00019arch OF c04s02b00x00p09n03i00019ent IS
  type A1 is range 50 to 100;
  subtype B1 is A1 range 1 to 60;             -- Failure_here
  subtype C1 is B1 range 2 to 50;
BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST:c04s02b00x00p09n03i00019 - Range constraints for the subtype declarations contradict the range of the subtype indication.(integer)"
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s02b00x00p09n03i00019arch;
