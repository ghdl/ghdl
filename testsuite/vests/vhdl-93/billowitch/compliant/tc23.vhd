
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
-- $Id: tc23.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s02b00x00p09n03i00023ent IS
END c04s02b00x00p09n03i00023ent;

ARCHITECTURE c04s02b00x00p09n03i00023arch OF c04s02b00x00p09n03i00023ent IS
  type    T1 is     range 0  to 100;
  subtype T2 is T1 range 20 to 80 ;
  subtype T3 is T2 range 40 to 60 ;  -- No_failure_here
  subtype T4 is T3 range 50 to 50 ;
BEGIN
  TESTING: PROCESS
    variable k : T4 := 50;
  BEGIN
    assert NOT(k=50) 
      report "***PASSED TEST: c04s02b00x00p09n03i00023"
      severity NOTE;
    assert (k=50) 
      report "***FAILED TEST: c04s02b00x00p09n03i00023 - Range constraints for the subtype declarations contradict the range of the subtype indication."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s02b00x00p09n03i00023arch;
