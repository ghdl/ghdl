
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
-- $Id: tc291.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s01b03x00p14n01i00291ent IS
END c03s01b03x00p14n01i00291ent;

ARCHITECTURE c03s01b03x00p14n01i00291arch OF c03s01b03x00p14n01i00291ent IS
  type T is
    range -2147483647 to 2147483647   -- No_failure_here
    units
      I ;  
      J = 2 I;
      K = 2 J;
      L = 10 K;
      M = 1000 L;
    end units;
BEGIN
  TESTING: PROCESS
    variable kk : T := 1 L;
  BEGIN
    assert NOT( kk = 20 J )
      report "***PASSED TEST: c03s01b03x00p14n01i00291"
      severity NOTE;
    assert ( kk = 20 J )
      report "***FAILED TEST: c03s01b03x00p14n01i00291 - The declaration of any physical type whose range is wholly contained within the bounds -2147483647 and +2147483647, inclusive."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s01b03x00p14n01i00291arch;
