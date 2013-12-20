
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
-- $Id: tc3009.vhd,v 1.2 2001-10-26 16:30:24 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c02s06b00x00p07n01i03009pkg is
  subtype    S1 is Integer;
  subtype    S2 is Integer;
  constant    C  :  S1;
end c02s06b00x00p07n01i03009pkg;

package body c02s06b00x00p07n01i03009pkg is
  constant C : S2 := 0;  --Failure_here
end c02s06b00x00p07n01i03009pkg;

ENTITY c02s06b00x00p07n01i03009ent IS
END c02s06b00x00p07n01i03009ent;

ARCHITECTURE c02s06b00x00p07n01i03009arch OF c02s06b00x00p07n01i03009ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE
      report "***FAILED TEST: c02s06b00x00p07n01i03009 - The subtype of deferred constant does not conform to that given in the deferred constant declaration."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s06b00x00p07n01i03009arch;
