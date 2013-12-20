
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
-- $Id: tc2702.vhd,v 1.2 2001-10-26 16:29:49 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c13s04b01x00p05n02i02702ent IS
END c13s04b01x00p05n02i02702ent;

ARCHITECTURE c13s04b01x00p05n02i02702arch OF c13s04b01x00p05n02i02702ent IS
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT(      (1e2 = 1E2)
                     and    (1.2e1 = 1.2E1)
                     and    (1.2e-1 = 1.2E-1)
                     and    (16#F#e1 = 16#F#E1)
                     and    (16#F.F#e1 = 16#F.F#E1))
      report "***PASSED TEST: c13s04b01x00p05n02i02702"
      severity NOTE;
    assert (      (1e2 = 1E2)
                  and    (1.2e1 = 1.2E1)
                  and    (1.2e-1 = 1.2E-1)
                  and    (16#F#e1 = 16#F#E1)
                  and    (16#F.F#e1 = 16#F.F#E1))
      report "***FAILED TEST: c13s04b01x00p05n02i02702 - Upper case and lower case E that used to indicate exponent in both integer and real literals test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c13s04b01x00p05n02i02702arch;
