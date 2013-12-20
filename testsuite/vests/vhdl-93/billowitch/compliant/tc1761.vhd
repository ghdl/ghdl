
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
-- $Id: tc1761.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s05b01x00p21n01i01761ent IS
END c09s05b01x00p21n01i01761ent;

ARCHITECTURE c09s05b01x00p21n01i01761arch OF c09s05b01x00p21n01i01761ent IS
  signal TS1,TS2   : integer;
  signal B,C   : integer;
  signal D,E,F   : bit;
BEGIN
  TS1 <= transport 1 after 10 ns when B = C else
         2 after 10 ns when B > C else
         3 after 10 ns;

  TS2 <= transport 4-1 after 10 ns when D = '1' else
         5+1 after 10 ns when E = '1' else
         6*2 after 10 ns when F = '1' else
         8/2 after 10 ns;

  TESTING: PROCESS(TS1,TS2)
  BEGIN
    if ( now > 1 ns) then
      assert NOT(TS1=1 and TS2=4) 
        report "***PASSED TEST: c09s05b01x00p21n01i01761" 
        severity NOTE;
      assert (TS1=1 and TS2=4) 
        report "***FAILED TEST: c09s05b01x00p21n01i01761 - Conditions in the conditional signal assignment statement should be valid." 
        severity ERROR;
    end if;
  END PROCESS TESTING;

END c09s05b01x00p21n01i01761arch;
