
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
-- $Id: tc27.vhd,v 1.2 2001-10-26 16:29:49 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s02b00x00p10n04i00027ent IS
END c04s02b00x00p10n04i00027ent;

ARCHITECTURE c04s02b00x00p10n04i00027arch OF c04s02b00x00p10n04i00027ent IS

  subtype s1 is integer range 1  to 10;      -- No_failure_here
  subtype s2 is integer range 10 downto 1;   -- No_failure_here

  -- the following are null ranges
  subtype s3 is integer range 1  downto 10;  -- No_failure_here
  subtype s4 is integer range 10 to 1;       -- No_failure_here
  
BEGIN
  TESTING: PROCESS
    variable k1 : s1 := 1;
    variable k2 : s2 := 10;
    variable k  : integer := 0;
  BEGIN
    for i in s1 loop
      if (i /= k1) then
        k := 1;
      end if;
      if (k1 < 10) then
        k1 := k1 + 1;
      end if;
    end loop;
    for i in s2 loop
      if (i /= k2) then
        k := 1;
      end if;
      if (k2 > 1) then
        k2 := k2 - 1;
      end if;
    end loop;
    assert NOT( k=0 )
      report "***PASSED TEST: c04s02b00x00p10n04i00027"
      severity NOTE;
    assert ( k=0 )
      report "***FAILED TEST: c04s02b00x00p10n04i00027 - The direction of a discrete subtype is the same as the direction of its subtype indication."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s02b00x00p10n04i00027arch;
