
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
-- $Id: tc18.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s02b00x00p09n01i00018ent IS
END c04s02b00x00p09n01i00018ent;

ARCHITECTURE c04s02b00x00p09n01i00018arch OF c04s02b00x00p09n01i00018ent IS
BEGIN
  TESTING: PROCESS
    -- Define a subtype.
    subtype DEC is INTEGER range 1 to 10;

    -- Define a subtype based on DEC.
    subtype DEC2 is DEC;

    -- Define two variable counters.
    variable CNT1, CNT2 : INTEGER := 0;
  BEGIN
    -- Verify that the range of DEC is the same as DEC2.
    for I in DEC loop
      CNT1 := CNT1 + 1;
    end loop;
    for I in DEC2 loop
      CNT2 := CNT2 + 1;
    end loop;
    assert NOT( CNT1 = CNT2 )
      report "***PASSED TEST: c04s02b00x00p09n01i00018"
      severity NOTE;
    assert ( CNT1 = CNT2 )
      report "***FAILED TEST: c04s02b00x00p09n01i00018 - If the subtype indication does not indicate a type constraint, the subtype is the same as that denoted by the type mark."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s02b00x00p09n01i00018arch;
