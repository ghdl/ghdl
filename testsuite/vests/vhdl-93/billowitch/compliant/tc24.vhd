
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
-- $Id: tc24.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s02b00x00p10n01i00024ent IS
END c04s02b00x00p10n01i00024ent;

ARCHITECTURE c04s02b00x00p10n01i00024arch OF c04s02b00x00p10n01i00024ent IS

BEGIN
  TESTING: PROCESS

    -- Define an ascending subtype.
    subtype ASC is INTEGER range 0 to 1;

    -- Define a descending subtype.
    subtype DES is INTEGER range 1 to 0;

    -- Define a 'previous value' variable.
    variable PREV : INTEGER;

    variable k : integer := 0;
    variable l : integer := 0;
  BEGIN
    -- Test the direction of the ascending range.
    PREV := -1;
    for I in ASC loop
      if (I > PREV) then
        PREV := I;
      else
        k := 1;
      end if;
    end loop;

    -- Test the direction of the descending range.
    PREV := 2;
    for I in DES loop
      if (I < PREV) then
        PREV := I;
      else
        l := 1;
      end if;
    end loop;
    assert NOT( k=0 and l=0 )
      report "***PASSED TEST:c04s02b00x00p10n01i00024"
      severity NOTE;
    assert ( k=0 and l=0 )
      report "***FAILED TEST: c04s02b00x00p10n01i00024 - The direction of a discrete subtype indication is the same as the direction of the range constraint that appears as the constraint of the subtype indication."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s02b00x00p10n01i00024arch;
