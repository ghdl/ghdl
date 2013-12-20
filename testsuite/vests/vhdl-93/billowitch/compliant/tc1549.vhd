
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
-- $Id: tc1549.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s09b00x00p10n01i01549ent IS
END c08s09b00x00p10n01i01549ent;

ARCHITECTURE c08s09b00x00p10n01i01549arch OF c08s09b00x00p10n01i01549ent IS

BEGIN
  TESTING: PROCESS
    -- All different non-numeric type declarations.
    -- enumerated types.
    type    COLORS is (RED, GREEN, BLUE);
    -- local variables
    variable EXECUTED_ONCE : BOOLEAN;
    variable COUNT         : INTEGER;
    variable k : integer := 0;
  BEGIN
    -- 1. These for-loops should only execute one time.
    EXECUTED_ONCE := FALSE;
    for I in INTEGER'HIGH to INTEGER'HIGH loop
      if (EXECUTED_ONCE) then
        k := 1;
      end if;
      assert (not( EXECUTED_ONCE ))
        report "Failing in first loop.";
      EXECUTED_ONCE := TRUE;
    end loop;

    EXECUTED_ONCE := FALSE;
    for I in INTEGER'LOW to INTEGER'LOW loop
      if (EXECUTED_ONCE) then
        k := 1;
      end if;
      assert (not( EXECUTED_ONCE ))
        report "Failing in second loop.";
      EXECUTED_ONCE := TRUE;
    end loop;

    EXECUTED_ONCE := FALSE;
    for I in INTEGER'HIGH downto INTEGER'HIGH loop
      if (EXECUTED_ONCE) then
        k := 1;
      end if;
      assert (not( EXECUTED_ONCE ))
        report "Failing in third loop.";
      EXECUTED_ONCE := TRUE;
    end loop;
    
    EXECUTED_ONCE := FALSE;
    for I in INTEGER'LOW downto INTEGER'LOW loop
      if (EXECUTED_ONCE) then
        k := 1;
      end if;
      assert (not( EXECUTED_ONCE ))
        report "Failing in fourth loop.";
      EXECUTED_ONCE := TRUE;
    end loop;
    
    EXECUTED_ONCE := FALSE;
    for I in COLORS'HIGH to COLORS'HIGH loop
      if (EXECUTED_ONCE) then
        k := 1;
      end if;
      assert (not( EXECUTED_ONCE ))
        report "Failing in fifth loop.";
      EXECUTED_ONCE := TRUE;
    end loop;

    EXECUTED_ONCE := FALSE;
    for I in COLORS'LOW to COLORS'LOW loop
      if (EXECUTED_ONCE) then
        k := 1;
      end if;
      assert (not( EXECUTED_ONCE ))
        report "Failing in sixth loop.";
      EXECUTED_ONCE := TRUE;
    end loop;

    EXECUTED_ONCE := FALSE;
    for I in COLORS'HIGH downto COLORS'HIGH loop
      if (EXECUTED_ONCE) then
        k := 1;
      end if;
      assert (not( EXECUTED_ONCE ))
        report "Failing in seventh loop.";
      EXECUTED_ONCE := TRUE;
    end loop;
    
    EXECUTED_ONCE := FALSE;
    for I in COLORS'LOW downto COLORS'LOW loop
      if (EXECUTED_ONCE) then
        k := 1;
      end if;
      assert (not( EXECUTED_ONCE ))
        report "Failing in eighth loop.";
      EXECUTED_ONCE := TRUE;
    end loop;

    -- 2. These for-loops should be executed COUNT number of times.
    COUNT := 0;
    for I in 3 to 13 loop
      COUNT := COUNT + 1;
    end loop;
    if (count /= 11) then
      k := 1;
    end if;
    assert (COUNT = 11)
      report "Failing in 9th loop.";
    
    COUNT := 0;
    for I in 13 downto 3 loop
      COUNT := COUNT + 1;
    end loop;
    if (count /= 11) then
      k := 1;
    end if;
    assert (COUNT = 11)
      report "Failing in 10th loop.";
    
    COUNT := 0;
    for I in COLORS'LOW to COLORS'HIGH loop
      COUNT := COUNT + 1;
    end loop;
    if (count /= (COLORS'POS( COLORS'HIGH ) - COLORS'POS( COLORS'LOW ) + 1)) then
      k := 1;
    end if;
    assert (COUNT = (COLORS'POS( COLORS'HIGH ) - COLORS'POS( COLORS'LOW ) + 1))
      report "Failing in 11th loop.";
    
    COUNT := 0;
    for I in COLORS'HIGH downto COLORS'LOW loop
      COUNT := COUNT + 1;
    end loop;
    if (count /= (COLORS'POS( COLORS'HIGH ) - COLORS'POS( COLORS'LOW ) + 1)) then
      k := 1;
    end if;
    assert (COUNT = (COLORS'POS( COLORS'HIGH ) - COLORS'POS( COLORS'LOW ) + 1))
      report "Failing in 12th loop.";


    assert NOT( k=0 )
      report "***PASSED TEST: c08s09b00x00p10n01i01549"
      severity NOTE;
    assert ( k=0 )
      report "***FAILED TEST: c08s09b00x00p10n01i01549 - The sequence of statements is executed once for each value of the discrete range"
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s09b00x00p10n01i01549arch;
