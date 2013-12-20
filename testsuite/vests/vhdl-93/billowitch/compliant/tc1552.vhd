
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
-- $Id: tc1552.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s09b00x00p10n03i01552ent IS
END c08s09b00x00p10n03i01552ent;

ARCHITECTURE c08s09b00x00p10n03i01552arch OF c08s09b00x00p10n03i01552ent IS

BEGIN
  TESTING: PROCESS
    -- enumerated type.
    type    COLORS is (RED, GREEN, BLUE, ORANGE, PINK, GRAY, YELLOW);

    -- local variables
    variable EXECUTED_ONCE : BOOLEAN;
    variable LAST_INT      : INTEGER;
    variable LAST_COLOR    : COLORS;

    variable k : integer := 0;
  BEGIN
    -- 1. Test ascending and descending integer discrete ranges.
    EXECUTED_ONCE := FALSE;
    LAST_INT := INTEGER'LOW + 1;
    for I in (INTEGER'LOW+1) to (INTEGER'LOW + 10) loop
      -- Verify that the first value is correct.
      if  (not(EXECUTED_ONCE)) then
        if (I /= (integer'low + 1)) then
          k := 1;
        end if;
        assert (I = (INTEGER'LOW+1))
          report "First value is bad.";
        EXECUTED_ONCE := TRUE;
        
        -- Otherwise, test that this value is to the right of the previous one.
      else
        if (integer'succ(last_int) /= I) then
          k := 1;
        end if;
        assert (INTEGER'SUCC( LAST_INT ) = I)
          report "Subsequent values are bad.";
        LAST_INT := I;
      end if;
    end loop;
    
    EXECUTED_ONCE := FALSE;
    LAST_INT := INTEGER'HIGH - 1;
    for I in (INTEGER'HIGH-1) downto (INTEGER'HIGH - 10) loop
      -- Verify that the first value is correct.
      if  (not(EXECUTED_ONCE)) then
        if (I /= integer'high-1) then
          k := 1;
        end if;
        assert (I = (INTEGER'HIGH-1))
          report "First value, second loop, is bad.";
        EXECUTED_ONCE := TRUE;
        
        -- Otherwise, test that this value is to the right of the previous one.
      else
        if (integer'pred(last_int) /= I) then
          k := 1;
        end if;
        assert (INTEGER'PRED( LAST_INT ) = I)
          report "Subsequent values, second loop, are bad.";
        LAST_INT := I;
      end if;
    end loop;
    
    -- 2. Test ascending and descending enumerated type ranges.
    EXECUTED_ONCE := FALSE;
    LAST_COLOR := COLORS'SUCC( COLORS'LOW );
    for I in (COLORS'SUCC( COLORS'LOW )) to (COLORS'HIGH) loop
      -- Verify that the first value is correct.
      if  (not(EXECUTED_ONCE)) then
        if (I /= colors'succ(colors'low)) then
          k := 1;
        end if;
        assert (I = (COLORS'SUCC( COLORS'LOW )))
          report "First value, third loop, is bad.";
        EXECUTED_ONCE := TRUE;
        -- Otherwise, test that this value is to the right of the previous one.
      else
        if (colors'succ(last_color) /= I) then
          k := 1;
        end if;
        assert (COLORS'SUCC( LAST_COLOR ) = I)
          report "Subsequent values, third loop, are bad.";
        LAST_COLOR := I;
      end if;
    end loop;
    
    EXECUTED_ONCE := FALSE;
    LAST_COLOR := COLORS'PRED( COLORS'HIGH );
    for I in (COLORS'PRED( COLORS'HIGH )) downto (COLORS'LOW) loop
      -- Verify that the first value is correct.
      if  (not(EXECUTED_ONCE)) then
        if (I /= colors'pred(colors'high)) then
          k := 1;
        end if;
        assert (I = (COLORS'PRED( COLORS'HIGH )))
          report "First value, fourth loop, is bad.";
        EXECUTED_ONCE := TRUE;
        
        -- Otherwise, test that this value is to the right of the previous one.
      else
        if (colors'pred(last_color) /= I) then
          k := 1;
        end if;
        assert (COLORS'PRED( LAST_COLOR ) = I)
          report "Subsequent values, fourth loop, are bad.";
        LAST_COLOR := I;
      end if;
    end loop;

    assert NOT( k=0 )
      report "***PASSED TEST: c08s09b00x00p10n03i01552"
      severity NOTE;
    assert ( k=0 )
      report "***FAILED TEST: c08s09b00x00p10n03i01552 - Each iteration of a loop statement with a for iteration scheme, the corresponding value of the discrete range is assigned to the loop parameter, these values are assigned in left to rigth order"
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s09b00x00p10n03i01552arch;
