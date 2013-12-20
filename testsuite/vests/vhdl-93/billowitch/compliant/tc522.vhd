
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
-- $Id: tc522.vhd,v 1.2 2001-10-26 16:29:56 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s03b00x00p03n01i00522ent IS
END c03s03b00x00p03n01i00522ent;

ARCHITECTURE c03s03b00x00p03n01i00522arch OF c03s03b00x00p03n01i00522ent IS

BEGIN
  TESTING: PROCESS
    -- Declare access types and access objects everywhere.
    -- Enumerated types.
    type    SWITCH_LEVEL is ('0', '1', 'X');
    type    AC1 is access SWITCH_LEVEL;

    subtype LOGIC_SWITCH is SWITCH_LEVEL range '0' to '1';
    type    AC2 is access LOGIC_SWITCH;

    -- Access types.
    type    AC3 is access AC2;

    -- array types.  Constrained.
    type WORD   is array(0 to 31) of BIT;
    type    AC4 is access WORD;

    -- record types.
    type DATE is
      record
        DAY           : INTEGER range 1 to 31;
        MONTH         : INTEGER range 1 to 12;
        YEAR          : INTEGER range -10000 to 1988;
      end record;
    type AC5 is access DATE;

    -- INTEGER types.
    type AC6 is access INTEGER;
    
    type POSITIVE        is range 0 to INTEGER'HIGH;
    type AC7 is access POSITIVE;
    
    -- Physical types.
    type AC8 is access TIME;
    
    type DISTANCE is range 0 to 1E9
      units
        -- Base units.
        A;                    -- angstrom
        
        -- Metric lengths.
        nm       = 10 A;      -- nanometer
        um       = 1000 nm;   -- micrometer (or micron)
        mm       = 1000 um;   -- millimeter
        cm       = 10 mm;     -- centimeter
        
        -- English lengths.
        mil      = 254000 A;  -- mil
        inch     = 1000 mil;  -- inch
      end units;
    type AC10 is access DISTANCE;
    
    -- floating point types.
    type AC11 is access REAL;
    
    type POSITIVE_R    is range 0.0 to REAL'HIGH;
    type AC12 is access POSITIVE_R;
    -- Predefined enumerated types.
    type AC13 is access BIT;
    
    type AC14 is access SEVERITY_LEVEL;
    
    type AC15 is access BOOLEAN;
    
    type AC16 is access CHARACTER;
    
    -- Other predefined types.
    type AC17 is access NATURAL;
    
    type AC18 is access STRING;
    
    type AC19 is access BIT_VECTOR;
    
    type MEMORY is array(0 to 64) of WORD;
    type AC20 is access MEMORY;
    
    -- Declare all the variables.
    variable VAR1 : AC1;
    variable VAR2 : AC2;
    variable VAR3 : AC3;
    variable VAR4 : AC4;
    variable VAR5 : AC5;
    variable VAR6 : AC6;
    variable VAR7 : AC7;
    variable VAR8 : AC8;
    variable VAR10: AC10;
    variable VAR11: AC11;
    variable VAR12: AC12;
    variable VAR13: AC13;
    variable VAR14: AC14;
    variable VAR15: AC15;
    variable VAR16: AC16;
    variable VAR17: AC17;
    variable VAR18: AC18;
    variable VAR19: AC19;
    variable VAR20: AC20;
  BEGIN
    -- Assert that all variables are initially NULL.
    assert (VAR1 = null)
      report "VAR1 has not been set to NULL.";
    assert (VAR2 = null)
      report "VAR2 has not been set to NULL.";
    assert (VAR3 = null)
      report "VAR3 has not been set to NULL.";
    assert (VAR4 = null)
      report "VAR4 has not been set to NULL.";
    assert (VAR5 = null)
      report "VAR5 has not been set to NULL.";
    assert (VAR6 = null)
      report "VAR6 has not been set to NULL.";
    assert (VAR7 = null)
      report "VAR7 has not been set to NULL.";
    assert (VAR8 = null)
      report "VAR8 has not been set to NULL.";
    assert (VAR10 = null)
      report "VAR10 has not been set to NULL.";
    assert (VAR11 = null)
      report "VAR11 has not been set to NULL.";
    assert (VAR12 = null)
      report "VAR12 has not been set to NULL.";
    assert (VAR13 = null)
      report "VAR13 has not been set to NULL.";
    assert (VAR14 = null)
      report "VAR14 has not been set to NULL.";
    assert (VAR15 = null)
      report "VAR15 has not been set to NULL.";
    assert (VAR16 = null)
      report "VAR16 has not been set to NULL.";
    assert (VAR17 = null)
      report "VAR17 has not been set to NULL.";
    assert (VAR18 = null)
      report "VAR18 has not been set to NULL.";
    assert (VAR19 = null)
      report "VAR19 has not been set to NULL.";
    assert (VAR20 = null)
      report "VAR20 has not been set to NULL.";
    assert   NOT(      (VAR1 = null)
                       and (VAR2 = null)
                       and (VAR3 = null)
                       and (VAR4 = null)
                       and (VAR5 = null)
                       and (VAR6 = null)
                       and (VAR7 = null)
                       and (VAR8 = null)
                       and (VAR10 = null)
                       and (VAR11 = null)
                       and (VAR12 = null)
                       and (VAR13 = null)
                       and (VAR14 = null)
                       and (VAR15 = null)
                       and (VAR16 = null)
                       and (VAR17 = null)
                       and (VAR18 = null)
                       and (VAR19 = null)
                       and (VAR20 = null))
      report "***PASSED TEST: c03s03b00x00p03n01i00522"
      severity NOTE;
    assert   (      (VAR1 = null)
                    and (VAR2 = null)
                    and (VAR3 = null)
                    and (VAR4 = null)
                    and (VAR5 = null)
                    and (VAR6 = null)
                    and (VAR7 = null)
                    and (VAR8 = null)
                    and (VAR10 = null)
                    and (VAR11 = null)
                    and (VAR12 = null)
                    and (VAR13 = null)
                    and (VAR14 = null)
                    and (VAR15 = null)
                    and (VAR16 = null)
                    and (VAR17 = null)
                    and (VAR18 = null)
                    and (VAR19 = null)
                    and (VAR20 = null))
      report "***FAILED TEST: c03s03b00x00p03n01i00522 - The null value of an access type is the default initial value of the type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s03b00x00p03n01i00522arch;
