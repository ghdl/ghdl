
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
-- $Id: tc546.vhd,v 1.2 2001-10-26 16:29:56 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s04b00x00p03n01i00546ent IS
END c03s04b00x00p03n01i00546ent;

ARCHITECTURE c03s04b00x00p03n01i00546arch OF c03s04b00x00p03n01i00546ent IS

BEGIN
  TESTING: PROCESS
    -- Enumerated types.
    type    SWITCH_LEVEL is ('0', '1', 'X');
    type    FT1 is file of SWITCH_LEVEL;
    subtype LOGIC_SWITCH is SWITCH_LEVEL range '0' to '1';
    type    FT2 is file of SWITCH_LEVEL;

    -- array types.  Unconstrained.
    type MEMORY is array(INTEGER range <>) of BIT;
    type    FT3 is file of MEMORY;

    -- array types.  Constrained.
    type WORD   is array(0 to 31) of BIT;
    type    FT4 is file of WORD;

    -- record types.
    type DATE is
      record
        DAY           : INTEGER range 1 to 31;
        MONTH         : INTEGER range 1 to 12;
        YEAR          : INTEGER range -10000 to 1988;
      end record;
    type FT5 is file of DATE;
    
    -- INTEGER types.
    type FT6 is file of INTEGER;
    type POSITIVE        is range 0 to INTEGER'HIGH;
    type FT7 is file of POSITIVE;
    
    -- Physical types.
    type FT8 is file of TIME;
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
    type FT10 is file of DISTANCE;
    
    -- floating point types.
    type FT11 is file of REAL;
    type POSITIVE_R    is range 0.0 to REAL'HIGH;
    type FT12 is file of POSITIVE_R;

    -- Predefined enumerated types.
    type FT13 is file of BIT;
    type FT14 is file of SEVERITY_LEVEL;
    type FT15 is file of BOOLEAN;
    type FT16 is file of CHARACTER;
    
    -- Other predefined types.
    type FT17 is file of NATURAL;
    type FT18 is file of STRING;
    type FT19 is file of BIT_VECTOR;

  BEGIN
    assert FALSE 
      report "***PASSED TEST: c03s04b00x00p03n01i00546"
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c03s04b00x00p03n01i00546arch;
