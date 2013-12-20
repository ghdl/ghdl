
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
-- $Id: tc2234.vhd,v 1.2 2001-10-26 16:30:16 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b06x00p01n01i02234ent IS
END c07s02b06x00p01n01i02234ent;

ARCHITECTURE c07s02b06x00p01n01i02234arch OF c07s02b06x00p01n01i02234ent IS

BEGIN
  TESTING: PROCESS
    -- user defined physical types.
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
    
    variable k    : integer;
  BEGIN
    k := 4 nm mod 1 A; 
    assert FALSE 
      report "***FAILED TEST: c07s02b06x00p01n01i02234 - Operators mod and rem are predefined for any integer type only." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b06x00p01n01i02234arch;
