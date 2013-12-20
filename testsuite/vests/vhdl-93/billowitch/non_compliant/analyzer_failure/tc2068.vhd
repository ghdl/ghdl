
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
-- $Id: tc2068.vhd,v 1.2 2001-10-26 16:30:15 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b04x00p01n02i02068ent IS
END c07s02b04x00p01n02i02068ent;

ARCHITECTURE c07s02b04x00p01n02i02068arch OF c07s02b04x00p01n02i02068ent IS

BEGIN
  TESTING: PROCESS
    -- All different type declarations.
    -- integer types.
    type POSITIVE        is range 0 to INTEGER'HIGH;
    
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
--                             m        = 100 cm;    -- meter 
        -- English lengths.
        mil      = 254000 A;  -- mil
        inch     = 1000 mil;  -- inch
--                             ft       = 12 inch;   -- foot
--                             yd       = 3 ft;      -- yard
      end units;
    
    -- Local declarations.
    variable POSV    : POSITIVE     := 0;
    variable DISTV   : DISTANCE     := 1 A;
  BEGIN
    POSV := POSV + DISTV;   
    assert FALSE 
      report "***FAILED TEST: c07s02b04x00p01n02i02068 - The operands of the operators + and - cannot be of different types."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b04x00p01n02i02068arch;
