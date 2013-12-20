
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
-- $Id: tc2292.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b06x00p32n01i02292ent IS
END c07s02b06x00p32n01i02292ent;

ARCHITECTURE c07s02b06x00p32n01i02292arch OF c07s02b06x00p32n01i02292ent IS

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
  BEGIN
    wait for 5 ns;
    assert ((1 A  * 10.0) > 1 A)
      report "Assertion error.(1)";
    assert ((1 nm * 1000.0) > 1 nm)
      report "Assertion error.(2)";
    assert ((1 um * 1000.0) > 1 um)
      report "Assertion error.(3)";
    assert ((1 mm * 10.0) > 1 mm)
      report "Assertion error.(4)";
    assert ((10.0  * 1 A) > 1 A)
      report "Assertion error.(6)";
    assert ((1000.0 * 1 nm) > 1 nm)
      report "Assertion error.(7)";
    assert ((1000.0 * 1 um) > 1 um)
      report "Assertion error.(8)";
    assert ((10.0 * 1 mm) > 1 mm)
      report "Assertion error.(9)";
    assert ((1 A * 254000.0) > 1 A)
      report "Assertion error.(16)";
    assert ((1 mil * 1000.0) > 1 mil)
      report "Assertion error.(17)";
    assert ((254000.0 * 1 A) > 1 A)
      report "Assertion error.(20)";
    assert ((1000.0 * 1 mil) > 1 mil)
      report "Assertion error.(21)";
    assert NOT(    ((1 A  * 10.0) > 1 A)   and
                   ((1 nm * 1000.0) > 1 nm)and
                   ((1 um * 1000.0) > 1 um)and
                   ((1 mm * 10.0) > 1 mm)   and
                   ((10.0  * 1 A) > 1 A)   and
                   ((1000.0 * 1 nm) > 1 nm)and
                   ((1000.0 * 1 um) > 1 um)and
                   ((10.0 * 1 mm) > 1 mm)   and
                   ((1 A * 254000.0) > 1 A)   and
                   ((1 mil * 1000.0) > 1 mil)   and
                   ((254000.0 * 1 A) > 1 A)   and
                   ((1000.0 * 1 mil) > 1 mil)   )
      report "***PASSED TEST: c07s02b06x00p32n01i02292"
      severity NOTE;
    assert (    ((1 A  * 10.0) > 1 A)   and
                ((1 nm * 1000.0) > 1 nm)and
                ((1 um * 1000.0) > 1 um)and
                ((1 mm * 10.0) > 1 mm)   and
                ((10.0  * 1 A) > 1 A)   and
                ((1000.0 * 1 nm) > 1 nm)and
                ((1000.0 * 1 um) > 1 um)and
                ((10.0 * 1 mm) > 1 mm)   and
                ((1 A * 254000.0) > 1 A)   and
                ((1 mil * 1000.0) > 1 mil)   and
                ((254000.0 * 1 A) > 1 A)   and
                ((1000.0 * 1 mil) > 1 mil)   )
      report "***FAILED TEST: c07s02b06x00p32n01i02292 - Multiplication of a physical type by an floating point test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b06x00p32n01i02292arch;
