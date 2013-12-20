
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
-- $Id: tc2324.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b07x00p01n01i02324ent IS
END c07s02b07x00p01n01i02324ent;

ARCHITECTURE c07s02b07x00p01n01i02324arch OF c07s02b07x00p01n01i02324ent IS

BEGIN
  TESTING: PROCESS
    -- Local declarations.
    variable INTV1    : INTEGER;
    variable INTV2    : INTEGER;
    variable INTV3    : INTEGER;
    variable INTV4    : INTEGER;
    variable INTV5    : INTEGER;
    variable INTV6    : INTEGER;
    variable INTV7    : INTEGER;
    variable INTV8    : INTEGER;
    variable REALV1   : REAL;
    variable REALV2   : REAL;
    variable REALV3   : REAL;
    variable REALV4   : REAL;
    variable REALV5   : REAL;
    variable REALV6   : REAL;
    variable REALV7   : REAL;
    variable REALV8   : REAL;
  BEGIN
    -- Test absolute value of integer literals and variables.
    INTV1 := abs (-5);
    assert (INTV1 = 5)
      report "Assertion Violation(1)";
    INTV2 := abs 5;
    assert (INTV2 = 5)
      report "Assertion Violation(2)";
    INTV3 := abs 0;
    assert (INTV3 = 0)
      report "Assertion Violation(3)";
    INTV4 := abs INTEGER'HIGH;
    assert (INTV4 = INTEGER'HIGH)
      report "Assertion Violation(4)";
    
    INTV5 := -5;
    INTV5 := abs INTV5;
    assert (INTV5 = 5)
      report "Assertion Violation(5)";
    INTV6 := 5;
    INTV6 := abs 5;
    assert (INTV6 = 5)
      report "Assertion Violation(6)";
    INTV7 := 0;
    INTV7 := abs 0;
    assert (INTV7 = 0)
      report "Assertion Violation(7)";
    INTV8 := INTEGER'HIGH;
    INTV8 := abs INTEGER'HIGH;
    assert (INTV8 = INTEGER'HIGH)
      report "Assertion Violation(8)";
    
    -- Do the same for the predefined physical type TIME.
    assert (abs (-5 ns) = 5 ns)
      report "Assertion Violation(9)";
    assert (abs 5 ns = 5 ns)
      report "Assertion Violation(10)";
    assert (abs 0 fs = 0 fs)
      report "Assertion Violation(11)";
    assert (abs TIME'HIGH = TIME'HIGH)
      report "Assertion Violation(12)";
    
    -- Test absolute value of real literals and variables.
    REALV1 := abs (-5.0);
    assert (REALV1 = 5.0)
      report "Assertion Violation(13)";
    REALV2 := abs 5.0;
    assert (REALV2 = 5.0)
      report "Assertion Violation(14)";
    REALV3 := abs 0.0;
    assert (REALV3 = 0.0)
      report "Assertion Violation(15)";
    REALV4 := abs REAL'HIGH;
    assert (REALV4 = REAL'HIGH)
      report "Assertion Violation(16)";

    REALV5 := -5.0;
    REALV5 := abs REALV5;
    assert (REALV5 = 5.0)
      report "Assertion Violation(17)";
    REALV6 := 5.0;
    REALV6 := abs 5.0;
    assert (REALV6 = 5.0)
      report "Assertion Violation(18)";
    REALV7 := 0.0;
    REALV7 := abs 0.0;
    assert (REALV7 = 0.0)
      report "Assertion Violation(19)";
    REALV8 := REAL'HIGH;
    REALV8 := abs REAL'HIGH;
    assert (REALV8 = REAL'HIGH)
      report "Assertion Violation(20)";
    wait for 5 ns;
    assert NOT(   (INTV1 = 5)      and
                  (INTV2 = 5)      and
                  (INTV3 = 0)      and
                  (INTV4 = INTEGER'HIGH)   and
                  (INTV5 = 5)      and
                  (INTV6 = 5)      and
                  (INTV7 = 0)      and
                  (INTV8 = INTEGER'HIGH)   and
                  (abs (-5 ns) = 5 ns)   and
                  (abs 5 ns = 5 ns)   and
                  (abs 0 fs = 0 fs)   and
                  (abs TIME'HIGH = TIME'HIGH)   and
                  (REALV1 = 5.0)      and
                  (REALV2 = 5.0)      and
                  (REALV3 = 0.0)      and
                  (REALV4 = REAL'HIGH)   and
                  (REALV5 = 5.0)      and
                  (REALV6 = 5.0)      and
                  (REALV7 = 0.0)      and
                  (REALV8 = REAL'HIGH)   ) 
      report "***PASSED TEST: c07s02b07x00p01n01i02324" 
      severity NOTE;
    assert (   (INTV1 = 5)      and
               (INTV2 = 5)      and
               (INTV3 = 0)      and
               (INTV4 = INTEGER'HIGH)   and
               (INTV5 = 5)      and
               (INTV6 = 5)      and
               (INTV7 = 0)      and
               (INTV8 = INTEGER'HIGH)   and
               (abs (-5 ns) = 5 ns)   and
               (abs 5 ns = 5 ns)   and
               (abs 0 fs = 0 fs)   and
               (abs TIME'HIGH = TIME'HIGH)   and
               (REALV1 = 5.0)      and
               (REALV2 = 5.0)      and
               (REALV3 = 0.0)      and
               (REALV4 = REAL'HIGH)   and
               (REALV5 = 5.0)      and
               (REALV6 = 5.0)      and
               (REALV7 = 0.0)      and
               (REALV8 = REAL'HIGH)   ) 
      report "***FAILED TEST: c07s02b07x00p01n01i02324 - Unary operator abs for any numeric type test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b07x00p01n01i02324arch;
