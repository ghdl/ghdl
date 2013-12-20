
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
-- $Id: tc2187.vhd,v 1.2 2001-10-26 16:29:46 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b05x00p01n01i02187ent IS
END c07s02b05x00p01n01i02187ent;

ARCHITECTURE c07s02b05x00p01n01i02187arch OF c07s02b05x00p01n01i02187ent IS

BEGIN
  TESTING: PROCESS
    -- Local declarations.
    variable INTV1   : INTEGER      := 0;
    variable INTV2   : INTEGER      := 0;
    variable TIMEV1  : TIME         := 1 ns;
    variable TIMEV2  : TIME         := 1 ns;
    variable REALV1  : REAL         := 0.0;
    variable REALV2  : REAL         := 0.0;
  BEGIN
    -- Test negation of simple integers.
    INTV1 := 14;
    assert (-INTV1 = -14)
      report "Negation of simple integers does not work.";
    INTV2 := INTEGER'HIGH;
    assert (-INTV2 = -INTEGER'HIGH)
      report "Negation of simple integers does not work.";

    -- Test identity of simple integers.
    assert (+14 = 14)
      report "Identity of simple integers does not work.";

    -- Test negation of "TIME".
    TIMEV1 := 1 ns;
    assert (-TIMEV1 = -1 ns)
      report "Negation of TIME values does not work.";
    TIMEV2 := TIME'HIGH;
    assert (-TIMEV2 = -TIME'HIGH)
      report "Negation of TIME values does not work.";
    
    -- Test identify of "TIME".
    assert (+14 ps = 14 ps)
      report "Identity of TIME values does not work.";
    
    -- Test negation of simple floating point numbers.
    REALV1 := 14.0;
    assert (-REALV1 = -14.0)
      report "Negation of simple integers does not work.";
    REALV2 := REAL'HIGH;
    assert (-REALV2 = -REAL'HIGH)
      report "Negation of simple integers does not work.";
    
    -- Test identity of simple floating point numbers.
    assert (+14.0 = 14.0)
      report "Identity of simple integers does not work.";
    wait for 5 fs;
    assert NOT(   (-INTV1 = -14)         and
                  (-INTV2 = -INTEGER'HIGH)   and
                  (+14 = 14)         and
                  (-TIMEV1 = -1 ns)      and
                  (-TIMEV2 = -TIME'HIGH)      and
                  (+14 ps = 14 ps)      and
                  (-REALV1 = -14.0)      and
                  (-REALV2 = -REAL'HIGH)      and
                  (+14.0 = 14.0)         )
      report "***PASSED TEST: c07s02b05x00p01n01i02187" 
      severity NOTE;
    assert (   (-INTV1 = -14)         and
               (-INTV2 = -INTEGER'HIGH)   and
               (+14 = 14)         and
               (-TIMEV1 = -1 ns)      and
               (-TIMEV2 = -TIME'HIGH)      and
               (+14 ps = 14 ps)      and
               (-REALV1 = -14.0)      and
               (-REALV2 = -REAL'HIGH)      and
               (+14.0 = 14.0)         )
      report "***FAILED TEST: c07s02b05x00p01n01i02187 - Identity and nefation function did not work correctly for all numeric types."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b05x00p01n01i02187arch;
