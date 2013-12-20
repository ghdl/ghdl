
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
-- $Id: tc1907.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s01b00x00p11n01i01907ent IS
END c07s01b00x00p11n01i01907ent;

ARCHITECTURE c07s01b00x00p11n01i01907arch OF c07s01b00x00p11n01i01907ent IS

BEGIN
  TESTING: PROCESS
    -- Local declarations.
    variable b1a, b2a, b3a, b4a : BOOLEAN;
    variable b1o, b2o, b3o, b4o : BOOLEAN;
    variable b1x, b2x, b3x, b4x : BOOLEAN;
  BEGIN
    -- Test that the following operators can be used associatively.
    -- 1. AND.
    b1a := TRUE;
    b2a := TRUE;
    b3a := FALSE;
    assert (NOT (b1a AND b2a AND b3a))
      report "AND operator cannot be used associatively.";
    b4a := TRUE;
    assert (b1a AND b2a AND b4a)
      report "AND operator cannot be used associatively.";

    -- 2. OR.
    b1o := FALSE;
    b2o := FALSE;
    b3o := TRUE;
    assert (b1o OR b2o OR b3o)
      report "OR operator cannot be used associatively.";
    b4o := FALSE;
    assert (NOT (b1o OR b2o OR b4o))
      report "OR operator cannot be used associatively.";
    
    -- 3. XOR.
    b1x := TRUE;
    b2x := TRUE;
    b3x := FALSE;
    assert (NOT (b1x XOR b2x XOR b3x))
      report "XOR operator cannot be used associatively.";
    b4x := TRUE;
    assert (b1x XOR b2x XOR b4x)
      report "XOR operator cannot be used associatively.";

    wait for 5 ns;

    assert NOT(    (NOT (b1a AND b2a AND b3a))    and
                   (b1a AND b2a AND b4a)      and
                   (b1o OR b2o OR b3o)      and
                   (NOT (b1o OR b2o OR b4o))   and
                   (NOT (b1x XOR b2x XOR b3x))   and
                   (b1x XOR b2x XOR b4x)      )
      report "***PASSED TEST: /src/ch07/sc01/p012/s010101.vhd"
      severity NOTE;
    assert (    (NOT (b1a AND b2a AND b3a))    and
                (b1a AND b2a AND b4a)      and
                (b1o OR b2o OR b3o)      and
                (NOT (b1o OR b2o OR b4o))   and
                (NOT (b1x XOR b2x XOR b3x))   and
                (b1x XOR b2x XOR b4x)      )
      report "***FAILED TEST: c07s01b00x00p11n01i01907 - Associative test for and or and xor failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s01b00x00p11n01i01907arch;
