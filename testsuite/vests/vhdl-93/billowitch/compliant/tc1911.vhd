
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
-- $Id: tc1911.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b00x00p09n02i01911ent IS
END c07s02b00x00p09n02i01911ent;

ARCHITECTURE c07s02b00x00p09n02i01911arch OF c07s02b00x00p09n02i01911ent IS

BEGIN
  TESTING: PROCESS
    variable ValueI1, SameValueI1, DifferentValueI1 : INTEGER;
    variable ValueI2, SameValueI2, DifferentValueI2 : INTEGER;
    variable ValueI3, SameValueI3, DifferentValueI3 : INTEGER;
    variable ValueI4, SameValueI4, DifferentValueI4 : INTEGER;
  BEGIN
    -- adding operators.  
    --   Cannot compare the "&" operator against either the "+" or 
    --   "-" operators.  The only two we can compare are the "+"
    --         and the "-" operators.
    -- "+", "-"
    ValueI1         := 14 + 1 - 9 - 7;
    SameValueI1     := ((14 + 1) - 9) - 7;
    DifferentValueI1:= 14 + (1 - (9 - 7));
    assert (ValueI1 = SameValueI1)
      report "Values of same precedence are not evaluated left to right.";
    assert (ValueI1 /= DifferentValueI1)
      report "Values of same precedence are not evaluated left to right.";
    
    -- multiplying operators.
    -- "*", "/"
    ValueI2          := 14 / 7 * 3;
    SameValueI2      := (14 / 7) * 3;   -- 6
    DifferentValueI2 := 14 / (7 * 3);   -- 0
    assert (ValueI2 = SameValueI2)
      report "Values of same precedence are not evaluated left to right.";
    assert (ValueI2 /= DifferentValueI2)
      report "Values of same precedence are not evaluated left to right.";
    
    -- "*", "mod"
    ValueI3          := 14 mod 7 * 3;
    SameValueI3      := (14 mod 7) * 3;   -- 0
    DifferentValueI3 := 14 mod (7 * 3);   -- 14
    assert (ValueI3 = SameValueI3)
      report "Values of same precedence are not evaluated left to right.";
    assert (ValueI3 /= DifferentValueI3)
      report "Values of same precedence are not evaluated left to right.";

    -- "*", "rem"
    ValueI4          := 14 rem 7 * 3;
    SameValueI4      := (14 rem 7) * 3;   -- 0
    DifferentValueI4 := 14 rem (7 * 3);   -- 14
    assert (ValueI4 = SameValueI4)
      report "Values of same precedence are not evaluated left to right.";
    assert (ValueI4 /= DifferentValueI4)
      report "Values of same precedence are not evaluated left to right.";
    
    wait for 5 ns;
    assert NOT(    (ValueI1 = SameValueI1)   and
                   (ValueI1 /= DifferentValueI1)   and
                   (ValueI2 = SameValueI2)   and
                   (ValueI2 /= DifferentValueI2)   and
                   (ValueI3 = SameValueI3)   and
                   (ValueI3 /= DifferentValueI3)   and
                   (ValueI4 = SameValueI4)   and
                   (ValueI4 /= DifferentValueI4)   )
      report "***PASSED TEST: c07s02b00x00p09n02i01911"
      severity NOTE;
    assert (    (ValueI1 = SameValueI1)   and
                (ValueI1 /= DifferentValueI1)   and
                (ValueI2 = SameValueI2)   and
                (ValueI2 /= DifferentValueI2)   and
                (ValueI3 = SameValueI3)   and
                (ValueI3 /= DifferentValueI3)   and
                (ValueI4 = SameValueI4)   and
                (ValueI4 /= DifferentValueI4)   )
      report "***FAILED TEST: c07s02b00x00p09n02i01911 - Operators are not associated with their operands in textual order."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b00x00p09n02i01911arch;
