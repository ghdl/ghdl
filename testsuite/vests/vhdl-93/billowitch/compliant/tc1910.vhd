
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
-- $Id: tc1910.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b00x00p09n01i01910ent IS
END c07s02b00x00p09n01i01910ent;

ARCHITECTURE c07s02b00x00p09n01i01910arch OF c07s02b00x00p09n01i01910ent IS

BEGIN
  TESTING: PROCESS
    -- Local declarations.
    variable ValueB1 : BOOLEAN;
    variable ValueB2 : BOOLEAN;
    variable ValueB3 : BOOLEAN;
    variable ValueB4 : BOOLEAN;
    variable ValueI1, SameValueI1, DifferentValueI1 : INTEGER;
    variable ValueI2, SameValueI2, DifferentValueI2 : INTEGER;
    variable ValueI3, SameValueI3, DifferentValueI3 : INTEGER;
    variable ValueI4, SameValueI4, DifferentValueI4 : INTEGER;
    variable ValueI5, SameValueI5, DifferentValueI5 : INTEGER;
    variable ValueI6, SameValueI6, DifferentValueI6 : INTEGER;
    variable ValueI7, SameValueI7, DifferentValueI7 : INTEGER;
  BEGIN
    -- "+" (addition) operator, and the "-" operator.
    --     - NOTE:  The following expression would not be able to parse
    --              if the precedence used was such that the "=" operator
    --              had a higher precedence than the "+" operator.  Thus,
    --              if this parses you are guaranteed that the precedence
    --              relative to these two levels is correctly defined.Same
    --              goes for the "-" operator.
    ValueB1         := 1 + 3 = 3 + 1;
    assert (ValueB1)
      report "The expression has not been processed correctly.(5)";
    ValueB2         := 3 - 1 = 5 - 3;
    assert (ValueB2)
      report "The expression has not been processed correctly.(6)";
    
    -- "+" (sign) operator, and the "-" (sign) operator.
    --     - NOTE:  The following expression would not be able to parse
    --              if the precedence used was such that the "=" operator
    --              had a higher precedence than the "+" operator.  Thus,
    --              if this parses you are guaranteed that the precedence
    --              relative to these two levels is correctly defined.Same
    --              goes for the "-" operator.
    ValueB3         := + 1 = + 1;
    assert (ValueB3)
      report "The expression has not been processed correctly.(7)";
    ValueB4         := - 3 = - 3;
    assert (ValueB4)
      report "The expression has not been processed correctly.(8)";

    ValueI1         := -3 + 4;
    SameValueI1     := (-3) + 4;
    DifferentValueI1:= -(3 + 4);
    assert (ValueI1 = SameValueI1)
      report "Values of lower precedence associated before those of higher precedence.(9)";
    assert (ValueI1 /= DifferentValueI1)
      report "Values of lower precedence associated before those of higher precedence.(10)";
    
    -- "*" operator.
    ValueI2         := 3 + 4 * 5;
    SameValueI2     := 3 + (4 * 5);
    DifferentValueI2:= (3 + 4) * 5;
    assert (ValueI2 = SameValueI2)
      report "Values of lower precedence associated before those of higher precedence.(13)";
    assert (ValueI2 /= DifferentValueI2)
      report "Values of lower precedence associated before those of higher precedence.(14)";
    
    -- "/" operator.
    ValueI3         := 5 + 10 / 5;
    SameValueI3     := 5 + (10 / 5);
    DifferentValueI3:= (5 + 10) / 5;
    assert (ValueI3 = SameValueI3)
      report "Values of lower precedence associated before those of higher precedence.(15)";
    assert (ValueI3 /= DifferentValueI3)
      report "Values of lower precedence associated before those of higher precedence.(16)";
    
    -- "mod" operator.
    ValueI4         := 4 + 11 mod 3;
    SameValueI4     := 4 + (11 mod 3);
    DifferentValueI4:= (4 + 11) mod 3;
    assert (ValueI4 = SameValueI4)
      report "Values of lower precedence associated before those of higher precedence.(17)";
    assert (ValueI4 /= DifferentValueI4)
      report "Values of lower precedence associated before those of higher precedence.(18)";
    
    -- "rem" operator.
    ValueI5         := 4 + 11 rem 3;
    SameValueI5     := 4 + (11 rem 3);
    DifferentValueI5:= (4 + 11) rem 3;
    assert (ValueI5 = SameValueI5)
      report "Values of lower precedence associated before those of higher precedence.(19)";
    assert (ValueI5 /= DifferentValueI5)
      report "Values of lower precedence associated before those of higher precedence.(20)";

    -- "**" operator.
    ValueI6         := 3 * 4 ** 2;
    SameValueI6     := 3 * (4 ** 2);
    DifferentValueI6:= (3 * 4) ** 2;
    assert (ValueI6 = SameValueI6)
      report "Values of lower precedence associated before those of higher precedence.(21)";
    assert (ValueI6 /= DifferentValueI6)
      report "Values of lower precedence associated before those of higher precedence.(22)";
    
    -- "abs" operator.
    ValueI7         := abs (-5) * (-7);
    SameValueI7     := (abs (-5)) * (-7);
    DifferentValueI7:= abs((-5) * (-7));
    assert (ValueI7 = SameValueI7)
      report "Values of lower precedence associated before those of higher precedence.(23)";
    assert (ValueI7 /= DifferentValueI7)
      report "Values of lower precedence associated before those of higher precedence.(24)";

    wait for 5 ns;

    assert NOT(   (ValueB1)   and
                  (ValueB2)   and 
                  (ValueB3)   and 
                  (ValueB4)   and 
                  (ValueI1 = SameValueI1)      and
                  (ValueI1 /= DifferentValueI1)   and
                  (ValueI2 = SameValueI2)      and
                  (ValueI2 /= DifferentValueI2)   and
                  (ValueI3 = SameValueI3)      and
                  (ValueI3 /= DifferentValueI3)   and
                  (ValueI4 = SameValueI4)      and
                  (ValueI4 /= DifferentValueI4)   and
                  (ValueI5 = SameValueI5)      and
                  (ValueI5 /= DifferentValueI5)   and
                  (ValueI6 = SameValueI6)      and
                  (ValueI6 /= DifferentValueI6)   and
                  (ValueI7 = SameValueI7)      and
                  (ValueI7 /= DifferentValueI7)   )
      report "***PASSED TEST: c07s02b00x00p09n01i01910"
      severity NOTE;
    assert (   (ValueB1)   and
               (ValueB2)   and 
               (ValueB3)   and 
               (ValueB4)   and 
               (ValueI1 = SameValueI1)      and
               (ValueI1 /= DifferentValueI1)   and
               (ValueI2 = SameValueI2)      and
               (ValueI2 /= DifferentValueI2)   and
               (ValueI3 = SameValueI3)      and
               (ValueI3 /= DifferentValueI3)   and
               (ValueI4 = SameValueI4)      and
               (ValueI4 /= DifferentValueI4)   and
               (ValueI5 = SameValueI5)      and
               (ValueI5 /= DifferentValueI5)   and
               (ValueI6 = SameValueI6)      and
               (ValueI6 /= DifferentValueI6)   and
               (ValueI7 = SameValueI7)      and
               (ValueI7 /= DifferentValueI7)   )
      report "***FAILED TEST: c07s02b00x00p09n01i01910 - Operators of higher precedence are associated with their operands before operators of lower precedence."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b00x00p09n01i01910arch;
