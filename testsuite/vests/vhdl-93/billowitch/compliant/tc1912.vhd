
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
-- $Id: tc1912.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b00x00p09n03i01912ent IS
END c07s02b00x00p09n03i01912ent;

ARCHITECTURE c07s02b00x00p09n03i01912arch OF c07s02b00x00p09n03i01912ent IS

BEGIN
  TESTING: PROCESS
    variable   b1a,b2a,b3a   : BOOLEAN;
    variable   b1b,b2b,b3b   : BOOLEAN;
    variable   b1c,b2c,b3c   : BOOLEAN;
    variable   i1a,i2a,i3a   : INTEGER;
    variable   i1b,i2b,i3b   : INTEGER;
  BEGIN
    -- I. logical operator and relational operator.
    b1a              := FALSE;
    b2a              := FALSE;
    b3a              := TRUE;
    assert ((b1a and b2a) /= b3a)
      report "1:Parentheses do NOT change the precedence of operation.";
    
    -- V. logical operator and miscellaneous operator.
    b1b              := FALSE;
    b2b              := TRUE;
    assert (not (b1b and b2b))
      report "2:Parentheses do NOT change the precedence of operation.";
    
    -- VI. relational operators cannot be thus compared to ANY other operators
    --     but the NOT operator, because they return boolean values and no other
    --     higher precedence operators work on this type.
    b1c              := FALSE;
    b2c              := TRUE;
    assert (not (b1c >= b2c))
      report "3:Parentheses do NOT change the precedence of operation.";

    -- VIII. adding operator and multiplying operator.
    i1a              := 3;
    i2a              := 4;
    i3a              := 5;
    assert (((i1a + i2a) * i3a) = 35)
      report "4:Parentheses do NOT change the precedence of operation.";
    
    -- XII. multiplying operator and miscellaneous operator.
    i1b              := 2;
    i2b              := 3;
    i3b              := 2;
    assert (((i1b + i2b) ** i3b) = 25)
      report "5:Parentheses do NOT change the precedence of operation.";

    wait for 5 ns;
    assert NOT(   ((b1a and b2a) /= b3a)      and
                  (not (b1b and b2b))      and
                  (not (b1c >= b2c))      and
                  (((i1a + i2a) * i3a) = 35)   and
                  (((i1b + i2b) ** i3b) = 25)   )
      report "***PASSED TEST: c07s02b00x00p09n03i01912"
      severity NOTE;
    assert (   ((b1a and b2a) /= b3a)      and
               (not (b1b and b2b))      and
               (not (b1c >= b2c))      and
               (((i1a + i2a) * i3a) = 35)   and
               (((i1b + i2b) ** i3b) = 25)   )
      report "***FAILED TEST: c07s02b00x00p09n03i01912 - The parentheses should be able to control the association of operators and operands."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b00x00p09n03i01912arch;
