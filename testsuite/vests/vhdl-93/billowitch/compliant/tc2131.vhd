
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
-- $Id: tc2131.vhd,v 1.2 2001-10-26 16:29:45 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b04x00p20n01i02131ent IS
END c07s02b04x00p20n01i02131ent;

ARCHITECTURE c07s02b04x00p20n01i02131arch OF c07s02b04x00p20n01i02131ent IS

  TYPE    integer_v  is array (integer range <>) of integer;
  SUBTYPE     integer_4  is integer_v (1 to 4);
  SUBTYPE     integer_8  is integer_v (1 to 8);

BEGIN
  TESTING: PROCESS
    variable r_operand : integer_4 := ( 5,6,7,8 );
    variable l_operand1: integer := 1;
    variable l_operand2: integer := 2;
    variable l_operand3: integer := 3;
    variable l_operand4: integer := 4;
    variable result    : integer_8;
  BEGIN
    result := l_operand1 &
              l_operand2 &
              l_operand3 &
              l_operand4 &
              r_operand;
    assert (result = (1,2,3,4,5,6,7,8))
      report "integer implicit array concatenation failed"
      severity FAILURE;
    assert NOT(result = (1,2,3,4,5,6,7,8))
      report "***PASSED TEST: c07s02b04x00p20n01i02131"
      severity NOTE;
    assert (result = (1,2,3,4,5,6,7,8))
      report "***FAILED TEST: c07s02b04x00p20n01i02131 - The left bound of this implicit array is the left bound of the index subtype of the array and its direction is ascending if the index subtype is ascending."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b04x00p20n01i02131arch;
