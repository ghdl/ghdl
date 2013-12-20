
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
-- $Id: tc2079.vhd,v 1.2 2001-10-26 16:29:45 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b04x00p20n01i02079ent IS
END c07s02b04x00p20n01i02079ent;

ARCHITECTURE c07s02b04x00p20n01i02079arch OF c07s02b04x00p20n01i02079ent IS

BEGIN
  TESTING: PROCESS
    type array_type is array (positive range <>) of integer;
    -- No_failure_here
    constant a : array_type (1 to 3) := (1, 2, 3);
    constant b : array_type (1 to 5) := (1, 2, 3, 4, 5);
    constant x : array_type := a & b;
  BEGIN
    assert NOT(x=(1,2,3,1,2,3,4,5))
      report "***PASSED TEST: c07s02b04x00p20n01i02079"
      severity NOTE;
    assert (x=(1,2,3,1,2,3,4,5))
      report "***FAILED TEST: c07s02b04x00p20n01i02079 - The result of the concatenation of two one-dimensional arrays is a one-dimensional array whose length is the sum of the lengths of its operands, and whose elements consist of the elements of the left operand (in left to right order) followed by the elements of the right operand (in left to right order)." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b04x00p20n01i02079arch;
