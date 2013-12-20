
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
-- $Id: tc2004.vhd,v 1.2 2001-10-26 16:29:44 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b02x00p08n02i02004ent IS
END c07s02b02x00p08n02i02004ent;

ARCHITECTURE c07s02b02x00p08n02i02004arch OF c07s02b02x00p08n02i02004ent IS

BEGIN
  TESTING: PROCESS
    type    array_x    is array (positive range <>) of integer;
    subtype    array_three    is array_x (1 to 6) ;
    subtype    array_four     is array_x (6 downto 1) ;
    variable    x : array_four  := (1,2,3,4,5,6);
    variable    y : array_three := (1,2,3,4,5,6);
  BEGIN
    assert NOT(x=y)
      report "***PASSED TEST: c07s02b02x00p08n02i02004"
      severity NOTE;
    assert ( x=y )
      report "***FAILED TEST: c07s02b02x00p08n02i02004 - Two composite values of the same type are equal if and only if for each element of the left operand there is a matching element of the right operand and vice versa."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b02x00p08n02i02004arch;
