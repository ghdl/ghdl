
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
-- $Id: tc2016.vhd,v 1.2 2001-10-26 16:29:45 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b02x00p16n02i02016ent IS
END c07s02b02x00p16n02i02016ent;

ARCHITECTURE c07s02b02x00p16n02i02016arch OF c07s02b02x00p16n02i02016ent IS

BEGIN
  TESTING: PROCESS
    type array_three is array (1 to 6) of integer;
    variable array_1 : array_three := (6,5,4,3,2,1);
    variable array_2 : array_three := (6,5,4,1,2,3);
    variable k : integer := 0;
  BEGIN
    if array_1 > array_2 then
      k := 5;
    end if;
    wait for 5 ns;
    assert NOT(k=5)
      report "***PASSED TEST: c07s02b02x00p16n02i02016"
      severity NOTE;
    assert ( k=5 )
      report "***FAILED TEST: c07s02b02x00p16n02i02016 - The relations > (greater than) and >= (greater than or equal) are defined to be the complements of the <= and < operators respectively for the same two operands."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b02x00p16n02i02016arch;
