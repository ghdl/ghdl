
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
-- $Id: tc2114.vhd,v 1.2 2001-10-26 16:29:45 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b04x00p20n01i02114ent IS
END c07s02b04x00p20n01i02114ent;

ARCHITECTURE c07s02b04x00p20n01i02114arch OF c07s02b04x00p20n01i02114ent IS

  TYPE     real_v is array (integer range <>) of real;
  SUBTYPE     real_8 is real_v (1 to 8);
  SUBTYPE     real_4 is real_v (1 to 4);

BEGIN
  TESTING : PROCESS
    variable result    : real_4;
    variable l_operand : real_4 := (12.345, -67.890,12.345, -67.890);
    variable r_operand : real_4 := ( -67.890, -67.890,12.345,12.345);
    alias    l_alias   : real_v (1 to 2) is l_operand (2 to 3);
    alias    r_alias   : real_v (1 to 2) is r_operand (3 to 4);
  BEGIN
    result := l_alias & r_alias;
    wait for 20 ns;
    assert NOT((result = ( -67.890,12.345,12.345,12.345)) and (result(1)=-67.890))
      report "***PASSED TEST:c07s02b04x00p20n01i02114"
      severity NOTE;
    assert ((result = ( -67.890,12.345,12.345,12.345)) and (result(1)=-67.890))
      report "***FAILED TEST: c07s02b04x00p20n01i02114 - Concatenation of two REAL aliases failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b04x00p20n01i02114arch;
