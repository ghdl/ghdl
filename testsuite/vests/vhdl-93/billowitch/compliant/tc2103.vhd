
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
-- $Id: tc2103.vhd,v 1.2 2001-10-26 16:29:45 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b04x00p20n01i02103ent IS
END c07s02b04x00p20n01i02103ent;

ARCHITECTURE c07s02b04x00p20n01i02103arch OF c07s02b04x00p20n01i02103ent IS

  TYPE     positive_v is array (integer range <>) of positive;
  SUBTYPE     positive_8 is positive_v (1 to 8);
  SUBTYPE     positive_4 is positive_v (1 to 4);
BEGIN
  TESTING : PROCESS
    variable result    : positive_4;
    variable l_operand : positive_4 := ( 1 , 89 , 1 , 89 );
    variable r_operand : positive_4 := ( 89 , 89 , 1 , 1 );
    alias    l_alias   : positive_v (1 to 2) is l_operand (2 to 3);
    alias    r_alias   : positive_v (1 to 2) is r_operand (3 to 4);
  BEGIN
    result := l_alias & r_alias;
    wait for 20 ns;
    assert NOT(result = ( 89 , 1 , 1 , 1 ))
      report "***PASSED TEST: c07s02b04x00p20n01i02103"
      severity NOTE;
    assert (result = ( 89 , 1 , 1 , 1 ))
      report "***FAILED TEST: c07s02b04x00p20n01i02103 - Concatenation of two RECORD arrays failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b04x00p20n01i02103arch;
