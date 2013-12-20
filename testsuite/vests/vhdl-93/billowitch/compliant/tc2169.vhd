
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
-- $Id: tc2169.vhd,v 1.2 2001-10-26 16:29:46 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b04x00p22n01i02169ent IS
END c07s02b04x00p22n01i02169ent;

ARCHITECTURE c07s02b04x00p22n01i02169arch OF c07s02b04x00p22n01i02169ent IS

  TYPE     severity_level_v is array (integer range <>) of severity_level;    
  SUBTYPE  severity_level_8 is severity_level_v (1 to 8);
  SUBTYPE  severity_level_4 is severity_level_v (1 to 4);

BEGIN
  TESTING: PROCESS
    variable result    : severity_level_4;
    variable l_operand : severity_level_4 := ( NOTE , FAILURE , NOTE , FAILURE );
    variable r_operand : severity_level_4 := ( FAILURE , FAILURE , NOTE , NOTE );
    alias    l_alias   : severity_level_v (1 to 2) is l_operand (2 to 3);
    alias    r_alias   : severity_level_v (1 to 2) is r_operand (3 to 4);
  BEGIN
    result := l_alias & r_alias;
    wait for 5 ns;
    assert NOT(( result = ( FAILURE , NOTE , NOTE , NOTE )) and ( result(1) =  FAILURE  ))
      report "***PASSED TEST: c07s02b04x00p22n01i02169"
      severity NOTE;
    assert (( result = ( FAILURE , NOTE , NOTE , NOTE )) and ( result(1) =  FAILURE  ))
      report "***FAILED TEST: c07s02b04x00p22n01i02169 - Concatenation of two SEVERITY_LEVEL aliases failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b04x00p22n01i02169arch;
