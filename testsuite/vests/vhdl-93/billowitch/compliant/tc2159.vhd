
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
-- $Id: tc2159.vhd,v 1.2 2001-10-26 16:29:46 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b04x00p21n01i02159ent IS
END c07s02b04x00p21n01i02159ent;

ARCHITECTURE c07s02b04x00p21n01i02159arch OF c07s02b04x00p21n01i02159ent IS

  TYPE     severity_level_v is array (integer range <>) of severity_level;    
  SUBTYPE  severity_level_5 is severity_level_v (1 to 5);
  SUBTYPE  severity_level_4 is severity_level_v (1 to 4);

BEGIN
  TESTING: PROCESS
    variable result    : severity_level_5;
    variable l_operand : severity_level :=  NOTE ;
    variable r_operand : severity_level_4 := ( NOTE ,  FAILURE ,  NOTE ,  FAILURE  );
  BEGIN
--
-- The element is treated as an implicit single element array !
--
    result := l_operand & r_operand;
    wait for 5 ns;
    assert NOT((result=(NOTE,NOTE,FAILURE,NOTE,FAILURE)) and (result(1)=NOTE))
      report "***PASSED TEST: c07s02b04x00p21n01i02159"
      severity NOTE;
    assert ((result=(NOTE,NOTE,FAILURE,NOTE,FAILURE)) and (result(1)=NOTE))
      report "***FAILED TEST: c07s02b04x00p21n01i02159 - Concatenation of element and SEVERITY_LEVEL array failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b04x00p21n01i02159arch;
