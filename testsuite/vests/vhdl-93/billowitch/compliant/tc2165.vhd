
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
-- $Id: tc2165.vhd,v 1.2 2001-10-26 16:29:46 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b04x00p22n01i02165ent IS
END c07s02b04x00p22n01i02165ent;

ARCHITECTURE c07s02b04x00p22n01i02165arch OF c07s02b04x00p22n01i02165ent IS

  TYPE     time_v is array (integer range <>) of time;
  SUBTYPE     time_2 is time_v (1 to 2);

BEGIN
  TESTING: PROCESS
    variable result    : time_2;
    variable l_operand : time :=  78 ns ;
    variable r_operand : time :=  23 ns ;
  BEGIN
--
-- The element is treated as an implicit single element array !
--
    result := l_operand & r_operand;
    wait for 5 ns;
    assert NOT(result = ( 78 ns, 23 ns ))
      report "***PASSED TEST: c07s02b04x00p22n01i02165"
      severity NOTE;
    assert (result = ( 78 ns, 23 ns ))
      report "***FAILED TEST: c07s02b04x00p22n01i02165 - Concatenation of element and element failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b04x00p22n01i02165arch;
