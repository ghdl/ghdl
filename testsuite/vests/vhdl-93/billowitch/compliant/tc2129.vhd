
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
-- $Id: tc2129.vhd,v 1.2 2001-10-26 16:29:45 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b04x00p20n01i02129ent IS
END c07s02b04x00p20n01i02129ent;

ARCHITECTURE c07s02b04x00p20n01i02129arch OF c07s02b04x00p20n01i02129ent IS

  TYPE     boolean_v is array (integer range <>) of boolean;
  SUBTYPE     boolean_5 is boolean_v (1 to 5);
  SUBTYPE     boolean_4 is boolean_v (1 to 4);

BEGIN
  TESTING: PROCESS
    variable result    : boolean_5;
    variable l_operand : boolean    := true;
    variable r_operand : boolean_4    := (true, false, true, false);
  BEGIN
    result := l_operand & r_operand;
    wait for 5 ns;
    assert NOT((result = (true, true, false, true, false)) and (result(1) = true))
      report "***PASSED TEST: c07s02b04x00p20n01i02129"
      severity NOTE;
    assert ((result = (true, true, false, true, false)) and (result(1) = true))
      report "***FAILED TEST: c07s02b04x00p20n01i02129 - Concatenation of element and BOOLEAN array failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b04x00p20n01i02129arch;
