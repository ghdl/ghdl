
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
-- $Id: tc2113.vhd,v 1.2 2001-10-26 16:29:45 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b04x00p20n01i02113ent IS
END c07s02b04x00p20n01i02113ent;

ARCHITECTURE c07s02b04x00p20n01i02113arch OF c07s02b04x00p20n01i02113ent IS

  TYPE     time_v       is array (integer range <>) of time;
  SUBTYPE     time_4       is time_v (1 to 4);
  SUBTYPE     time_8       is time_v (1 to 8);

BEGIN
  TESTING : PROCESS
    variable result    : time_8;
    variable l_operand : time_4 := ( 78 ns , 23 ns , 78 ns , 23 ns );
    variable r_operand : time_4 := ( 23 ns , 23 ns , 78 ns , 78 ns );
  BEGIN
    result := l_operand & r_operand;
    wait for 20 ns;
    assert NOT((result = ( 78 ns, 23 ns, 78 ns, 23 ns, 23 ns, 23 ns, 78 ns, 78 ns )) and (result(1) = 78 ns))
      report "***PASSED TEST: c07s02b04x00p20n01i02113"
      severity NOTE;
    assert ((result = ( 78 ns, 23 ns, 78 ns, 23 ns, 23 ns, 23 ns, 78 ns, 78 ns )) and (result(1) = 78 ns))
      report "***FAILED TEST: c07s02b04x00p20n01i02113 - Concatenation of two TIME arrays failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b04x00p20n01i02113arch;
