
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
-- $Id: tc1986.vhd,v 1.2 2001-10-26 16:29:44 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b02x00p02n01i01986ent IS
  type omega is range (-100) to 100
    units
      o1;
      o2  = 5 o1;
      o3  = 10 o1;
    end units;
END c07s02b02x00p02n01i01986ent;

ARCHITECTURE c07s02b02x00p02n01i01986arch OF c07s02b02x00p02n01i01986ent IS

BEGIN
  TESTING: PROCESS
    variable om1, om2, om3 : omega;

    --alias in A of variable in A of E physical type
    
    alias al1 : omega is om1;
    alias al2 : omega is om2;
    alias al3 : omega is om3;
  BEGIN
    om1 := 4 o1;
    om2 := 5 o1;
    om3 := 6 o1;

    assert NOT(     5 o1 = 5 o1      and
                    5 o1 = abs(5 o1)   and
                    5 o1 = abs(-5 o1)   and
                    4 o1 /= 5 o1      and
                    4 o1 /= abs(5 o1)   and
                    4 o1 /= abs(-5 o1)   and
                    4 o1 <= 5 o1      and
                    4 o1 <= abs(5 o1)   and
                    4 o1 <= abs(-5 o1)   and
                    5 o1 <= abs(-5 o1)   and
                    4 o1 < 5 o1      and
                    4 o1 < abs(5 o1)   and
                    4 o1 < abs(-5 o1)   and
                    6 o1 >= 5 o1      and
                    6 o1 >= abs(5 o1)   and
                    6 o1 >= abs(-5 o1)   and
                    5 o1 >= abs(-5 o1)   and
                    6 o1 > 5 o1      and
                    6 o1 > abs(5 o1)   and
                    6 o1 > abs(-5 o1)   and
                    
--relation operators with variables
                    
                    om1 = om1      and
                    om2 = abs(om2)      and
                    om2 = abs(-om2)   and
                    om1 /= om2      and
                    om1 /= abs(om2)   and
                    om1 /= abs(-om2)   and
                    om1 <= om2      and
                    om1 <= abs(om2)   and
                    om1 <= abs(-om2)   and
                    om2 <= abs(-om2)   and
                    om1 < om2      and
                    om1 < abs(om2)      and
                    om1 < abs(-om2)   and
                    om2 >= om1      and
                    om2 >= abs(om1)   and
                    om2 >= abs(-om1)   and
                    om2 >= abs(-om1)   and
                    om2 > om1      and
                    om2 > abs(om1)      and
                    om2 > abs(-om1)   )
      report "***PASSED TEST: c07s02b02x00p02n01i01986"
      severity NOTE;
    assert (     5 o1 = 5 o1      and
                 5 o1 = abs(5 o1)   and
                 5 o1 = abs(-5 o1)   and
                 4 o1 /= 5 o1      and
                 4 o1 /= abs(5 o1)   and
                 4 o1 /= abs(-5 o1)   and
                 4 o1 <= 5 o1      and
                 4 o1 <= abs(5 o1)   and
                 4 o1 <= abs(-5 o1)   and
                 5 o1 <= abs(-5 o1)   and
                 4 o1 < 5 o1      and
                 4 o1 < abs(5 o1)   and
                 4 o1 < abs(-5 o1)   and
                 6 o1 >= 5 o1      and
                 6 o1 >= abs(5 o1)   and
                 6 o1 >= abs(-5 o1)   and
                 5 o1 >= abs(-5 o1)   and
                 6 o1 > 5 o1      and
                 6 o1 > abs(5 o1)   and
                 6 o1 > abs(-5 o1)   and
                 
--relation operators with variables
                 
                 om1 = om1      and
                 om2 = abs(om2)      and
                 om2 = abs(-om2)   and
                 om1 /= om2      and
                 om1 /= abs(om2)   and
                 om1 /= abs(-om2)   and
                 om1 <= om2      and
                 om1 <= abs(om2)   and
                 om1 <= abs(-om2)   and
                 om2 <= abs(-om2)   and
                 om1 < om2      and
                 om1 < abs(om2)      and
                 om1 < abs(-om2)   and
                 om2 >= om1      and
                 om2 >= abs(om1)   and
                 om2 >= abs(-om1)   and
                 om2 >= abs(-om1)   and
                 om2 > om1      and
                 om2 > abs(om1)      and
                 om2 > abs(-om1)   )
      report "***FAILED TEST: c07s02b02x00p02n01i01986 - Relational operators truth table test for data type of Physical failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b02x00p02n01i01986arch;
