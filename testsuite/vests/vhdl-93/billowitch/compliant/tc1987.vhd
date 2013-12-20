
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
-- $Id: tc1987.vhd,v 1.2 2001-10-26 16:29:44 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b02x00p02n01i01987ent IS
END c07s02b02x00p02n01i01987ent;

ARCHITECTURE c07s02b02x00p02n01i01987arch OF c07s02b02x00p02n01i01987ent IS

BEGIN
  TESTING: PROCESS
    variable r1, r2, r3, r4 : real;
  BEGIN

    r1 := 69.0;
    r2 := 50.0;
    r3 := (-69.0);
    r4 := (-50.0);

    assert NOT(     real'high > real'low   and
                    real'high >= real'low   and
                    real'high > 0.0   and
                    real'high >= 0.0   and
                    real'low < 0.0      and
                    real'low <= 0.0   and
                    real'high /= real'low   and

                    r1 > r2   and
                    r1 >= r2   and
                    r1 > 0.0   and
                    r1 /= r2   and
                    r2 < r1   and
                    r2 <= r1   and
                    
                    r4 > r3   and
                    r4 >= r3   and
                    r4 < 0.0   and
                    r4 /= r3   and
                    r3 < r4   and
                    r3 <= r4   and
                    
                    r1 > r3   and
                    r2 >= r4   and
                    r4 < r1   and
                    r1 /= r3   and
                    r2 /= r4   and
                    r3 < r1   and
                    r4 <= r2   and
                    
                    3.14E1 > 3.10E1   and
                    5.7E-9 < 5.7E+9 )
      report "***PASSED TEST: c07s02b02x00p02n01i01987"
      severity NOTE;
    assert (     real'high > real'low   and
                 real'high >= real'low   and
                 real'high > 0.0   and
                 real'high >= 0.0   and
                 real'low < 0.0      and
                 real'low <= 0.0   and
                 real'high /= real'low   and

                 r1 > r2   and
                 r1 >= r2   and
                 r1 > 0.0   and
                 r1 /= r2   and
                 r2 < r1   and
                 r2 <= r1   and
                 
                 r4 > r3   and
                 r4 >= r3   and
                 r4 < 0.0   and
                 r4 /= r3   and
                 r3 < r4   and
                 r3 <= r4   and
                 
                 r1 > r3   and
                 r2 >= r4   and
                 r4 < r1   and
                 r1 /= r3   and
                 r2 /= r4   and
                 r3 < r1   and
                 r4 <= r2   and
                 
                 3.14E1 > 3.10E1   and
                 5.7E-9 < 5.7E+9 )
      report "***FAILED TEST: c07s02b02x00p02n01i01987 - Relational operators truth table test for data type of Real failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b02x00p02n01i01987arch;
