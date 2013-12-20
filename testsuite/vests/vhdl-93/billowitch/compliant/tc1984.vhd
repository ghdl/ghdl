
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
-- $Id: tc1984.vhd,v 1.2 2001-10-26 16:29:44 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b02x00p02n01i01984ent IS
END c07s02b02x00p02n01i01984ent;

ARCHITECTURE c07s02b02x00p02n01i01984arch OF c07s02b02x00p02n01i01984ent IS

BEGIN
  TESTING: PROCESS
    variable B1 : boolean := true;
    variable B2 : boolean := false;
    variable A1 : bit := '1';
    variable A2 : bit := '0';
  BEGIN

    assert NOT(     A1 = '1'   and
                    '1' = A1   and
                    B2 = false   and
                    false = B2   and
                    A1 /= A2   and
                    B1 /= B2   and
                    A2 < A1   and
                    B2 < B1   and
                    A1 > A2   and
                    B1 > B2   and
                    A2 <= A1   and
                    B2 <= B1   and
                    A1 >= A2   and
                    B1 >= B2   and
                    A1 <= A1   and
                    B1 <= B1   and
                    B2 <= B2   and
                    A2 <= A2   )   
      report "***PASSED TEST: c07s02b02x00p02n01i01984"
      severity NOTE;
    assert (     A1 = '1'   and
                 '1' = A1   and
                 B2 = false   and
                 false = B2   and
                 A1 /= A2   and
                 B1 /= B2   and
                 A2 < A1   and
                 B2 < B1   and
                 A1 > A2   and
                 B1 > B2   and
                 A2 <= A1   and
                 B2 <= B1   and
                 A1 >= A2   and
                 B1 >= B2   and
                 A1 <= A1   and
                 B1 <= B1   and
                 B2 <= B2   and
                 A2 <= A2   )   
      report "***FAILED TEST: c07s02b02x00p02n01i01984 - Relational operators true table test for data type of BIT and BOOLEAN failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b02x00p02n01i01984arch;
