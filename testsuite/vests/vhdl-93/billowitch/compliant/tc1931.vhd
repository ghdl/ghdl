
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
-- $Id: tc1931.vhd,v 1.2 2001-10-26 16:29:44 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b01x00p01n02i01931ent IS
END c07s02b01x00p01n02i01931ent;

ARCHITECTURE c07s02b01x00p01n02i01931arch OF c07s02b01x00p01n02i01931ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
--
--          Test operators on one-dimesioned arrays of BIT
--
    ASSERT ( B"1100" AND  B"1010" ) = B"1000" 
      REPORT "ERROR: composite AND operator failed : BIT"
      SEVERITY FAILURE;
    ASSERT ( B"1100"  OR  B"1010" ) = B"1110" 
      REPORT "ERROR: composite  OR operator failed : BIT"
      SEVERITY FAILURE;
    ASSERT ( B"1100" NAND B"1010" ) = B"0111" 
      REPORT "ERROR: composite NAND operator failed : BIT"
      SEVERITY FAILURE;
    ASSERT ( B"1100" NOR  B"1010" ) = B"0001" 
      REPORT "ERROR: composite NOR operator failed : BIT"
      SEVERITY FAILURE;
    ASSERT ( B"1100" XOR  B"1010" ) = B"0110" 
      REPORT "ERROR: composite XOR operator failed : BIT"
      SEVERITY FAILURE;
    ASSERT (         NOT  B"1100" ) = B"0011" 
      REPORT "ERROR: composite NOT operator failed : BIT"
      SEVERITY FAILURE;
    wait for 5 ns;
    assert NOT(   (( B"1100" AND  B"1010" ) = B"1000")   and
                  (( B"1100"  OR  B"1010" ) = B"1110")   and
                  (( B"1100" NAND B"1010" ) = B"0111")   and
                  (( B"1100" NOR  B"1010" ) = B"0001")   and
                  (( B"1100" XOR  B"1010" ) = B"0110")   and
                  ((         NOT  B"1100" ) = B"0011")   )
      report "***PASSED TEST: c07s02b01x00p01n02i01931"
      severity NOTE;
    assert (   (( B"1100" AND  B"1010" ) = B"1000")   and
               (( B"1100"  OR  B"1010" ) = B"1110")   and
               (( B"1100" NAND B"1010" ) = B"0111")   and
               (( B"1100" NOR  B"1010" ) = B"0001")   and
               (( B"1100" XOR  B"1010" ) = B"0110")   and
               ((         NOT  B"1100" ) = B"0011")   )
      report "***FAILED TEST: c07s02b01x00p01n02i01931 - Logical operators should be valid for any one-dimensional array type whose element type is BIT."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b01x00p01n02i01931arch;
