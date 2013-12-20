
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
-- $Id: tc900.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c10s03b00x00p04n01i00900pkg_1 is
  type MVL1 is (LOW,HIGH,RISING);
  type MVL2 is (LOW,HIGH,RISING,FALLING,AMBIGUOUS);
end c10s03b00x00p04n01i00900pkg_1;

use work.c10s03b00x00p04n01i00900pkg_1.all;
ENTITY c10s03b00x00p04n01i00900ent IS
END c10s03b00x00p04n01i00900ent;

ARCHITECTURE c10s03b00x00p04n01i00900arch OF c10s03b00x00p04n01i00900ent IS
  signal S1 : MVL2;
  signal S2 : MVL2;
  signal S3 : MVL2;
BEGIN
  TESTING: PROCESS
  BEGIN
    S1 <= LOW;           -- No_failure_here
    S2 <= HIGH;        -- No_failure_here
    S3 <= RISING;        -- No_failure_here
    wait for 5 ns;
    assert NOT(S1 = LOW and S2 = HIGH and S3 = RISING)
      report "***PASSED TEST: c10s03b00x00p04n01i00900" 
      severity NOTE;
    assert (S1 = LOW and S2 = HIGH and S3 = RISING)
      report "***FAILED TEST: c10s03b00x00p04n01i00900 - The occurence of the identifier is legal if and only if exactly one visible declaration is acceptable for the overloading rules in the given context."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c10s03b00x00p04n01i00900arch;
