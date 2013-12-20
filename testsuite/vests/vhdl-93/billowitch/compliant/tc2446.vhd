
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
-- $Id: tc2446.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x02p02n01i02446ent IS
END c07s03b02x02p02n01i02446ent;

ARCHITECTURE c07s03b02x02p02n01i02446arch OF c07s03b02x02p02n01i02446ent IS

BEGIN
  TESTING: PROCESS
    type T1 is array (1 to 10) of Integer;
    constant C : T1 := (1 => 2, 2=>4, 3 =>6, 4 =>5, others =>10) ; -- No_failure_here
  BEGIN
    assert NOT(C(1)=2 and C(2)=4 and C(3)=6 and C(4)=5 and C(5)=10 and C(6)=10) 
      report "***PASSED TEST: c07s03b02x02p02n01i02446" 
      severity NOTE;
    assert (C(1)=2 and C(2)=4 and C(3)=6 and C(4)=5 and C(5)=10 and C(6)=10) 
      report "***FAILED TEST: c07s03b02x02p02n01i02446 - All element associations of an array aggregate must be either all positional or all named."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x02p02n01i02446arch;
