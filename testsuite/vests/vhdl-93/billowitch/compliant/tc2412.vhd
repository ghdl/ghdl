
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
-- $Id: tc2412.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x00p09n01i02412ent IS
END c07s03b02x00p09n01i02412ent;

ARCHITECTURE c07s03b02x00p09n01i02412arch OF c07s03b02x00p09n01i02412ent IS
  type T1 is array (1 to 5) of integer;
  constant C : T1 := (1 => 0, 2 => 2, 3 => 3, 4 =>4, others=> 4) ; -- No_Failure_here
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT(C(1)=0 and C(2)=2 and C(3)=3 and C(4)=4 and C(5)=4) 
      report "***PASSED TEST: c07s03b02x00p09n01i02412" 
      severity NOTE;
    assert (C(1)=0 and C(2)=2 and C(3)=3 and C(4)=4 and C(5)=4) 
      report "***FAILED TEST: c07s03b02x00p09n01i02412 - Each element of the value defined by an aggregate must be represented once and only once in the aggregate." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x00p09n01i02412arch;
