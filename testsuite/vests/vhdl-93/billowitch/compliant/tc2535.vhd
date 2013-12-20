
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
-- $Id: tc2535.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b05x00p13n03i02535ent IS
END c07s03b05x00p13n03i02535ent;

ARCHITECTURE c07s03b05x00p13n03i02535arch OF c07s03b05x00p13n03i02535ent IS
  type Memory is array (Integer range <>) of Integer;
  subtype T1 is Memory (1 to 6) ;
  subtype T2 is Memory (1 to 6) ;
BEGIN
  TESTING: PROCESS
    variable V1 : T1 ;
    variable V2 : T1 := (2,3,6,3,4,5) ;
  BEGIN
    V1 := T2 (V2) ;    -- No_Failure_here
    wait for 1 ns;
    assert NOT(V1 = (2,3,6,3,4,5)) 
      report "***PASSED TEST: c07s03b05x00p13n03i02535" 
      severity NOTE;
    assert (V1 = (2,3,6,3,4,5)) 
      report "***FAILED TEST: c07s03b05x00p13n03i02535 - A check is made that for each element of the operand there is a matching element of the target subtype."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b05x00p13n03i02535arch;
