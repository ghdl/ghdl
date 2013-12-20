
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
-- $Id: tc2533.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b05x00p10n01i02533ent IS
END c07s03b05x00p10n01i02533ent;

ARCHITECTURE c07s03b05x00p10n01i02533arch OF c07s03b05x00p10n01i02533ent IS
  type Memory is array (Integer range <>) of Integer;
  subtype T1 is Memory (1 to 6) ;
BEGIN
  TESTING: PROCESS
    variable V1 : T1 ;
    variable V2 : T1 := (2,3,4,2,5,6) ;
  BEGIN
    V1 := Memory (V2) ;    -- No_Failure_her
    assert NOT(V1=(2,3,4,2,5,6))
      report "***PASSED TEST: c07s03b05x00p10n01i02533"
      severity NOTE;
    assert (V1=(2,3,4,2,5,6))
      report "***FAILED TEST: c07s03b05x00p10n01i02533 - The bounds of the operand are converted to the corresponding index type of the target type when the type mark is an unconstrained array type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b05x00p10n01i02533arch;
