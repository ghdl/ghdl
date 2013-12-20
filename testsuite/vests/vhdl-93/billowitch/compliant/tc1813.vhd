
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
-- $Id: tc1813.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s01b00x00p08n01i01813ent IS
END c07s01b00x00p08n01i01813ent;

ARCHITECTURE c07s01b00x00p08n01i01813arch OF c07s01b00x00p08n01i01813ent IS
  type A1  is array (1 to 5) of integer;
  type Acc is access A1;
BEGIN
  TESTING: PROCESS
    variable V1 : Acc := new A1'(1,2,3,4,5);
    variable V2 : integer;
    variable V3 : integer;
    variable V4 : integer;
    variable V5 : integer;
    variable V6 : integer;
  BEGIN
    V2 := V1(1); -- No_failure_here
    V3 := V1(2); -- No_failure_here
    V4 := V1(3); -- No_failure_here
    V5 := V1(4); -- No_failure_here
    V6 := V1(5); -- No_failure_here
    assert NOT(V2=1 and V3=2 and V4=3 and V5=4 and V6=5)
      report "***PASSED TEST: c07s01b00x00p08n01i01813"
      severity NOTE;
    assert (V2=1 and V3=2 and V4=3 and V5=4 and V6=5)
      report "***FAILED TEST: c07s01b00x00p08n01i01813 - The primary must be a name, a literal, an aggregate, a function call, a qualified expression, a type conversion, an allocator, or an expression enclosed within parentheses."  
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s01b00x00p08n01i01813arch;
