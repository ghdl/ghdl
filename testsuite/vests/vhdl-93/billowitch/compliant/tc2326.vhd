
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
-- $Id: tc2326.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s01b00x00p07n01i02326ent IS
END c07s01b00x00p07n01i02326ent;

ARCHITECTURE c07s01b00x00p07n01i02326arch OF c07s01b00x00p07n01i02326ent IS

BEGIN
  TESTING: PROCESS
    variable r1, r2, r3, r4 : real;
  BEGIN
    
    r1 := 69.0;
    r2 := 50.0;
    r3 := (-69.0);
    r4 := (-50.0);
    wait for 5 ns; 
    assert NOT(   ( r1 = abs(r3))   and
                  ( r2 = abs(r4))   and
                  ( 50.0 = abs(-50.0))   and
                  ( (-25.0) = (-abs(-25.0)))   and
                  ( 3.14E-2 = abs(-3.14E-2))   and
                  ( (-0.379) = (-abs(-0.379)))   ) 
      report "***PASSED TEST: c07s01b00x00p07n01i02326"
      severity NOTE;
    assert (   ( r1 = abs(r3))   and
               ( r2 = abs(r4))   and
               ( 50.0 = abs(-50.0))   and
               ( (-25.0) = (-abs(-25.0)))   and
               ( 3.14E-2 = abs(-3.14E-2))   and
               ( (-0.379) = (-abs(-0.379)))   ) 
      report "***FAILED TEST: c07s01b00x00p07n01i02326 - The result of the 'abs' operation must be the absolute value of the operand."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s01b00x00p07n01i02326arch;
