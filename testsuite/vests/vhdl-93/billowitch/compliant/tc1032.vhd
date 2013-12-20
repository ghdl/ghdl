
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
-- $Id: tc1032.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s04b00x00p02n01i01032ent IS
END c06s04b00x00p02n01i01032ent;

ARCHITECTURE c06s04b00x00p02n01i01032arch OF c06s04b00x00p02n01i01032ent IS

BEGIN
  TESTING: PROCESS
    type TWO is range 1 to 2;
    
    type A0 is array (TWO) of BOOLEAN;
    type A1 is array (TWO) of A0;
    type A2 is array (TWO) of A1;
    type A3 is array (TWO) of A2;
    type A4 is array (TWO) of A3;
    type A5 is array (TWO) of A4;
    type A6 is array (TWO) of A5;
    type A7 is array (TWO) of A6;
    type A8 is array (TWO) of A7;
    type A9 is array (TWO) of A8;
    
    variable V1: A9;
  BEGIN
    V1(1)(2)(1)(2)(1)(2)(1)(2)(1)(2) := TRUE; 
    assert NOT(V1(1)(2)(1)(2)(1)(2)(1)(2)(1)(2) = TRUE) 
      report "***PASSED TEST: c06s04b00x00p02n01i01032"
      severity NOTE;
    assert (V1(1)(2)(1)(2)(1)(2)(1)(2)(1)(2) = TRUE) 
      report "***FAILED TEST: c06s04b00x00p02n01i01032 - The prefix of an indexed name can be a indexed name."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s04b00x00p02n01i01032arch;
