
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
-- $Id: tc1028.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s04b00x00p02n01i01028ent IS
  type THREE is range 1 to 3;
  type A1 is array (THREE) of BOOLEAN;
  type A2 is array (THREE, THREE) of BOOLEAN;
  type A3 is array (THREE) of A1;
  
  type R1 is record
               RE1: A1;
             end record;
  
  type R2 is record
               RE2: A2;
             end record;
  
  type R3 is record
               RE3: A3;
             end record;
END c06s04b00x00p02n01i01028ent;

ARCHITECTURE c06s04b00x00p02n01i01028arch OF c06s04b00x00p02n01i01028ent IS

BEGIN
  TESTING: PROCESS
    variable V: BOOLEAN;
    variable V1: R1 ; -- := (RE1=>(others=>TRUE));
    variable V2: R2 ; -- := (RE2=>(others=>(others=>TRUE)));  
    variable V3: R3 ; -- := (RE3=>(others=>(others=>TRUE)));
  BEGIN
    V := V1.RE1(1);
    assert NOT( V=false )
      report "***PASSED TEST: c06s04b00x00p02n01i01028"
      severity NOTE;
    assert ( V=false )
      report "***FAILED TEST: c06s04b00x00p02n01i01028 - The prefix of an indexed name can be a selected name."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s04b00x00p02n01i01028arch;
