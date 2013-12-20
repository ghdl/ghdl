
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
-- $Id: tc1356.vhd,v 1.2 2001-10-26 16:29:40 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s05b00x00p03n01i01356ent IS
END c08s05b00x00p03n01i01356ent;

ARCHITECTURE c08s05b00x00p03n01i01356arch OF c08s05b00x00p03n01i01356ent IS

BEGIN
  TESTING: PROCESS
    type t1 is record
                 ele1 : integer;
                 ele2 : real;
               end record;
    variable f1: t1;
    variable i : integer := 0;
    variable r : real    := 0.0;
  BEGIN
    f1.ele1 := 1;
    f1.ele2 := 2.3;
    i := f1.ele1;
    r := f1.ele2;
    assert NOT((i=1) and (r=2.3)) 
      report "***PASSED TEST: c08s05b00x00p03n01i01356" 
      severity NOTE;
    assert ((i=1) and (r=2.3)) 
      report "***FAILED TEST: c08s05b00x00p03n01i01356 - Target and the expression on the right-hand side should have the same type." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s05b00x00p03n01i01356arch;
