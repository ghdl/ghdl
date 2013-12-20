
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
-- $Id: tc2374.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x00p02n01i02374ent IS
END c07s03b02x00p02n01i02374ent;

ARCHITECTURE c07s03b02x00p02n01i02374arch OF c07s03b02x00p02n01i02374ent IS

BEGIN
  TESTING: PROCESS
    type       x1 is array (1 to 2) of integer;
    constant    v1 :  x1 := (0, 0);   -- Success_here
  BEGIN
    assert NOT(v1(1)=0 and v1(2)=0)
      report "***PASSED TEST: c07s03b02x00p02n01i02374"
      severity NOTE;
    assert (v1(1)=0 and v1(2)=0)
      report "***FAILED TEST: c07s03b02x00p02n01i02374 - The aggregate consists of one or more element associations seperated with commas(,) which are enclosed with parentheses."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x00p02n01i02374arch;
