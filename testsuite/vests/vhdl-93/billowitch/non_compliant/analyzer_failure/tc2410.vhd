
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
-- $Id: tc2410.vhd,v 1.2 2001-10-26 16:30:18 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x00p09n01i02410ent IS
END c07s03b02x00p09n01i02410ent;

ARCHITECTURE c07s03b02x00p09n01i02410arch OF c07s03b02x00p09n01i02410ent IS
  type     array_three is array (1 to 6) of integer;
  constant x : array_three := (1, 2, 3, 4, 5, 6);
  constant y : array_three := (1 => 1, 2 => 2, 2 => 3,others => 0);
  -- Failure_here
BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c07s03b02x00p09n01i02410 - An element of the value defined by an aggregate can be represented only once in an aggregate."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x00p09n01i02410arch;
