
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
-- $Id: tc255.vhd,v 1.2 2001-10-26 16:30:30 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s01b02x00p07n01i00255ent IS
END c03s01b02x00p07n01i00255ent;

ARCHITECTURE c03s01b02x00p07n01i00255arch OF c03s01b02x00p07n01i00255ent IS
  subtype T1 is integer range 1 to 10;
  subtype T2 is integer range 1 to 100;
BEGIN
  TESTING: PROCESS
    variable V1 : T1;
    variable V2 : T1 := 4;
    variable V3 : T1 := 9;
  BEGIN
    V1 := V2 * V3;   -- failure_here
    assert FALSE 
      report "***FAILED TEST: c03s01b02x00p07n01i00255 - Result of mathematical operation is not of integer type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s01b02x00p07n01i00255arch;
