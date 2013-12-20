
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
-- $Id: tc1770.vhd,v 1.2 2001-10-26 16:30:12 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s05b02x00p11n01i01770ent IS
END c09s05b02x00p11n01i01770ent;

ARCHITECTURE c09s05b02x00p11n01i01770arch OF c09s05b02x00p11n01i01770ent IS
  type string4 is array( 1 to 4 ) of CHARACTER;
  signal x : string4;
  signal y : integer;
BEGIN

  with x select
    y <= transport  1 when "one",   -- Failure_here
    2 when "two",   -- Failure_here
    0 when others;

  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c09s05b02x00p11n01i01770 - Select expression in a selected assignment statement is not the same type of a choice." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c09s05b02x00p11n01i01770arch;
