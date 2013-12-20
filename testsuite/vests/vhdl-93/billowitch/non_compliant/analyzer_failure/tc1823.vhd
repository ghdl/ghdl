
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
-- $Id: tc1823.vhd,v 1.2 2001-10-26 16:30:13 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s01b00x00p08n01i01823ent IS
  type    small_int    is range 0 to 7;
  type    cmd_bus    is array (small_int) of small_int;
END c07s01b00x00p08n01i01823ent;

ARCHITECTURE c07s01b00x00p08n01i01823arch OF c07s01b00x00p08n01i01823ent IS
  signal s_bus : cmd_bus;
BEGIN
  TESTING : PROCESS
  BEGIN
    s_bus(0) <= small_int(small_int) after 5 ns; -- type name illegal here
    wait for 5 ns;
    assert FALSE
      report "***FAILED TEST: c07s01b00x00p08n01i01823 - Type names are not permitted as primaries in a type conversion expression."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s01b00x00p08n01i01823arch;
