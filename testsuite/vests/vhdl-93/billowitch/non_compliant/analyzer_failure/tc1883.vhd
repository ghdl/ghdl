
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
-- $Id: tc1883.vhd,v 1.2 2001-10-26 16:30:14 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s01b00x00p08n01i01883ent IS
END c07s01b00x00p08n01i01883ent;

ARCHITECTURE c07s01b00x00p08n01i01883arch OF c07s01b00x00p08n01i01883ent IS
  type    small_int    is range 0 to 7;
  type    cmd_bus    is array (small_int range <>) of small_int;
  signal    obus      : cmd_bus(small_int);
BEGIN
  TESTING : PROCESS
  BEGIN
    obus <= (0 =>c07s01b00x00p08n01i01883arch, others => 5) after 5 ns; 
    -- architecture body name illegal here
    wait for 5 ns;
    assert FALSE
      report "***FAILED TEST: c07s01b00x00p08n01i01883 - Architecture body names are not permitted as primaries in a element association expression."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s01b00x00p08n01i01883arch;
