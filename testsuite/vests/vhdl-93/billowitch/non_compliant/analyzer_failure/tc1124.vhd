
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
-- $Id: tc1124.vhd,v 1.2 2001-10-26 16:30:06 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s05b00x00p04n01i01124ent IS
END c06s05b00x00p04n01i01124ent;

ARCHITECTURE c06s05b00x00p04n01i01124arch OF c06s05b00x00p04n01i01124ent IS

BEGIN
  TESTING: PROCESS
    type     BIT_VECTOR is array (positive range <>) of BIT;
    variable NUM1 : BIT_VECTOR(1 to 10) := B"01_01_01_01_01";
  BEGIN
    NUM1(7 to 12) := B"010_010";
    assert FALSE 
      report "***FAILED TEST: c06s05b00x00p04n01i01124 - Bounds of the slice cannot exceed those defined by the discrete range."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s05b00x00p04n01i01124arch;
