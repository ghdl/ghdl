
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
-- $Id: tc1391.vhd,v 1.2 2001-10-26 16:30:09 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s05b00x00p04n03i01391ent IS
END c08s05b00x00p04n03i01391ent;

ARCHITECTURE c08s05b00x00p04n03i01391arch OF c08s05b00x00p04n03i01391ent IS

BEGIN
  TESTING: PROCESS
    type ARR is array(0 to 1) of bit;
    variable i : integer := 1;
    variable j : integer := 2;
    variable S : BIT_VECTOR(0 to 1);
    variable T : BIT_VECTOR(0 to 2);
  BEGIN
    (S(i),T(j)) := ARR'('0','1');
    assert FALSE 
      report "***FAILED TEST: c08s05b00x00p04n03i01391 - Each element association of the aggregate must be a locally static name that denotes a variable"
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s05b00x00p04n03i01391arch;
