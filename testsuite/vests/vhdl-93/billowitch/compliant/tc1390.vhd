
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
-- $Id: tc1390.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s05b00x00p04n03i01390ent IS
END c08s05b00x00p04n03i01390ent;

ARCHITECTURE c08s05b00x00p04n03i01390arch OF c08s05b00x00p04n03i01390ent IS

BEGIN
  TESTING: PROCESS
    variable A : integer := 0;
    variable B : integer := 0;
    variable C : integer := 1;
    variable D : integer := 2;
    type array_of_ints is array (Positive range <>) of integer;
  BEGIN
    (A,B) := array_of_ints'(C,D);
    assert NOT( (A=1) and (B=2) )
      report "***PASSED TEST: c08s05b00x00p04n03i01390"
      severity NOTE;
    assert ( (A=1) and (B=2) )
      report "***FAILED TEST: c08s05b00x00p04n03i01390 - Each element association of the aggregate must be a locally static name that denotes a variable"
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s05b00x00p04n03i01390arch;
