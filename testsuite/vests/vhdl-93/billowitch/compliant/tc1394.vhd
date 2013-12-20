
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
-- $Id: tc1394.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s05b00x00p04n01i01394ent IS
END c08s05b00x00p04n01i01394ent;

ARCHITECTURE c08s05b00x00p04n01i01394arch OF c08s05b00x00p04n01i01394ent IS

BEGIN
  TESTING: PROCESS
    type AT2 is array (0 to 1, 0 to 1) of CHARACTER;
    type AT1 is array (0 to 1) of CHARACTER;
    variable v1, v2    : AT1;
    variable av       : AT2 := (('a', 'b'), ('c', 'd'));
  BEGIN
    assert v1 = (NUL, NUL);
    assert v2 = (NUL, NUL);
    v1(0) := av(0,0);
    v1(1) := av(0,1);
    v2(0) := av(1,0);
    v2(1) := av(1,1);
    assert v1 = ('a', 'b');
    assert v2 = ('c', 'd');
    wait for 1 ns;
    assert NOT( v1 = ('a','b') and v2 = ('c', 'd') )
      report "***PASSED TEST: c08s05b00x00p04n01i01394"
      severity NOTE;
    assert ( v1 = ('a','b') and v2 = ('c', 'd') )
      report "***FAILED TEST: c08s05b00x00p04n01i01394 - Aggregate (2-d array type) assignment for variable test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s05b00x00p04n01i01394arch;
