
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
-- $Id: tc1318.vhd,v 1.2 2001-10-26 16:29:40 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s04b00x00p07n01i01318ent IS
END c08s04b00x00p07n01i01318ent;

ARCHITECTURE c08s04b00x00p07n01i01318arch OF c08s04b00x00p07n01i01318ent IS
  signal s1, s2 : CHARACTER := NUL;
BEGIN
  TESTING: PROCESS
    type       AT is array (INTEGER range <>) of CHARACTER;
    variable    av : AT(0 to 1) := ('1', '2');
  BEGIN
    assert s1 = NUL;
    assert s2 = NUL;
    (s1, s2) <= av;
    wait on s1;
    assert s1 = '1';
    assert s2 = '2'; 
    assert NOT( s1 = '1' and s2 = '2' )
      report "***PASSED TEST: c08s04b00x00p07n01i01318" 
      severity NOTE;
    assert ( s1 = '1' and s2 = '2' )
      report "***FAILED TEST: c08s04b00x00p07n01i01318 - Aggregate (array type) signal assignment test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s04b00x00p07n01i01318arch;
