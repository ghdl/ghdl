
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
-- $Id: tc2987.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c02s05b00x00p06n01i02987pkg is
  constant wtime: time := 5 ns;
end c02s05b00x00p06n01i02987pkg;

ENTITY c02s05b00x00p06n01i02987ent IS
END c02s05b00x00p06n01i02987ent;

ARCHITECTURE c02s05b00x00p06n01i02987arch OF c02s05b00x00p06n01i02987ent IS
  use work.c02s05b00x00p06n01i02987pkg.wtime;
BEGIN
  TESTING: PROCESS
    variable TimeCount : time := 0 ns;
  BEGIN
    TimeCount   := NOW;
    wait for wtime;
    TimeCount   := NOW - TimeCount;   
    assert NOT( TimeCount = 5 ns )
      report "***PASSED TEST: c02s05b00x00p06n01i02987"
      severity NOTE;
    assert ( TimeCount = 5 ns )
      report "***FAILED TEST: c02s05b00x00p06n01i02987 - Package declaration visibility test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s05b00x00p06n01i02987arch;
