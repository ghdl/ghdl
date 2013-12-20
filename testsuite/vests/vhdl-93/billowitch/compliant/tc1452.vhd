
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
-- $Id: tc1452.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s07b00x00p01n01i01452ent IS
END c08s07b00x00p01n01i01452ent;

ARCHITECTURE c08s07b00x00p01n01i01452arch OF c08s07b00x00p01n01i01452ent IS

begin
  expr_check: process
    variable x : integer := 3;
    variable y : integer := 5;
    variable z : integer := 9;
    variable k : integer := 0;
  begin
    if -x + z < y + x and x * z > y -x then        -- no_failure_here
      k := 1;
    end if;
    assert (k = 1)
      report "***FAILED TEST: c08s07b00x00p01n01i01452 - expression type of IF statement wrong"
      severity ERROR;
    assert NOT(k = 1)
      report "***PASSED TEST: c08s07b00x00p01n01i01452"
      severity NOTE;
    wait;
  end process;

END c08s07b00x00p01n01i01452arch;
