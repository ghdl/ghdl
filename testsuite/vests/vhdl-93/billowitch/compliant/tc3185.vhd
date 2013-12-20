
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
-- $Id: tc3185.vhd,v 1.3 2001-10-29 02:12:44 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library std;
use std.TEXTIO.all;
ENTITY c14s03b00x00p42n01i03185ent IS
END c14s03b00x00p42n01i03185ent;

ARCHITECTURE c14s03b00x00p42n01i03185arch OF c14s03b00x00p42n01i03185ent IS

BEGIN
  TESTING: PROCESS
    file F   : TEXT open write_mode is "iofile.02";
    variable    L   : LINE;
  BEGIN
    --write out to the file
    for I in 1 to 100 loop
      WRITE       (L,integer'(1994));
      WRITELINE   (F, L);
    end loop;
    assert FALSE
      report "***PASSED TEST: c14s03b00x00p42n01i03185 - This test will write TEXT of integer type into file iofile.02."
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c14s03b00x00p42n01i03185arch;
