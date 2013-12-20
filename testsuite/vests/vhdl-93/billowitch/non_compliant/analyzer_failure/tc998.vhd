
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
-- $Id: tc998.vhd,v 1.2 2001-10-26 16:30:29 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c06s03b00x00p09n01i00998pkg is
  type TWO is range 1 to 2;
end c06s03b00x00p09n01i00998pkg;

use work.c06s03b00x00p09n01i00998pkg.all;
ENTITY c06s03b00x00p09n01i00998ent IS
END c06s03b00x00p09n01i00998ent;

ARCHITECTURE c06s03b00x00p09n01i00998arch OF c06s03b00x00p09n01i00998ent IS

BEGIN
  TESTING: PROCESS
    subtype ST2 is c06s03b00x00p09n01i00998ent.TWO (1 to 1);
    -- SEMANTIC ERROR: ILLEGAL EXPANDED NAME
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c06s03b00x00p09n01i00998 - Expanded name is illegal."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s03b00x00p09n01i00998arch;
