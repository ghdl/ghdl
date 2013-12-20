
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
-- $Id: tc3010.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c02s06b00x00p07n01i03010pkg is
  constant X1 : integer;
  constant X2 : integer;
end c02s06b00x00p07n01i03010pkg;

package body c02s06b00x00p07n01i03010pkg is
  constant X1: integer := 1;
  constant X2: integer := X1;
end c02s06b00x00p07n01i03010pkg;

use work.c02s06b00x00p07n01i03010pkg.all;
ENTITY c02s06b00x00p07n01i03010ent IS
END c02s06b00x00p07n01i03010ent;

ARCHITECTURE c02s06b00x00p07n01i03010arch OF c02s06b00x00p07n01i03010ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT(X1=1 and X2=1)
      report "***PASSED TEST: c02s06b00x00p07n01i03010" 
      severity NOTE;
    assert (X1=1 and X2=1)
      report "***FAILED TEST: c02s06b00x00p07n01i03010 - Deferred constant declaration test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s06b00x00p07n01i03010arch;
