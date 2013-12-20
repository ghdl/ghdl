
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
-- $Id: tc3004.vhd,v 1.2 2001-10-26 16:30:24 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c02s06b00x00p06n02i03004pkg is
  constant C1 : integer := 10;
end c02s06b00x00p06n02i03004pkg;

package body c02s06b00x00p06n02i03004pkg is
  constant C2 : integer := 0;
end;

use work.c02s06b00x00p06n02i03004pkg.all;
ENTITY c02s06b00x00p06n02i03004ent IS
END c02s06b00x00p06n02i03004ent;

ARCHITECTURE c02s06b00x00p06n02i03004arch OF c02s06b00x00p06n02i03004ent IS

BEGIN
  TESTING: PROCESS
    variable A1 : integer := work.c02s06b00x00p06n02i03004pkg.C1;
    variable A2 : integer := workc02s06b00x00p06n02i03004pkg.C2; -- Failure_here
  BEGIN
    assert FALSE
      report "***FAILED TEST: c02s06b00x00p06n02i03004 - Items declared in the body of the package cannot be made visible outside the package body."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s06b00x00p06n02i03004arch;
