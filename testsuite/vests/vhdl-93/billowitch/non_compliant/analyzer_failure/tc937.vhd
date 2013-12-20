
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
-- $Id: tc937.vhd,v 1.2 2001-10-26 16:30:28 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c10s04b00x00p06n01i00937pkg_a is
  type MC is (LOW,HIGH,RISING);
end c10s04b00x00p06n01i00937pkg_a;

package c10s04b00x00p06n01i00937pkg is
  function MC return boolean;
end c10s04b00x00p06n01i00937pkg;

package body c10s04b00x00p06n01i00937pkg is
  function MC return boolean is
  begin
    return false;
  end;
end c10s04b00x00p06n01i00937pkg;

use work.c10s04b00x00p06n01i00937pkg_a.all,work.c10s04b00x00p06n01i00937pkg.all;
ENTITY c10s04b00x00p06n01i00937ent IS
END c10s04b00x00p06n01i00937ent;

ARCHITECTURE c10s04b00x00p06n01i00937arch OF c10s04b00x00p06n01i00937ent IS

BEGIN
  TESTING : PROCESS
    variable S1: MC;     -- Failure_here.
  BEGIN
    S1 := Low;
    assert FALSE 
      report "***FAILED TEST: c10s04b00x00p06n01i00937 - Ambiguity in usage of potentially visible declarations." 
      severity ERROR;
    wait;
  END PROCESS;

END c10s04b00x00p06n01i00937arch;
