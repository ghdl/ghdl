
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
-- $Id: tc3025.vhd,v 1.2 2001-10-26 16:30:25 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c11s04b00x00p07n03i03025pkg_p is
end c11s04b00x00p07n03i03025p;

use work.c11s04b00x00p07n03i03025pkg_p.all;
package c11s04b00x00p07n03i03025pkg_pp is
end c11s04b00x00p07n03i03025pkg_pp;

use work.c11s04b00x00p07n03i03025pkg_pp.all;
package c11s04b00x00p07n03i03025pkg_ppp is
end c11s04b00x00p07n03i03025pkg_ppp;


package c11s04b00x00p07n03i03025pkg_p is
end c11s04b00x00p07n03i03025pkg_p;

use work.c11s04b00x00p07n03i03025pkg_pp.all;   -- Failure_here
package c11s04b00x00p07n03i03025pkg_ppp is
end c11s04b00x00p07n03i03025pkg_ppp;

ENTITY c11s04b00x00p07n03i03025ent IS
END c11s04b00x00p07n03i03025ent;

ARCHITECTURE c11s04b00x00p07n03i03025arch OF c11s04b00x00p07n03i03025ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE
      report "***FAILED TEST: c11s04b00x00p07n03i03025 - Package ch1104_p00703_01_pkg_pp has been changed since last analysis."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c11s04b00x00p07n03i03025arch;
