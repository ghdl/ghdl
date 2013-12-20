
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
-- $Id: tc2418.vhd,v 1.2 2001-10-26 16:30:18 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c07s03b02x00p10n01i02418pkg is
  type byte is range 0 to 15;
  type cmd_bus is array (0 to 3) of byte;
end c07s03b02x00p10n01i02418pkg;

use work.c07s03b02x00p10n01i02418pkg.all;
ENTITY c07s03b02x00p10n01i02418ent IS
  port ( signal b_inp : in boolean := (0 to 3 => 0) = (0 to 3 => 1));
END c07s03b02x00p10n01i02418ent;

ARCHITECTURE c07s03b02x00p10n01i02418arch OF c07s03b02x00p10n01i02418ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE
      report "***FAILED TEST: c07s03b02x00p10n01i02418 - The type of the aggregate is not determinable from the context."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x00p10n01i02418
