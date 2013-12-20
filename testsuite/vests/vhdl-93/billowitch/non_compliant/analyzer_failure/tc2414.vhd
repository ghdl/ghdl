
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
-- $Id: tc2414.vhd,v 1.2 2001-10-26 16:30:18 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x00p10n01i02414ent IS
END c07s03b02x00p10n01i02414ent;

ARCHITECTURE c07s03b02x00p10n01i02414arch OF c07s03b02x00p10n01i02414ent IS
  type s27 is array (1 to 4) of integer;
BEGIN
  TESTING: PROCESS
    variable V1 : s27 := (1, 2, 3, 4);
  BEGIN
    (v1(1) , v1(2)) := (v1(3), v1(4)); -- Failure_here
                                           -- type of aggregate not
                                           -- determinable from context
    assert FALSE 
      report "***FAILED TEST: c07s03b02x00p10n01i02414 - Type of the aggregate must be determinable from the context."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x00p10n01i02414arch;
