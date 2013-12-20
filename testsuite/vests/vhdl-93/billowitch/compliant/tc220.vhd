
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
-- $Id: tc220.vhd,v 1.2 2001-10-26 16:29:46 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s01b01x00p06n03i00220ent IS
  type e is (EMIN,ETYP,EMAX);
END c03s01b01x00p06n03i00220ent;

ARCHITECTURE c03s01b01x00p06n03i00220arch OF c03s01b01x00p06n03i00220ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT(e'pos(ETYP) < e'pos(EMAX))
      report "***PASSED TEST: c03s01b01x00p06n03i00220"
      severity NOTE;
    assert (e'pos(ETYP) < e'pos(EMAX))
      report "***FAILED TEST: c03s01b01x00p06n03i00220 - The position number of the value of each additional enumeration literal is one more than that of its predecessor in the list." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s01b01x00p06n03i00220arch;
