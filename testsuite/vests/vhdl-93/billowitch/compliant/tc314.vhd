
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
-- $Id: tc314.vhd,v 1.2 2001-10-26 16:29:52 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s01b04x01p01n03i00314ent IS
END c03s01b04x01p01n03i00314ent;

ARCHITECTURE c03s01b04x01p01n03i00314arch OF c03s01b04x01p01n03i00314ent IS
  subtype T1 is REAL range 1.0 to 10.0 ;
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT(T1'LEFT < T1'RIGHT)
      report "***PASSED TEST: c03s01b04x01p01n03i00314"
      severity NOTE;
    assert (T1'LEFT < T1'RIGHT)
      report "***FAILED TEST: c03s01b04x01p01n03i00314 - The range of REAL is defined with an ascending range."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s01b04x01p01n03i00314arch;
