
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
-- $Id: tc310.vhd,v 1.2 2001-10-26 16:30:25 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s01b04x00p06n01i00310ent IS
END c03s01b04x00p06n01i00310ent;

ARCHITECTURE c03s01b04x00p06n01i00310arch OF c03s01b04x00p06n01i00310ent IS
  type R1 is range -10.0 to 10.0;
  constant C1 : R1 := 2.0 ;
  signal S1 : R1;
BEGIN
  TESTING: PROCESS
  BEGIN
    S1 <= C1 * 6.0 after 5 ns;
    wait for 10 ns;
    assert NOT(S1 = 12.0)
      report "***PASSED TEST: c03s01b04x00p06n01i00310"
      severity NOTE;
    assert ( S1=12.0)
      report "***FAILED TEST: c03s01b04x00p06n01i00310 - Value not within bounds." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s01b04x00p06n01i00310arch;
