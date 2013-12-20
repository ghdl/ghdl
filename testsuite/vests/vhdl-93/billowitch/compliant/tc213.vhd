
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
-- $Id: tc213.vhd,v 1.2 2001-10-26 16:29:45 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s01b01x00p03n01i00213ent IS
END c03s01b01x00p03n01i00213ent;

ARCHITECTURE c03s01b01x00p03n01i00213arch OF c03s01b01x00p03n01i00213ent IS
  type pqr is (foo, swath, a, 'a');  -- No_failure_here
  signal dude1 : pqr := foo;
  signal dude2 : pqr;      
  signal dude3 : pqr := a;
  signal dude4 : pqr := 'a';
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT(dude1 = foo and dude2 = foo and dude3 = a and dude4 = 'a')
      report "***PASSED TEST: c03s01b01x00p03n01i00213"
      severity NOTE;
    assert (dude1 = foo and dude2 = foo and dude3 = a and dude4 = 'a' )
      report "***FAILED TEST: c03s01b01x00p03n01i00213 - When an enumeration type is being declared, that both identifiers and literals may be contained in the list of elements."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s01b01x00p03n01i00213arch;
