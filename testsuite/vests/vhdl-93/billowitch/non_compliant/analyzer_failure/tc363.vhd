
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
-- $Id: tc363.vhd,v 1.2 2001-10-26 16:30:26 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x01p03n01i00363ent IS
END c03s02b01x01p03n01i00363ent;

ARCHITECTURE c03s02b01x01p03n01i00363arch OF c03s02b01x01p03n01i00363ent IS
  type week    is array (positive range <>) of integer;
  type a    is access week;
  subtype weekend1 is week (10 to 20);
  subtype weekend2 is a (10 to 20);
  type week2    is array (1 to 10) of integer;
  type b    is access week2;
  subtype weekend3 is week2 (1 to 2); -- Failure_here
  subtype weekend4 is b (1 to 2);     -- Failure_here
BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c03s02b01x01p03n01i00363 - Index constraint not allowed in the subtype declaration of weekend3."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p03n01i00363arch;
