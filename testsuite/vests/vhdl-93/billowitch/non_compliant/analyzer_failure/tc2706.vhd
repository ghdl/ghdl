
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
-- $Id: tc2706.vhd,v 1.2 2001-10-26 16:30:21 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c13s04b01x00p06n02i02706ent IS
END c13s04b01x00p06n02i02706ent;

ARCHITECTURE c13s04b01x00p06n02i02706arch OF c13s04b01x00p06n02i02706ent IS

BEGIN
  TESTING: PROCESS
    variable total_time : integer;
  BEGIN
    total_time := 123234e-3;    -- failure_here
    assert FALSE
      report "***FAILED TEST: c13s04b01x00p06n02i02706 - An integer literal may not have a negative exponent."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c13s04b01x00p06n02i02706arch;
