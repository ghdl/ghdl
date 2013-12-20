
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
-- $Id: tc1768.vhd,v 1.2 2001-10-26 16:30:12 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s05b02x00p11n01i01768ent IS
END c09s05b02x00p11n01i01768ent;

ARCHITECTURE c09s05b02x00p11n01i01768arch OF c09s05b02x00p11n01i01768ent IS
  signal   i,j   : real := 1.0;
BEGIN

  with i select     -- Failure_here
            j <= transport 1.0    when 1.0,
                 0.2    when 0.2,
                 1.0e24 when 0.0,
                 0.0    when others;

  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c09s05b02x00p11n01i01768 - Select expression in a selected assignment statement should be a discrete type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c09s05b02x00p11n01i01768arch;
