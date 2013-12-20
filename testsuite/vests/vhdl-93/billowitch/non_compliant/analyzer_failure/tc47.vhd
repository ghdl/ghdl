
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
-- $Id: tc47.vhd,v 1.2 2001-10-26 16:30:26 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b01x01p03n02i00047ent IS
END c04s03b01x01p03n02i00047ent;

ARCHITECTURE c04s03b01x01p03n02i00047arch OF c04s03b01x01p03n02i00047ent IS
  function retrieve (VM:integer) return integer is
    constant pi : real := 3.142;
  begin
    pi := 45.00;    -- Failure_here - pi is a constant
    return 12;
  end retrieve;
BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c04s03b01x01p03n02i00047- The value of a constant cannot be changed after the declartion elaboration."
      severity ERROR;
    wait;
  END PROCESS TESTING;

  ENDc04s03b01x01p03n02i00047arch;
