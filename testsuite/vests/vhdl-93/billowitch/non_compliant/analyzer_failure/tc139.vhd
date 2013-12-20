
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
-- $Id: tc139.vhd,v 1.2 2001-10-26 16:30:09 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x02p09n01i00139ent IS
END c04s03b02x02p09n01i00139ent;

ARCHITECTURE c04s03b02x02p09n01i00139arch OF c04s03b02x02p09n01i00139ent IS
  procedure P1 (p : in integer := 3; r: inout integer) is        
  begin
    r := p / 3 ;
  end;
BEGIN
  TESTING: PROCESS
    variable x : integer := 1;
  BEGIN
    P1 (x);  -- Failure_here
    -- named association missing.
    assert FALSE 
      report "***FAILED TEST: c04s03b02x02p09n01i00139 - The actual list for procedure call does not match the formal list."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x02p09n01i00139arch;
