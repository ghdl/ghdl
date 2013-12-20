
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
-- $Id: tc92.vhd,v 1.2 2001-10-26 16:30:28 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x00p03n01i00092ent IS
END c04s03b02x00p03n01i00092ent;

ARCHITECTURE c04s03b02x00p03n01i00092arch OF c04s03b02x00p03n01i00092ent IS

BEGIN
  TESTING: PROCESS
    function exp_type_check (constant x : out integer := 3) -- Failure_here
      return integer is
    begin
      return x;
    end;
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c04s03b02x00p03n01i00092- A constant of mode out cannot be declared in a constant interface declaration."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x00p03n01i00092arch;
