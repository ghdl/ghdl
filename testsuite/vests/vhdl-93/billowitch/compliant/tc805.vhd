
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
-- $Id: tc805.vhd,v 1.2 2001-10-26 16:30:00 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c01s01b03x00p03n01i00805ent IS
  port (CLK: in bit);
begin
  assert FALSE
    report "The test of concurrent assertion statement in entity statement passed when you see this asssertion note."
    severity note;
  process
  begin
    wait;
  end process;
END c01s01b03x00p03n01i00805ent;

ARCHITECTURE c01s01b03x00p03n01i00805arch OF c01s01b03x00p03n01i00805ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***PASSED TEST: c01s01b03x00p03n01i00805 - This test needs manual check to make sure that assertion notice appear."
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c01s01b03x00p03n01i00805arch;
