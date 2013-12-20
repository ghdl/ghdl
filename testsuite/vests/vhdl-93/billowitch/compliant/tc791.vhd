
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
-- $Id: tc791.vhd,v 1.2 2001-10-26 16:30:00 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c01s01b02x00p03n01i00791ent_1 IS
END  ;

-- legal. with entity_simple_name
ENTITY c01s01b02x00p03n01i00791ent_2 IS
END c01s01b02x00p03n01i00791ent_2 ;

-- legal. begin with no statements following
ENTITY c01s01b02x00p03n01i00791ent_3 IS
begin
END c01s01b02x00p03n01i00791ent_3;

-- legal. no space before semicolon
ENTITY c01s01b02x00p03n01i00791ent_4 IS
END c01s01b02x00p03n01i00791ent_4;

-- legal. NEW line before semicolon
ENTITY c01s01b02x00p03n01i00791ent_5 IS
END c01s01b02x00p03n01i00791ent_5
  ;

--------------------------------
ENTITY c01s01b02x00p03n01i00791ent IS
END c01s01b02x00p03n01i00791ent;

ARCHITECTURE c01s01b02x00p03n01i00791arch OF c01s01b02x00p03n01i00791ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***PASSED TEST: c01s01b02x00p03n01i00791" 
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c01s01b02x00p03n01i00791arch;
