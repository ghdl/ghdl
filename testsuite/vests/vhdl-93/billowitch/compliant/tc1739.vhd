
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
-- $Id: tc1739.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s04b00x00p10n01i01739ent IS
begin
  assert false 
    report "Success:entity assertion with static expression" 
    severity NOTE;
END c09s04b00x00p10n01i01739ent;

ARCHITECTURE c09s04b00x00p10n01i01739arch OF c09s04b00x00p10n01i01739ent IS

BEGIN
  assert false 
    report "Success:architecture assertion with static expression" 
    severity NOTE;

  b: block
  begin
    assert false 
      report "Success:architecture in block: assertion with static expression" 
      severity NOTE;
  end block b;

  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***PASSED TEST: c09s04b00x00p10n01i01739 - This test need manual check, three assertion notes of Success should appear."
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c09s04b00x00p10n01i01739arch;
