
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
-- $Id: tc3067.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s04b02x00p06n01i03067ent IS
END c12s04b02x00p06n01i03067ent;

ARCHITECTURE c12s04b02x00p06n01i03067arch OF c12s04b02x00p06n01i03067ent IS

BEGIN
  G1: if TRUE generate
    assert FALSE 
      report "***This assertion note should occur.***" 
      severity NOTE;
  end generate;
  G2: if FALSE generate
    assert FALSE 
      report "***This assertion note should not occur.***" 
      severity ERROR;
  end generate;

  TESTING: PROCESS
  BEGIN
    assert FALSE
      report "***PASSED TEST: c12s04b02x00p06n01i03067 - This test needs manual check to make sure CORRECT assertion note appear."
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c12s04b02x00p06n01i03067arch;
