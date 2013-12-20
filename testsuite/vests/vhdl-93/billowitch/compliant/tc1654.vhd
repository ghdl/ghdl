
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
-- $Id: tc1654.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s00b00x00p04n01i01654ent IS
END c09s00b00x00p04n01i01654ent;

ARCHITECTURE c09s00b00x00p04n01i01654arch OF c09s00b00x00p04n01i01654ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE                            -- force assertion violation
      report "PASS: process TESTING executes."
      severity NOTE;

    assert FALSE
      report "***PASSED TEST: c09s00b00x00p04n01i01654 - it is really uncertain that which assertion note appear first for different simulator."
      severity NOTE;
    wait;
  END PROCESS TESTING;

  TEST: PROCESS
  begin
    assert FALSE
      report "PASS: process TEST executes."
      severity NOTE;
    
    -- Note: It does not matter which process executes first. (The order
    --      of process execution is not defined by the LRM, and dependence
    --      on the execution order is not allowed.)
    
    wait;                                   -- wait forever
  END PROCESS TEST;

END c09s00b00x00p04n01i01654arch;
