
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
-- $Id: tc53.vhd,v 1.2 2001-10-26 16:29:56 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c04s03b01x01p04n03i00053pkg is
  constant C1 : Bit ;
  constant C2 : Integer ;
end c04s03b01x01p04n03i00053pkg;

package body c04s03b01x01p04n03i00053pkg is
  constant C1 : Bit    := '1';
  constant C2 : Integer    := 20;
end c04s03b01x01p04n03i00053pkg;  -- Failure_here


use work.c04s03b01x01p04n03i00053pkg.all;
ENTITY c04s03b01x01p04n03i00053ent IS
END c04s03b01x01p04n03i00053ent;

ARCHITECTURE c04s03b01x01p04n03i00053arch OF c04s03b01x01p04n03i00053ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT( C1 = '1' and C2 = 20 ) 
      report "***PASSED TEST:c04s03b01x01p04n03i00053" 
      severity NOTE;
    assert ( C1 = '1' and C2 = 20 ) 
      report "***FAILED TEST: c04s03b01x01p04n03i00053 - Full constant declaration test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b01x01p04n03i00053arch;
