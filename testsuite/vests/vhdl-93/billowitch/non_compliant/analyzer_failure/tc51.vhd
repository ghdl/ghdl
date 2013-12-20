
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
-- $Id: tc51.vhd,v 1.2 2001-10-26 16:30:26 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c04s03b01x01p04n03i00051pkg is
  constant PI : Real;
  constant g : real;
end c04s03b01x01p04n03i00051pkg;

package body c04s03b01x01p04n03i00051pkg is
  constant g : Real := 9.8;           -- full declaration for 'g'
  -- The full declaration for PI is missing.
end c04s03b01x01p04n03i00051pkg;  -- Failure_here


ENTITY c04s03b01x01p04n03i00051ent IS
END c04s03b01x01p04n03i00051ent;

ARCHITECTURE c04s03b01x01p04n03i00051arch OF c04s03b01x01p04n03i00051ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c04s03b01x01p04n03i00051 - Declaration for deferred constant is missing in the package body."
      severity ERROR;
    wait;
  END PROCESS TESTING;

  ENDc04s03b01x01p04n03i00051arch;
