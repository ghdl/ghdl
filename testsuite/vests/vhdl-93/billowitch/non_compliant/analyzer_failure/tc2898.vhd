
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
-- $Id: tc2898.vhd,v 1.2 2001-10-26 16:30:23 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s01b01x00p06n01i02898ent IS
END c02s01b01x00p06n01i02898ent;

ARCHITECTURE c02s01b01x00p06n01i02898arch OF c02s01b01x00p06n01i02898ent IS
  function func1 (signal A:integer) return integer is
  begin
    if a > 0 then
      return 5;
    else
      return 0;
    end if;
  end func1;
  constant C1 : integer := 0;
BEGIN
  TESTING: PROCESS
    variable V1 : integer;
  BEGIN
    V1 := func1( C1 );  -- Failure_here
    -- ERROR: Actual corresponding to a formal of class signal must be a signal
    assert FALSE
      report "***FAILED TEST: c02s01b01x00p06n01i02898 - The formal designator of class signal must be associated with an actual of class signal."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s01b01x00p06n01i02898arch;
