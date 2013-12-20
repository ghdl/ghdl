
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
-- $Id: tc2977.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s03b01x00p05n01i02977ent IS
END c02s03b01x00p05n01i02977ent;

ARCHITECTURE c02s03b01x00p05n01i02977arch OF c02s03b01x00p05n01i02977ent IS
  function "and" (constant c1,c2 : in boolean) return boolean is
  begin
    return true;
  end;
BEGIN
  TESTING: PROCESS
    variable b1 : boolean := true;
    variable bf : boolean := false;
    variable bt : boolean := true;
  BEGIN
    assert (b1=true)
      report "Error in initial conditions detected"
      severity failure;
    assert (bf=false)
      report "Error in initial conditions detected"
      severity failure;
    assert (bt=true)
      report "Error in initial conditions detected"
      severity failure;
    b1 := bf and bt;
    assert NOT( b1=true )
      report "***PASSED TEST: c02s03b01x00p05n01i02977"
      severity NOTE;
    assert ( b1=true )
      report "***FAILED TEST: c02s03b01x00p05n01i02977 - Error in invocation overloaded operator and."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s03b01x00p05n01i02977arch;
