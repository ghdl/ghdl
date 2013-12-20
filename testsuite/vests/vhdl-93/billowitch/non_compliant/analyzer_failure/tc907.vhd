
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
-- $Id: tc907.vhd,v 1.2 2001-10-26 16:30:28 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c10s03b00x00p05n01i00907pkg is
  function FA ( B : INTEGER ) return INTEGER;
  function FB ( B : INTEGER ) return INTEGER;
end c10s03b00x00p05n01i00907pkg;

package body c10s03b00x00p05n01i00907pkg is
  function FA ( B : INTEGER ) return INTEGER is
    constant C : INTEGER := 6;
  begin
    return B;
  end FA;
  
  function FB ( B : INTEGER ) return INTEGER is
  begin
    return C; -- Failure_here
    -- error: entity not within the region it is immediately declared
  end FB;
end c10s03b00x00p05n01i00907pkg;


ENTITY c10s03b00x00p05n01i00907ent IS
END c10s03b00x00p05n01i00907ent;

ARCHITECTURE c10s03b00x00p05n01i00907arch OF c10s03b00x00p05n01i00907ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    wait for 5 ns;
    assert FALSE
      report "***FAILED TEST: c10s03b00x00p05n01i00907 - Entity is not within the region it is immediately declared in."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c10s03b00x00p05n01i00907arch;
