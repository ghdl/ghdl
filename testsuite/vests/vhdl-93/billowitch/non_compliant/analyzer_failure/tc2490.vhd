
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
-- $Id: tc2490.vhd,v 1.2 2001-10-26 16:30:19 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b03x00p04n01i02490ent IS
END c07s03b03x00p04n01i02490ent;

ARCHITECTURE c07s03b03x00p04n01i02490arch OF c07s03b03x00p04n01i02490ent IS

BEGIN
  TESTING: PROCESS
    function check (x:integer; y:boolean; z:real) return boolean is
    begin
      if y then
        return true;
      end if;
      return false;
    end;
    variable p : integer := 3;
    variable q : boolean := true;
    variable s : boolean;
    variable r : real;
    variable r1: real;
  BEGIN
    s := check (p, q, r, r1);  -- Failure_here
    assert FALSE 
      report "***FAILED TEST: c07s03b03x00p04n01i02490 - Each formal parameter of a function should have exactly one actual parameter associated with it in a function call."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b03x00p04n01i02490arch;
