
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
-- $Id: tc2491.vhd,v 1.2 2001-10-26 16:30:19 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b03x00p04n01i02491ent IS
END c07s03b03x00p04n01i02491ent;

ARCHITECTURE c07s03b03x00p04n01i02491arch OF c07s03b03x00p04n01i02491ent IS

BEGIN
  TESTING: PROCESS
    function F_REAL ( A,B,C : REAL; D : REAL := 4.0 ) return REAL is
    begin
      return A + B + C + D;
    end F_REAL;
    subtype R is REAL range REAL'LEFT to F_REAL( 1.0, 2.0, 3.0, B=>4.0 );-- Failure_here
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c07s03b03x00p04n01i02491 - Each formal parameter of a function should have exactly one actual parameter associated with it in a function call."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b03x00p04n01i02491arch;
