
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
-- $Id: tc899.vhd,v 1.2 2001-10-26 16:30:28 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c10s03b00x00p04n01i00899pkg_1 is
  type    T  is (one,two,three,four);
  subtype    SS is INTEGER;
  function    F return REAL;
end c10s03b00x00p04n01i00899pkg_1;

package body c10s03b00x00p04n01i00899pkg_1 is
  function F return REAL is
  begin
    return 0.0;
  end F;
end c10s03b00x00p04n01i00899pkg_1;

package c10s03b00x00p04n01i00899pkg_2 is
  type    T  is (one,two,three,four);
  subtype    SS is INTEGER;
  function    F return REAL;
end c10s03b00x00p04n01i00899pkg_2;

package body c10s03b00x00p04n01i00899pkg_2 is
  function F return REAL is
  begin
    return 0.0;
  end F;
end c10s03b00x00p04n01i00899pkg_2;

use work.c10s03b00x00p04n01i00899pkg_1.all,work.c10s03b00x00p04n01i00899_pkg_2.all;
ENTITY c10s03b00x00p04n01i00899ent IS
  port (P:BOOLEAN) ;
  
  subtype S2 is SS;  -- Failure_here
  -- SEMANTIC ERROR:  ambiguous reference to subtype SS
  
  type R is range F to F;  -- Failure_here
  -- SEMANTIC ERROR:  ambiguous reference to function F
END c10s03b00x00p04n01i00899ent;

ARCHITECTURE c10s03b00x00p04n01i00899arch OF c10s03b00x00p04n01i00899ent IS

BEGIN
  TESTING: PROCESS
    variable V1 : T;  -- Failure_here
    -- SEMANTIC ERROR:  ambiguous reference to type T
    
    variable V2 : SS;  -- Failure_here
    -- SEMANTIC ERROR:  ambiguous reference to subtype SS
  BEGIN
    V1 := one;  -- Failure_here
    -- SEMANTIC ERROR:  ambiguous reference to literal "one"
    assert FALSE
      report "***FAILED TEST: c10s03b00x00p04n01i00899 - Ambiguous references not permitted."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c10s03b00x00p04n01i00899arch;
