
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
-- $Id: tc2989.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c02s05b00x00p06n01i02989pkg is
  constant    c1    : INTEGER;
  function    f    return INTEGER;
  procedure    p(x : inout INTEGER);
end c02s05b00x00p06n01i02989pkg;

package body c02s05b00x00p06n01i02989pkg is
  constant c1 : INTEGER := 10;
  constant c2 : INTEGER := 20;
  function f return INTEGER is
  begin
    return c1 + c2;
  end;
  procedure p( x: inout INTEGER) is
  begin
    x := c1 + c2;
  end;
end c02s05b00x00p06n01i02989pkg;


ENTITY c02s05b00x00p06n01i02989ent IS
END c02s05b00x00p06n01i02989ent;

ARCHITECTURE c02s05b00x00p06n01i02989arch OF c02s05b00x00p06n01i02989ent IS
  signal s1 : INTEGER := WORK.c02s05b00x00p06n01i02989pkg.c1;
  signal s2 : INTEGER := WORK.c02s05b00x00p06n01i02989pkg.c1;
BEGIN
  TESTING: PROCESS
    variable temp : INTEGER;
  BEGIN
    s1 <= WORK.c02s05b00x00p06n01i02989pkg.F;

    WORK.c02s05b00x00p06n01i02989pkg.P(temp);
    s2 <= temp;
    wait for 5 ns;
    assert NOT( s1 = 30 and s2 = 30 )
      report "***PASSED TEST: c02s05b00x00p06n01i02989"
      severity NOTE;
    assert ( s1 = 30 and s2 = 30 )
      report "***FAILED TEST: c02s05b00x00p06n01i02989 - Package declaration visibility test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s05b00x00p06n01i02989arch;
