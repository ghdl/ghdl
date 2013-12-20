
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
-- $Id: tc2487.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b03x00p02n01i02487ent IS
END c07s03b03x00p02n01i02487ent;

ARCHITECTURE c07s03b03x00p02n01i02487arch OF c07s03b03x00p02n01i02487ent IS

BEGIN
  TESTING: PROCESS
    function typeconv (a1 : real) return integer is
    begin
      return 1;
    end;
    function func1    (a2 : integer) return integer is
    begin
      return 5;
    end func1;
    variable x: real := 1.2;
    variable y: integer;
  BEGIN
    y := func1 (typeconv (x));
    assert NOT(y=5)
      report "***PASSED TEST: c07s03b03x00p02n01i02487" 
      severity NOTE;
    assert (y=5)
      report "***FAILED TEST: c07s03b03x00p02n01i02487 - The function call consists of a function name and (optionally) an actual parameter list enclosed with parentheses."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b03x00p02n01i02487arch;
