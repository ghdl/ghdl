
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
-- $Id: tc2497.vhd,v 1.2 2001-10-26 16:30:19 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b03x00p04n02i02497ent IS
END c07s03b03x00p04n02i02497ent;

ARCHITECTURE c07s03b03x00p04n02i02497arch OF c07s03b03x00p04n02i02497ent IS

BEGIN
  TESTING: PROCESS
    function func1 (a1 : real; b1 : integer:= 12) return integer is 
    begin
      return 5;
    end;
    variable x: real := 1.2;
    variable y: integer ;
  BEGIN
    y := func1 (y,x);
    assert FALSE 
      report "***FAILED TEST: c07s03b03x00p04n02i02497 - The actual parameter can be specified explicitly by an association element in the association list."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b03x00p04n02i02497arch;
