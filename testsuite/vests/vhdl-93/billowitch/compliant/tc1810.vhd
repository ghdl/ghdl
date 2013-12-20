
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
-- $Id: tc1810.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s01b00x00p08n01i01810ent IS
END c07s01b00x00p08n01i01810ent;

ARCHITECTURE c07s01b00x00p08n01i01810arch OF c07s01b00x00p08n01i01810ent IS

BEGIN
  TESTING: PROCESS
    variable a1 : boolean := true;
    variable b1 : boolean;
    variable x1 : integer := 12;
    variable y1 : real := 12.3;
    variable p1 : real := 12.5;
    variable z1 : integer := 10;
  BEGIN
    b1 := (x1 < z1) or (y1 > p1) or (x1 = z1) or a1; -- No_failure_here
    assert NOT(b1 = true)
      report "***PASSED TEST: c07s01b00x00p08n01i01810"
      severity NOTE;
    assert ( b1 = true )
      report "***FAILED TEST: c07s01b00x00p08n01i01810 - The primary must be a name, a literal, an aggregate, a function call, a qualified expression, a type conversion, an allocator, or an expression enclosed with parentheses."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s01b00x00p08n01i01810arch;
