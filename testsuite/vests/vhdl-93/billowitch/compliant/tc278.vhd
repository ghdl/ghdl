
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
-- $Id: tc278.vhd,v 1.2 2001-10-26 16:29:49 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s01b03x00p07n01i00278ent IS
END c03s01b03x00p07n01i00278ent;

ARCHITECTURE c03s01b03x00p07n01i00278arch OF c03s01b03x00p07n01i00278ent IS
  type twos_complement_integer is range -32768 to 32767;
  type J is
    range twos_complement_integer'low to twos_complement_integer'high
    units         -- Success_here
      A;
      B = 10 A;
      C = 10 B;
      D = 10 C;
    end units;
BEGIN
  TESTING: PROCESS
    variable k : J := 31000 A;
  BEGIN
    k := 5 A;
    assert NOT(k=5 A) 
      report "***PASSED TEST: c03s01b03x00p07n01i00278" 
      severity NOTE;
    assert (k=5 A) 
      report "***FAILED TEST: c03s01b03x00p07n01i00278 - The bounds in the range constraint are not locally static expressions."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s01b03x00p07n01i00278arch;
