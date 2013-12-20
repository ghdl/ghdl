
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
-- $Id: tc290.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s01b03x00p13n01i00290ent IS
  type mytime is range 1 to 30 
    units
      fs;
    end units;
END c03s01b03x00p13n01i00290ent;

ARCHITECTURE c03s01b03x00p13n01i00290arch OF c03s01b03x00p13n01i00290ent IS

BEGIN
  TESTING: PROCESS
    variable t,a   :mytime;
    variable b   :integer;
  BEGIN
    a:=30 fs;
    b := 10;
    t:= a/b;
    assert NOT(t = 3 fs) 
      report "***PASSED TEST: c03s01b03x00p13n01i00290" 
      severity NOTE;
    assert (t = 3 fs) 
      report "***FAILED TEST: c03s01b03x00p13n01i00290 - Physical type value arithmetic operation failed." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s01b03x00p13n01i00290arch;
