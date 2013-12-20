
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
-- $Id: tc934.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c10s04b00x00p04n01i00934pkg is
  Type Weekdays is (Monday, Tuesday, Wednesday, Thursday, Friday);
end c10s04b00x00p04n01i00934pkg;

ENTITY c10s04b00x00p04n01i00934ent IS
END c10s04b00x00p04n01i00934ent;

use  WORK.c10s04b00x00p04n01i00934pkg.all;
ARCHITECTURE c10s04b00x00p04n01i00934arch OF c10s04b00x00p04n01i00934ent IS
  signal done : bit;
  signal wkday :Weekdays;    -- No_failure_here
BEGIN
  TESTING : PROCESS
  BEGIN
    assert NOT(wkday = Monday) 
      report "***PASSED TEST: c10s04b00x00p04n01i00934"
      severity NOTE;
    assert (wkday = Monday) 
      report "***FAILED TEST: c10s04b00x00p04n01i00934 - Items declared via a use clause are visible in the declarative region."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c10s04b00x00p04n01i00934arch;
