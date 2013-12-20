
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
-- $Id: tc289.vhd,v 1.2 2001-10-26 16:30:23 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s01b03x00p13n01i00289ent IS
END c03s01b03x00p13n01i00289ent;

ARCHITECTURE c03s01b03x00p13n01i00289arch OF c03s01b03x00p13n01i00289ent IS
  type T is
    range 1 to 100
    units
      I ;  
      J = 2 I;
      K = 2 J;
      L = 10 K;
    end units;
  signal S1 : T;
BEGIN
  TESTING: PROCESS
  BEGIN
    S1 <= 10 * L;
    assert FALSE 
      report "***FAILED TEST: c03s01b03x00p13n01i00289 - Value doesn't belong to the physical type." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s01b03x00p13n01i00289arch;
