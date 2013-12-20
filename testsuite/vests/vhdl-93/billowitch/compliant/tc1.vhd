
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
-- $Id: tc1.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s01b00x00p03n01i00001ent IS
END c04s01b00x00p03n01i00001ent;

ARCHITECTURE c04s01b00x00p03n01i00001arch OF c04s01b00x00p03n01i00001ent IS
  type t1 is range 0.012345 to 300.012345;  -- No_failure_here
BEGIN
  TESTING: PROCESS
    variable k : t1 := 10.0;
  BEGIN
    k := 123.0;
    assert NOT( k=123.0 )
      report "***PASSED TEST: c04s01b00x00p03n01i00001"
      severity NOTE;
    assert ( k=123.0 )
      report "***FAILED TEST: c04s01b00x00p03n01i00001 - Type declaration has the format: the reserved word type followed by an identifier and the reserved word is."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s01b00x00p03n01i00001arch;
