
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
-- $Id: tc239.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s01b02x00p04n01i00239ent IS
END c03s01b02x00p04n01i00239ent;

ARCHITECTURE c03s01b02x00p04n01i00239arch OF c03s01b02x00p04n01i00239ent IS
  type t3 is range (((((10-1)-1)-1)-1)-1) to (((((10+1)+1)+1)+1)+1); 
BEGIN
  TESTING: PROCESS
    variable k : integer := 6;
  BEGIN
    k := 5;
    assert NOT(k=5) 
      report "***PASSED TEST: c03s01b02x00p04n01i00239" 
      severity NOTE;
    assert (k=5) 
      report "***FAILED TEST: c03s01b02x00p04n01i00239 - Each each bound of a range constraint that is used in an integer type definition is a locally static expression [of some integer type, but the two bounds need not have the same integer type.]" 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s01b02x00p04n01i00239arch;
