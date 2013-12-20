
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
-- $Id: tc1152.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s06b00x00p02n01i01152ent IS
END c06s06b00x00p02n01i01152ent;

ARCHITECTURE c06s06b00x00p02n01i01152arch OF c06s06b00x00p02n01i01152ent IS
  type iarray is array (1 to 10) of bit;
BEGIN
  TESTING: PROCESS
    variable k : integer := 0;
  BEGIN
    for foo in iarray'range(1) loop  -- Success_here
      k := k + 1;
    end loop;
    assert NOT( k=10 )
      report "***PASSED TEST: c06s06b00x00p02n01i01152"
      severity NOTE;
    assert ( k=10 )
      report "***FAILED TEST: c06s06b00x00p02n01i01152 - The attribute name consists of a prefix, an apostrophe('), an attribute designator, and (optionally) a static expression enclosed with parentheses."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s06b00x00p02n01i01152arch;
