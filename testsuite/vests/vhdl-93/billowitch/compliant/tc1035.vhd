
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
-- $Id: tc1035.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s04b00x00p02n01i01035ent IS
END c06s04b00x00p02n01i01035ent;

ARCHITECTURE c06s04b00x00p02n01i01035arch OF c06s04b00x00p02n01i01035ent IS

BEGIN
  TESTING: PROCESS
    type THREE is range 1 to 3;
    
    type A1 is array (THREE) of BOOLEAN;
    type A2 is array (THREE, THREE) of BOOLEAN;
    type A3 is array (THREE) of A1;
    
    function F3(i : integer) return A3 is
      variable AR3 : A3;
    begin
      return AR3;
    end F3;
    variable A  : integer;
    variable V  : BOOLEAN;
    
  BEGIN
    V := F3(A)(1)(3);   -- Indexed Name
    assert NOT(V=false)
      report "***PASSED TEST: c06s04b00x00p02n01i01035"
      severity NOTE;
    assert (V= false) 
      report "***FAILED TEST: c06s04b00x00p02n01i01035 - The prefix of an indexed name can be a indexed name."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s04b00x00p02n01i01035arch;
