
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
-- $Id: tc1159.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s06b00x00p02n01i01159ent IS
END c06s06b00x00p02n01i01159ent;

ARCHITECTURE c06s06b00x00p02n01i01159arch OF c06s06b00x00p02n01i01159ent IS

BEGIN
  TESTING: PROCESS
    type arr is array(0 to 50) of boolean;

    function ret_arr(I : integer) return arr is
      variable RA : arr ;
    begin
      return  RA;
    end ret_arr;
    variable k : integer := 0;
  BEGIN
    k := arr'low;
    assert NOT( k=0 )
      report "***PASSED TEST: c06s06b00x00p02n01i01159"
      severity NOTE;
    assert ( k=0 )
      report "***FAILED TEST: c06s06b00x00p02n01i01159 - The prefix of an attribute name may be a selected name." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s06b00x00p02n01i01159arch;
