
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
-- $Id: tc2713.vhd,v 1.2 2001-10-26 16:29:49 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c13s04b02x00p01n01i02713ent IS
END c13s04b02x00p01n01i02713ent;

ARCHITECTURE c13s04b02x00p01n01i02713arch OF c13s04b02x00p01n01i02713ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT(   16#F.FF#E+2 = 2#1.1111_1111_111#E11   and
                  16#F.FF#E+2 = 4095.0   ) 
      report "***PASSED TEST: c13s04b02x00p01n01i02713"
      severity NOTE;
    assert (   16#F.FF#E+2 = 2#1.1111_1111_111#E11   and
               16#F.FF#E+2 = 4095.0   ) 
      report "***FAILED TEST: c13s04b02x00p01n01i02713 - Base literals expressed in two and sixteen test failed." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c13s04b02x00p01n01i02713arch;
