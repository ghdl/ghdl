
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
-- $Id: tc2286.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b06x00p14n01i02286ent IS
END c07s02b06x00p14n01i02286ent;

ARCHITECTURE c07s02b06x00p14n01i02286arch OF c07s02b06x00p14n01i02286ent IS

BEGIN
  TESTING: PROCESS
    type PHYS is range 1 to 100000
      units
        A;
        B = 100 A;
        C = 100 B;
      end units;
    function F_PHYS ( A : PHYS ) return PHYS is
    begin   
      return A;       
    end F_PHYS;
    variable P : PHYS    := 1 B;
    variable Z : integer := time'(1 min) / time'(27 sec);
  BEGIN
    Z  := P / F_PHYS(1 A);
    assert NOT(Z = 100)
      report "***PASSED TEST: c07s02b06x00p14n01i02286" 
      severity NOTE;
    assert (Z = 100)
      report "***FAILED TEST: c07s02b06x00p14n01i02286 - Incompatible operands: May not be multiplied or divided." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b06x00p14n01i02286arch;
