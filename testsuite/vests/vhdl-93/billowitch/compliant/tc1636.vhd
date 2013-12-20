
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
-- $Id: tc1636.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s12b00x00p05n01i01636ent IS
END c08s12b00x00p05n01i01636ent;

ARCHITECTURE c08s12b00x00p05n01i01636arch OF c08s12b00x00p05n01i01636ent IS

BEGIN
  TESTING: PROCESS
    type PH is range 1 to 24
      units
        U;
        X=3 U;
        Y=2 X;
      end units;
    type AR1 is array (POSITIVE range <>) of PH;   
    function K return AR1 is
      variable V : AR1(49 to 50) ;
    begin
      V := (5 U,X) ;
      return V;
    end K;
    variable kk : AR1(49 to 50); 
  BEGIN
    kk := K;
    assert NOT(kk = (5 U,X))
      report "***PASSED TEST: c08s12b00x00p05n01i01636" 
      severity NOTE;
    assert (kk = (5 U,X))
      report "***FAILED TEST: c08s12b00x00p05n01i01636 - The return type must be the same base tyep declared in the specification of the function."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s12b00x00p05n01i01636arch;
