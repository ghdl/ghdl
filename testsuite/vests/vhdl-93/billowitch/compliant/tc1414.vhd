
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
-- $Id: tc1414.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s05b01x00p01n01i01414ent IS
END c08s05b01x00p01n01i01414ent;

ARCHITECTURE c08s05b01x00p01n01i01414arch OF c08s05b01x00p01n01i01414ent IS

BEGIN
  TESTING: PROCESS
    type array_type is array (1 to 10) of integer;
    variable   v1 : array_type;   
  BEGIN
    v1 (1) := integer'(12);
    assert NOT(v1(1)=12) 
      report "***PASSED TEST: c08s05b01x00p01n01i01414" 
      severity NOTE;
    assert (v1(1)=12) 
      report "***FAILED TEST: c08s05b01x00p01n01i01414 - Each element of the array variable there is a matching element on the right hand side." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s05b01x00p01n01i01414arch;
