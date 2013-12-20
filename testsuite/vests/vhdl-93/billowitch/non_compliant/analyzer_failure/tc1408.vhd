
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
-- $Id: tc1408.vhd,v 1.2 2001-10-26 16:30:09 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s05b01x00p01n01i01408ent IS
END c08s05b01x00p01n01i01408ent;

ARCHITECTURE c08s05b01x00p01n01i01408arch OF c08s05b01x00p01n01i01408ent IS

BEGIN
  TESTING: PROCESS
    type      A1 is array (1 to 15) of integer;
    variable   XC : A1; 
  BEGIN
    XC (4 to 1) := (4,3,2,1);
    assert FALSE 
      report "***FAILED TEST: c08s05b01x00p01n01i01408 - the type of the target and the value assigned to the target in an array variable assignment statement must be the same." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s05b01x00p01n01i01408arch;
