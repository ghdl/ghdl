
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
-- $Id: tc1376.vhd,v 1.2 2001-10-26 16:30:09 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s05b00x00p03n01i01376ent IS
END c08s05b00x00p03n01i01376ent;

ARCHITECTURE c08s05b00x00p03n01i01376arch OF c08s05b00x00p03n01i01376ent IS

BEGIN
  TESTING: PROCESS

    type type1 is range 1 to 10;
    type type2 is range 1 to 10;

    variable v1 : type1 := 1;
    variable v2 : type2 := 1; 
    
  BEGIN
    --
    -- The following variable assignment is illegal and
    -- should generate a type mis-match error.
    --
    v1 := v2;                         -- mismatched types

    assert FALSE 
      report "***FAILED TEST: c08s05b00x00p03n01i01376 - Named variable and right-hand side expression type mismatched." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s05b00x00p03n01i01376arch;
