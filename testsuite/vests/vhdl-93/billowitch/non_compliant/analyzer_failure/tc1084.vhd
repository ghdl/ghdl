
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
-- $Id: tc1084.vhd,v 1.2 2001-10-26 16:30:06 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s05b00x00p02n01i01084ent IS
END c06s05b00x00p02n01i01084ent;

ARCHITECTURE c06s05b00x00p02n01i01084arch OF c06s05b00x00p02n01i01084ent IS

BEGIN
  TESTING: PROCESS
    type FIVE is range 1 to 5;
    type A51 is array (FIVE) of BOOLEAN;
    type A53 is array (FIVE) of A51;
    
    variable V51: A51 ;
    variable V53: A53 ;
  BEGIN
    V51(2 downto 1, 3 to 4) := V51(2 downto 1, 3 to 4);       
    -- SYNTAX ERROR: NO MULTIPLE DISCRETE RANGES IN SLICE NAMES
    assert FALSE 
      report "***FAILED TEST: c06s05b00x00p02n01i01084 - Slice name consists of a single discrete range enclosed within parentheses."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s05b00x00p02n01i01084arch;
