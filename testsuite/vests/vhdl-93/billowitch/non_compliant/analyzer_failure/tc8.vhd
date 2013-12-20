
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
-- $Id: tc8.vhd,v 1.2 2001-10-26 16:30:27 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s01b00x00p08n01i00008ent IS
END c04s01b00x00p08n01i00008ent;

ARCHITECTURE c04s01b00x00p08n01i00008arch OF c04s01b00x00p08n01i00008ent IS

BEGIN
  TESTING: PROCESS
    type A1 is array (1 to 1) of BOOLEAN;
    type A2 is array (1 to 1) of BOOLEAN;
    
    variable V7: A1;
    variable V8: A2;
  BEGIN
    if V7 = V8 then  -- Failure_here
      -- ERROR - SEMANTIC ERROR: OPERANDS OF = INCOMPATIBLE IN TYPE
      null ;
    end if;
    
    assert FALSE 
      report "***FAILED TEST: c04s01b00x00p08n01i00008 - Types are different and hence incompatible."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s01b00x00p08n01i00008arch;
