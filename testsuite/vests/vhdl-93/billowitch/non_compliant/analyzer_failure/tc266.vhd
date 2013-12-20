
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
-- $Id: tc266.vhd,v 1.2 2001-10-26 16:30:21 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s01b03x00p02n01i00266ent IS
END c03s01b03x00p02n01i00266ent;

ARCHITECTURE c03s01b03x00p02n01i00266arch OF c03s01b03x00p02n01i00266ent IS
  type UPLE is
    units  -- Failure_here
      -- ERROR - SYNTAX ERROR: PHYSICAL TYPE DEFINITION MUST HAVE RANGE CONSTRAINT
      single;
      double = 2 single;
      triple = 3 single;
      quadruple = 2 double;
      pentuple = 5 single;
      sextuple = 2 triple;
      septuple = 7 single;
      octuple = 2 quadruple;
    end units;
BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c03s01b03x00p02n01i00266 - Physical type definition must have range constraint." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s01b03x00p02n01i00266arch;
