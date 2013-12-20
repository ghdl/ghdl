
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
-- $Id: tc1667.vhd,v 1.2 2001-10-26 16:30:11 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s01b00x00p05n01i01667ent IS
END c09s01b00x00p05n01i01667ent;

ARCHITECTURE c09s01b00x00p05n01i01667arch OF c09s01b00x00p05n01i01667ent IS

BEGIN
  B:block
    variable err : boolean := true;   -- illegal location for variable decl
  begin
    err := true;                      -- illegal location for variable assignment
  end block;

  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c09s01b00x00p05n01i01667 - Sequential statement not allowed." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c09s01b00x00p05n01i01667arch;
