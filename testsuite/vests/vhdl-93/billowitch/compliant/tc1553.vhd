
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
-- $Id: tc1553.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s09b00x00p10n01i01553ent IS
END c08s09b00x00p10n01i01553ent;

ARCHITECTURE c08s09b00x00p10n01i01553arch OF c08s09b00x00p10n01i01553ent IS
  signal VS : STRING(1 to 14) := "This is a test";
BEGIN
  TESTING: PROCESS
  BEGIN
    for i in VS'range loop
      VS <= VS(VS'LEFT + 1 to VS'RIGHT) & '_' after 1 ns;
      wait for 2 ns;
    end loop;
    wait for 5 ns;
    assert NOT( VS = "______________" )
      report "***PASSED TEST: c08s09b00x00p10n01i01553"
      severity NOTE;
    assert ( VS = "______________" )
      report "***FAILED TEST: c08s09b00x00p10n01i01553 - The loop parameter is declared by its appearance in the loop parameter specification and its scope is limited to the loop statement." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s09b00x00p10n01i01553arch;
