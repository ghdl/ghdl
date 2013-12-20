
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
-- $Id: tc1267.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s02b00x00p07n01i01267ent IS
END c08s02b00x00p07n01i01267ent;

ARCHITECTURE c08s02b00x00p07n01i01267arch OF c08s02b00x00p07n01i01267ent IS

BEGIN
  TESTING: PROCESS
  BEGIN

    assert FALSE;
    assert FALSE 
      report "***PASSED TEST: c08s02b00x00p07n01i01267 - This test needs manual check. The assertion message consists at least that 1.An indication that this message is from an assertion. 2.Severity level. 3.Value of the message string. 4.The name of the design unit."
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c08s02b00x00p07n01i01267arch;
