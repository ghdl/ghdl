
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
-- $Id: tc1649.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s13b00x00p03n01i01649ent IS
END c08s13b00x00p03n01i01649ent;

ARCHITECTURE c08s13b00x00p03n01i01649arch OF c08s13b00x00p03n01i01649ent IS

BEGIN
  TESTING: PROCESS
    -- local variables
    variable FIRST_DONE  : BOOLEAN := FALSE;
    variable SECOND_DONE : BOOLEAN := FALSE;
  BEGIN
    FIRST_DONE := TRUE;
    null;
    SECOND_DONE := TRUE;

    -- Make sure that both statements surrounding the null
    -- statement got executed.
    assert NOT(FIRST_DONE and SECOND_DONE)
      report "***PASSED TEST: c08s13b00x00p03n01i01649"
      severity NOTE;
    assert (FIRST_DONE and SECOND_DONE)
      report "***FAILED TEST: c08s13b00x00p03n01i01649 - The execution of the null statement has no effect other than to pass on to the next statement." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s13b00x00p03n01i01649arch;
