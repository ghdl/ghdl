
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
-- $Id: tc1650.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s13b00x00p03n01i01650ent IS
END c08s13b00x00p03n01i01650ent;

ARCHITECTURE c08s13b00x00p03n01i01650arch OF c08s13b00x00p03n01i01650ent IS

BEGIN
  TESTING: PROCESS
    -- local variables
    variable LOCALI      : INTEGER := 47;
    variable LOCALR      : REAL    := 47.0;
    variable LOCALB      : BOOLEAN := TRUE;
  BEGIN
    -- Check for proper initialization.
    assert (LOCALI = 47);
    assert (LOCALR = 47.0);
    assert (LOCALB = TRUE);

    -- Execute the NULL statement.
    null;

    -- Verify that nothing has changed as a result.
    assert NOT((LOCALI = 47)   and
               (LOCALR = 47.0)   and
               (LOCALB = TRUE))
      report "***PASSED TEST: c08s13b00x00p03n01i01650"
      severity NOTE;
    assert ((LOCALI = 47)      and
            (LOCALR = 47.0)      and
            (LOCALB = TRUE))
      report "***FAILED TEST: c08s13b00x00p03n01i01650 - The execution of the null statement has no effect on any of the local variable within the process." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s13b00x00p03n01i01650arch;
