
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
-- $Id: tc1094.vhd,v 1.2 2001-10-26 16:30:06 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s05b00x00p03n01i01094ent IS
END c06s05b00x00p03n01i01094ent;

ARCHITECTURE c06s05b00x00p03n01i01094arch OF c06s05b00x00p03n01i01094ent IS

BEGIN
  TESTING: PROCESS
    type sting is array (1 to 5, 1 to 5) of character;
    variable str : sting;
  BEGIN
    str(1 to 3, 1 to 3) := str(3 to 5, 3 to 5); -- slice of a two
                                                -- dimensional array is
                                                -- illegal.
    assert FALSE 
      report "***FAILED TEST: c06s05b00x00p03n01i01094 - Prefix of a slice must be appropraite for a one-dimensional array object." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s05b00x00p03n01i01094arch;
