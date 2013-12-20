
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
-- $Id: tc1384.vhd,v 1.2 2001-10-26 16:30:09 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s05b00x00p03n01i01384ent IS
END c08s05b00x00p03n01i01384ent;

ARCHITECTURE c08s05b00x00p03n01i01384arch OF c08s05b00x00p03n01i01384ent IS

BEGIN
  TESTING: PROCESS
    type small_int is range 0 to 7;
    variable v1 : small_int := 0;
  BEGIN

    small_int := v1;                  -- illegal type name target

    assert FALSE 
      report "***FAILED TEST: c08s05b00x00p03n01i01384 - Target of a variable assignment can not be the name of a type name." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s05b00x00p03n01i01384arch;
