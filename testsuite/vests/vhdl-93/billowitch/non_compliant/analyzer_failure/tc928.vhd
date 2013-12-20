
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
-- $Id: tc928.vhd,v 1.2 2001-10-26 16:30:28 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c10s04b00x00p01n01i00928pkg is
  -- It is OK to define a type that overrides the name of a library
  type work is (foo, bar);   -- No_failure_here
end c10s04b00x00p01n01i00928pkg;

use  work.c10s04b00x00p01n01i00928pkg.all;
ENTITY c10s04b00x00p01n01i00928ent IS
  port (P : in  bit);
END c10s04b00x00p01n01i00928ent;

ARCHITECTURE c10s04b00x00p01n01i00928arch OF c10s04b00x00p01n01i00928ent IS

BEGIN
  TESTING: PROCESS(P)
    -- This is an error because the type work defined in work.c10s04b00x00p01n01i00928pkg is
    -- NOT directly visible, it is overridden by library "work"
    variable doit : work ;    -- Failure_here
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c10s04b00x00p01n01i00928 - Type definition for 'work' does not exist in scope of declaration region for architecture 'blow2' of 'E'."
      severity ERROR;
  END PROCESS TESTING;

END c10s04b00x00p01n01i00928arch;
