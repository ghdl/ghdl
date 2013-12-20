
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
-- $Id: tc913.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c10s03b00x00p07n01i00913pkg is
  -- It is OK to define a type that overrides the name of a library
  type work is (foo, bar);   -- No_failure_here
end c10s03b00x00p07n01i00913pkg;

use work.c10s03b00x00p07n01i00913pkg.all;
ENTITY c10s03b00x00p07n01i00913ent IS
END c10s03b00x00p07n01i00913ent;

ARCHITECTURE c10s03b00x00p07n01i00913arch OF c10s03b00x00p07n01i00913ent IS

BEGIN
  TESTING : PROCESS
    -- This succeeds in finding type "work" defined in package "c10s03b00x00p07n01i00913pkg"
    -- in library "work"
    variable doit : work.c10s03b00x00p07n01i00913pkg.work ;   -- No_failure_here
  BEGIN
    doit := foo;
    wait for 5 ns;
    assert NOT(doit = foo) 
      report "***PASSED TEST: c10s03b00x00p07n01i00913"
      severity NOTE;
    assert (doit = foo) 
      report "***FAILED TEST: c10s03b00x00p07n01i00913 - A declaration can be visible by selection for a primary unit contained in a library."
      severity ERROR;
    wait;
  END PROCESS;

END c10s03b00x00p07n01i00913arch;
