
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
-- $Id: tc966.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c06s03b00x00p04n01i00966pkg is
  constant   tPLH : TIME := 10 ns;
  constant   tPHL : TIME := 12 ns;
end c06s03b00x00p04n01i00966pkg;

ENTITY c06s03b00x00p04n01i00966ent IS
END c06s03b00x00p04n01i00966ent;

ARCHITECTURE c06s03b00x00p04n01i00966arch OF c06s03b00x00p04n01i00966ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    wait for 5 ns;
    assert NOT(work.c06s03b00x00p04n01i00966pkg.tPLH = 10 ns and work.c06s03b00x00p04n01i00966pkg.tPHL = 12 ns)
      report "***PASSED TEST: c06s03b00x00p04n01i00966" 
      severity NOTE;
    assert (work.c06s03b00x00p04n01i00966pkg.tPLH = 10 ns and work.c06s03b00x00p04n01i00966pkg.tPHL = 12 ns)
      report "***FAILED TEST: c06s03b00x00p04n01i00966 - Selected name should be able to be used to denote a named entity whose declaration is contained within a package."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s03b00x00p04n01i00966arch;
