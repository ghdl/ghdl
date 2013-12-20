
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
-- $Id: tc2291.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b06x00p32n01i02291ent IS
END c07s02b06x00p32n01i02291ent;

ARCHITECTURE c07s02b06x00p32n01i02291arch OF c07s02b06x00p32n01i02291ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    -- Test the predefined type TIME in this respect.
    assert ((1 fs * 1000.0) > 1 fs)
      report "Assertion error.(28)";
    assert ((1 ps * 1000.0) > 1 ps)
      report "Assertion error.(29)";
    assert ((1 ns * 1000.0) > 1 ns)
      report "Assertion error.(30)";
    wait for 5 fs;
    assert NOT(    ((1 fs * 1000.0) > 1 fs)   and
                   ((1 ps * 1000.0) > 1 ps)   and
                   ((1 ns * 1000.0) > 1 ns)   and
                   ((1000.0 * 1 fs) > 1 fs)   and
                   ((1000.0 * 1 ps) > 1 ps)   and
                   ((1000.0 * 1 ns) > 1 ns)   )
      report "***PASSED TEST: c07s02b06x00p32n01i02291"
      severity NOTE;
    assert (    ((1 fs * 1000.0) > 1 fs)   and
                ((1 ps * 1000.0) > 1 ps)   and
                ((1 ns * 1000.0) > 1 ns)   and
                ((1000.0 * 1 fs) > 1 fs)   and
                ((1000.0 * 1 ps) > 1 ps)   and
                ((1000.0 * 1 ns) > 1 ns)   )
      report "***FAILED TEST: c07s02b06x00p32n01i02291 - Multiplication of a predefined physical type by an floating point test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b06x00p32n01i02291arch;
