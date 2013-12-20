
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
-- $Id: tc2293.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b06x00p32n01i02293ent IS
END c07s02b06x00p32n01i02293ent;

ARCHITECTURE c07s02b06x00p32n01i02293arch OF c07s02b06x00p32n01i02293ent IS
BEGIN
  TESTING: PROCESS
  BEGIN
    -- Test the predefined type TIME in this respect.
    assert ((1 us * 1000.0) > 1 us)
      report "Assertion error.(31)";
    assert ((1 ms * 1000.0) > 1 ms)
      report "Assertion error.(32)";
    assert ((1 sec * 60.0)  > 1 sec)
      report "Assertion error.(33)";
    wait for 5 us;
    assert NOT(    ((1 us * 1000.0) > 1 us)   and
                   ((1 ms * 1000.0) > 1 ms)   and
                   ((1 sec * 60.0)  > 1 sec)   and
                   ((1000.0 * 1 us) > 1 us)   and
                   ((1000.0 * 1 ms) > 1 ms)   and
                   ((60.0 * 1 sec)  > 1 sec)   )
      report "***PASSED TEST: c07s02b06x00p32n01i02293"
      severity NOTE;
    assert (    ((1 us * 1000.0) > 1 us)   and
                ((1 ms * 1000.0) > 1 ms)   and
                ((1 sec * 60.0)  > 1 sec)   and
                ((1000.0 * 1 us) > 1 us)   and
                ((1000.0 * 1 ms) > 1 ms)   and
                ((60.0 * 1 sec)  > 1 sec)   )
      report "***FAILED TEST: c07s02b06x00p32n01i02293 - Multiplication of a predefined physical type by an floating point test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b06x00p32n01i02293arch;
