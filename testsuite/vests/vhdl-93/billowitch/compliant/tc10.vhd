
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
-- $Id: tc10.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s02b00x00p02n01i00010ent IS
END c04s02b00x00p02n01i00010ent;

ARCHITECTURE c04s02b00x00p02n01i00010arch OF c04s02b00x00p02n01i00010ent IS
  subtype eight_bit       is integer range -32768 to 32767;       -- No_failure_here
  subtype positive_8_bit    is eight_bit range 1 to 32767;     -- No_failure_here

  -- an unconstrained array declaration
  type  memory is array  (integer range <>) of bit;
  subtype foo1 is memory (1 to 10);                      -- No_failure_here
  subtype foo3 is memory (integer range 25 downto 2);    -- No_failure_here
BEGIN
  TESTING: PROCESS
    variable k1 : eight_bit    := 0;
    variable k2 : positive_8_bit    := 10;
    variable k3 : foo1       := ("1111111111");
    variable k5 : foo3       := ("111111111111111111111111");
  BEGIN
    assert NOT(    k1 = 0         and
                   k2 = 10         and
                   k3 = "1111111111"   and
                   k5 = "111111111111111111111111")
      report "***PASSED TEST: c04s02b00x00p02n01i00010"
      severity NOTE;
    assert (    k1 = 0         and
                k2 = 10         and
                k3 = "1111111111"   and
                k5 = "111111111111111111111111")
      report "***FAILED TEST: c04s02b00x00p02n01i00010 - Subtype declaration syntactic format test fail."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s02b00x00p02n01i00010arch;
