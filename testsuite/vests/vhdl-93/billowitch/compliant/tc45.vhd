
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
-- $Id: tc45.vhd,v 1.2 2001-10-26 16:29:54 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b01x01p02n01i00045ent IS
END c04s03b01x01p02n01i00045ent;

ARCHITECTURE c04s03b01x01p02n01i00045arch OF c04s03b01x01p02n01i00045ent IS
  type    T1_0 is array (integer range <>) of integer;
  subtype    T1_1 is T1_0 (1 to 2);
  subtype    T1_2 is T1_0 (1 to 4);

  type    T2_0 is array (integer range <>) of T1_2;
  subtype    T2_1 is T2_0 (1 to 2);
  subtype    T2_2 is T2_0 (1 to 4);

  -- Create some constants for constructing the real tests...
  constant C1   : T1_1 := (1, 2);
  constant C2   : T1_1 := C1;
  constant C3   : T1_2 := (1, 2, 3, 4);
  constant C4   : T1_2 := C3;

  -- Success_here : on all constant declarations below
  
  constant C5   : T2_1 := ((1, 2, 3, 4), (5, 6, 7, 8));
  constant C6   : T2_1 := (C3, C4);
  constant C7   : T2_1 := (C1 & C2, C2 & C1);
  constant C8   : T2_1 := (1 & 2 & C2, C3);
  constant C10: T2_2 := ((1, 2, 3, 4), (5, 6, 7, 8),
                         (9, 0, 1, 2), (3, 4, 5, 6));
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT(   C1 = (1,2)      and
                  C2 = (1,2)      and
                  C3 = (1,2,3,4)      and
                  C4 = (1,2,3,4)      and
                  C5 = ((1, 2, 3, 4), (5, 6, 7, 8))   and
                  C6 = ((1, 2, 3, 4), (1, 2, 3, 4))   and
                  C7 = ((1, 2, 1, 2), (1, 2, 1, 2))   and
                  C8 = ((1, 2, 1, 2), (1, 2, 3, 4))   and
                  C10= (   (1, 2, 3, 4), (5, 6, 7, 8),
                           (9, 0, 1, 2), (3, 4, 5, 6))   )
      report "***PASSED TEST: c04s03b01x01p02n01i00045"
      severity NOTE;
    assert (   C1 = (1,2)      and
               C2 = (1,2)      and
               C3 = (1,2,3,4)      and
               C4 = (1,2,3,4)      and
               C5 = ((1, 2, 3, 4), (5, 6, 7, 8))   and
               C6 = ((1, 2, 3, 4), (1, 2, 3, 4))   and
               C7 = ((1, 2, 1, 2), (1, 2, 1, 2))   and
               C8 = ((1, 2, 1, 2), (1, 2, 3, 4))   and
               C10= (   (1, 2, 3, 4), (5, 6, 7, 8),
                        (9, 0, 1, 2), (3, 4, 5, 6))   )
      report "***FAILED TEST: c04s03b01x01p02n01i00045 - Syntactic error in constant declaration."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b01x01p02n01i00045arch;
