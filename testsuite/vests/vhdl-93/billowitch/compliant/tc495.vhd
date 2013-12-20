
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
-- $Id: tc495.vhd,v 1.2 2001-10-26 16:29:55 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b02x00p01n01i00495ent IS
END c03s02b02x00p01n01i00495ent;

ARCHITECTURE c03s02b02x00p01n01i00495arch OF c03s02b02x00p01n01i00495ent IS

BEGIN
  TESTING: PROCESS
    type tRecord1 is
      record
        element1 : INTEGER;
        element2 : CHARACTER;
      end record;

    type tRecord2 is
      record
        element3 : INTEGER;
        element4 : CHARACTER;
        element5 : tRecord1;
      end record;

    variable V1 : tRecord1 := (1, '1');
    variable V2 : tRecord2 := (2, '2', (3, '3'));
  BEGIN
    assert V1.element1 = 1;
    assert V1.element2 = '1';
    assert V2.element3 = 2;
    assert V2.element4 = '2';
    assert V2.element5.element1 = 3;
    assert V2.element5.element2 = '3';
    wait for 1 ns;
    assert NOT(   V1.element1 = 1      and
                  V1.element2 = '1'   and
                  V2.element3 = 2      and
                  V2.element4 = '2'   and
                  V2.element5.element1 = 3   and
                  V2.element5.element2 = '3'   ) 
      report "***PASSED TEST: c03s02b02x00p01n01i00495"
      severity NOTE;
    assert (   V1.element1 = 1      and
               V1.element2 = '1'   and
               V2.element3 = 2      and
               V2.element4 = '2'   and
               V2.element5.element1 = 3   and
               V2.element5.element2 = '3'   ) 
      report "***FAILED TEST: c03s02b02x00p01n01i00495 - Record type in record type declartion test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b02x00p01n01i00495arch;
