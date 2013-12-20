
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
-- $Id: tc218.vhd,v 1.2 2001-10-26 16:29:46 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s01b01x00p06n03i00218ent IS
END c03s01b01x00p06n03i00218ent;

ARCHITECTURE c03s01b01x00p06n03i00218arch OF c03s01b01x00p06n03i00218ent IS
  subtype BTRUE is BOOLEAN range TRUE to TRUE;
  type ENUM1 is (ZERO, ONE, TWO, THREE);
  type ENUM2 is (TRUE, FALSE);
  type ENUM3 is ('1', '0');
  type ENUM4 is ('Z', 'Y', 'X');
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT((ENUM1'POS(ZERO) = 0)
               and (ENUM1'POS(ONE) = 1)
               and (ENUM1'POS(TWO) = 2)
               and (ENUM1'POS(THREE) = 3)
               and (ENUM2'POS(TRUE) = 0)
               and (ENUM2'POS(FALSE) = 1)
               and (ENUM3'POS('1') = 0)
               and (ENUM3'POS('0') = 1)
               and (ENUM4'POS('Z') = 0)
               and (ENUM4'POS('Y') = 1)
               and (ENUM4'POS('X') = 2) )
      report "***PASSED TEST: c03s01b01x00p06n03i00218"
      severity NOTE;
    assert ( (ENUM1'POS(ZERO) = 0)
             and (ENUM1'POS(ONE) = 1)
             and (ENUM1'POS(TWO) = 2)
             and (ENUM1'POS(THREE) = 3)
             and (ENUM2'POS(TRUE) = 0)
             and (ENUM2'POS(FALSE) = 1)
             and (ENUM3'POS('1') = 0)
             and (ENUM3'POS('0') = 1)
             and (ENUM4'POS('Z') = 0)
             and (ENUM4'POS('Y') = 1)
             and (ENUM4'POS('X') = 2))
      report "***FAILED TEST: c03s01b01x00p06n03i00218 - The position value of the nth listed enumeration literal is n-1."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s01b01x00p06n03i00218arch;
