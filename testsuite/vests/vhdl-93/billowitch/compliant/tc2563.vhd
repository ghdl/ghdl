
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
-- $Id: tc2563.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b06x00p06n03i02563ent IS
END c07s03b06x00p06n03i02563ent;

ARCHITECTURE c07s03b06x00p06n03i02563arch OF c07s03b06x00p06n03i02563ent IS

BEGIN
  TESTING: PROCESS
    type MEM is array (Integer range <>) of Integer;
    subtype T1 is MEM (1 to 6) ;
    type LINK is access BIT_VECTOR;
    variable HEAD : LINK :=  new BIT_VECTOR'("00110110") ;
  BEGIN
    assert NOT(HEAD'HIGH=7)
      report "***PASSED TEST: c07s03b06x00p06n03i02563"
      severity NOTE;
    assert (HEAD'HIGH=7)
      report "***FAILED TEST: c07s03b06x00p06n03i02563 - The created object is con strained by the bounds of the initial value defined by that expression."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b06x00p06n03i02563arch;
