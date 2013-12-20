
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
-- $Id: tc2559.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b06x00p05n01i02559ent IS
END c07s03b06x00p05n01i02559ent;

ARCHITECTURE c07s03b06x00p05n01i02559arch OF c07s03b06x00p05n01i02559ent IS

BEGIN
  TESTING: PROCESS
    type CELL;
    type LINK is access CELL;
    type CELL is
      record
        VALUE : Bit;
        SUCC  : Bit;
      end record;
    type T1 is access BIT_VECTOR ;
    variable HEAD : LINK := new CELL'('1','0') ;
    variable V2   : T1   := new BIT_VECTOR(0 to 7) ;     --- No_failure_here
  BEGIN
    assert NOT((V2.all="00000000") and (HEAD.VALUE='1') and (HEAD.SUCC='0'))
      report "***PASSED TEST: c07s03b06x00p05n01i02559"
      severity NOTE;
    assert ((V2.all="00000000") and (HEAD.VALUE='1') and (HEAD.SUCC='0'))
      report "***FAILED TEST: c07s03b06x00p05n01i02559 - "
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b06x00p05n01i02559arch;
