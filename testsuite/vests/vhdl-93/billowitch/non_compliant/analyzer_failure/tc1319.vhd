
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
-- $Id: tc1319.vhd,v 1.2 2001-10-26 16:30:09 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s04b00x00p08n01i01319ent IS
END c08s04b00x00p08n01i01319ent;

ARCHITECTURE c08s04b00x00p08n01i01319arch OF c08s04b00x00p08n01i01319ent IS
  type aggsig is array (1 to 4) of bit;
  signal S  : aggsig;
  signal S1 : bit;
  signal S2 : bit;
  signal S3 : bit;
  signal S4 : bit;
BEGIN
  TESTING: PROCESS
  BEGIN
    S <= (bit'('0'), bit'('1'), bit'('0'),bit'('1'));
    (S1, S2, S1, S4) <= S;
    assert FALSE 
      report "***FAILED TEST: c08s04b00x00p08n01i01319 - Signal is identified as target more than once in the same assignment."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s04b00x00p08n01i01319arch;
