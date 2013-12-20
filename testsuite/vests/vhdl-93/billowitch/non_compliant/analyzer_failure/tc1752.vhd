
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
-- $Id: tc1752.vhd,v 1.2 2001-10-26 16:30:12 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s05b00x00p25n01i01752ent IS
  generic (g1: integer := 12);
  port (
    input1: in  bit ;
    input2: in  bit ;
    clk   : in  boolean;
    output: out bit);
END c09s05b00x00p25n01i01752ent;

ARCHITECTURE c09s05b00x00p25n01i01752arch OF c09s05b00x00p25n01i01752ent IS
  type a is array (1 to 4) of boolean;
  signal i    :  a;
BEGIN
  (i(g1), i(2), i(3), i(4)) <= a'(true, false, false, true);
  -- Failure_here : i(g1) is not a locally static name
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c09s05b00x00p25n01i01752 - Only locally static signal names may contain here." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c09s05b00x00p25n01i01752arch;
