
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
-- $Id: tc1306.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s04b00x00p06n01i01306ent IS
END c08s04b00x00p06n01i01306ent;

ARCHITECTURE c08s04b00x00p06n01i01306arch OF c08s04b00x00p06n01i01306ent IS
  signal   S1 : BIT := '1';
  type REC_1 is record
                  RE_1:BIT;
                  RE_2:INTEGER;
                end record;
  signal  S3 : REC_1;
BEGIN
  TESTING: PROCESS
  BEGIN
    S3.RE_1 <= S1;
    wait for 1 ns;
    assert NOT(S3.RE_1 = '1') 
      report "***PASSED TEST: c08s04b00x00p06n01i01306" 
      severity NOTE;
    assert (S3.RE_1 = '1') 
      report "***FAILED TEST: c08s04b00x00p06n01i01306 - A indexed name can be used on the left-hand side of a signal assignment." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s04b00x00p06n01i01306arch;
