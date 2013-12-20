
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
-- $Id: tc958.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s03b00x00p02n01i00958ent IS
END c06s03b00x00p02n01i00958ent;

ARCHITECTURE c06s03b00x00p02n01i00958arch OF c06s03b00x00p02n01i00958ent IS

BEGIN
  TESTING: PROCESS
    type ONE is range 1 to 1;

    type R0 is record X: ONE; RE: BOOLEAN; end record;            
    type R1 is record X: ONE; RE: R0; end record;
    type R2 is record X: ONE; RE: R1; end record;
    type R3 is record X: ONE; RE: R2; end record;
    type R4 is record X: ONE; RE: R3; end record;
    type R5 is record X: ONE; RE: R4; end record;
    type R6 is record X: ONE; RE: R5; end record;
    type R7 is record X: ONE; RE: R6; end record;
    type R8 is record X: ONE; RE: R7; end record;
    type R9 is record X: ONE; RE: R8; end record;

    variable V1: R9;
  BEGIN
    assert NOT(V1.RE.RE.RE.RE.RE.RE.RE.RE.RE.RE = false)
      report "***PASSED TEST: c06s03b00x00p02n01i00958"
      severity NOTE;
    assert (V1.RE.RE.RE.RE.RE.RE.RE.RE.RE.RE = false)
      report "***FAILED TEST: c06s03b00x00p02n01i00958 - The selected name consists of a prefix, a dot (.), and a suffix."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s03b00x00p02n01i00958arch;
