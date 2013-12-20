
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
-- $Id: tc884.vhd,v 1.2 2001-10-26 16:30:01 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c10s01b00x00p09n01i00884ent IS
END c10s01b00x00p09n01i00884ent;

ARCHITECTURE c10s01b00x00p09n01i00884arch OF c10s01b00x00p09n01i00884ent IS

  constant GS1: INTEGER := 105;
  constant GS2: INTEGER := 785;
  signal PS1: INTEGER := 356;
  signal PS2: INTEGER := 123;

BEGIN
  TESTING: PROCESS
    constant GS1: INTEGER := 3;
    constant GS2: INTEGER := 9;
  BEGIN
    PS1 <= GS1 + 1;
    PS2 <= GS2 + 2;
    wait on PS1, PS2;
    assert NOT( PS1=4 and PS2=11 )
      report "***PASSED TEST: c10s01b00x00p09n01i00884"
      severity NOTE;
    assert ( PS1=4 and PS2=11 )
      report "***FAILED TEST: c10s01b00x00p09n01i00884 - A declaration region is formed by the text of a process statement."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c10s01b00x00p09n01i00884arch;
