
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
-- $Id: tc2575.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c13s02b00x00p02n02i02575ent IS
END c13s02b00x00p02n02i02575ent;

ARCHITECTURE c13s02b00x00p02n02i02575arch OF c13s02b00x00p02n02i02575ent IS
  type MEM is range 4 to 5;     -- Space is a separator except in this comment section
  type M1  is range 2 to 4;     -- End of line is a separator between the
  -- earlier comment section and this type.
BEGIN
  TESTING: PROCESS
    variable j : MEM := 4;
    variable n : M1  := 2;
  BEGIN
    assert NOT(j=4 and n=2) 
      report "***PASSED TEST: c13s02b00x00p02n02i02575"
      severity NOTE;
    assert (j=4 and n=2) 
      report "***FAILED TEST: c13s02b00x00p02n02i02575 - Lexical test failed."
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c13s02b00x00p02n02i02575arch;
