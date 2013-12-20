
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
-- $Id: tc1126.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s05b00x00p04n01i01126ent IS
END c06s05b00x00p04n01i01126ent;

ARCHITECTURE c06s05b00x00p04n01i01126arch OF c06s05b00x00p04n01i01126ent IS

BEGIN
  TESTING: PROCESS
    type    FIVE1 is range 1 to 5;
    type    FIVE2 is range 1 to 5;
    type    A3B   is array (FIVE1 range <>) of BOOLEAN;
    subtype    A3    is A3B   (FIVE1);
    type    A4B   is array (FIVE2 range <>) of A3;
    subtype    A4    is A4B   (FIVE2);

    variable    V4: A4 ;
  BEGIN
    V4(3)(1 to 5) := V4(4)(FIVE1);   -- legal assignments.
    assert NOT(V4(3)(1 to 5) = (false,false,false,false,false)) 
      report "***PASSED TEST: c06s05b00x00p04n01i01126" 
      severity NOTE;
    assert (V4(3)(1 to 5) = (false,false,false,false,false)) 
      report "***FAILED TEST: c06s05b00x00p04n01i01126 - Bounds of the discrete range must be the type of the index of the array." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s05b00x00p04n01i01126arch;
