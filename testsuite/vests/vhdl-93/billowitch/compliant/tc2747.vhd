
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
-- $Id: tc2747.vhd,v 1.2 2001-10-26 16:29:49 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c13s07b00x00p02n01i02747ent IS
END c13s07b00x00p02n01i02747ent;

ARCHITECTURE c13s07b00x00p02n01i02747arch OF c13s07b00x00p02n01i02747ent IS
  type       x1 is array (1 to 10) of bit;
  constant    v1 : x1 := B"00_11_00_11_00";
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT(v1 = B"00_11_00_11_00") 
      report "***PASSED TEST: c13s07b00x00p02n01i02747" 
      severity NOTE;
    assert (v1 = B"00_11_00_11_00") 
      report "***FAILED TEST: c13s07b00x00p02n01i02747 - A bit string literal consists of a sequence of extended digits enclosed between two quotations and is preceded by a base specifier." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c13s07b00x00p02n01i02747arch;
