
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
-- $Id: tc2758.vhd,v 1.2 2001-10-26 16:29:49 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c13s07b00x00p05n01i02758ent IS
END c13s07b00x00p05n01i02758ent;

ARCHITECTURE c13s07b00x00p05n01i02758arch OF c13s07b00x00p05n01i02758ent IS
  constant   bcap : bit_vector := x"F_F_F";
  constant   blow : bit_vector := x"FFF";
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT( bcap=blow )
      report "***PASSED TEST: c13s07b00x00p05n01i02758"
      severity NOTE;
    assert ( bcap=blow )
      report "***FAILED TEST: c13s07b00x00p05n01i02758 - Underscore in bit string value should not change the value."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c13s07b00x00p05n01i02758arch;
