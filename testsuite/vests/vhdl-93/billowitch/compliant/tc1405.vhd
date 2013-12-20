
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
-- $Id: tc1405.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s05b00x00p07n01i01405ent IS
END c08s05b00x00p07n01i01405ent;

ARCHITECTURE c08s05b00x00p07n01i01405arch OF c08s05b00x00p07n01i01405ent IS

BEGIN
  TESTING: PROCESS
    variable S1 : BIT;
    variable T1 : BIT;
    variable T2 : BIT;
    variable B2 : BIT_VECTOR(0 to 2) := B"111";
  BEGIN
    (S1, T1, T2) := B2;
    assert NOT( (S1='1') and (T1='1') and (T2='1') )
      report "***PASSED TEST: c08s05b00x00p07n01i01405"
      severity NOTE;
    assert ( (S1='1') and (T1='1') and (T2='1') )
      report "***FAILED TEST: c08s05b00x00p07n01i01405 - Subtypes of the subelements of the right-hand side and that of the names in the aggregate should match"
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s05b00x00p07n01i01405arch;
