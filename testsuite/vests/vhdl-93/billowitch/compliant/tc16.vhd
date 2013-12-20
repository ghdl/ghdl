
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
-- $Id: tc16.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s02b00x00p06n03i00016ent IS
END c04s02b00x00p06n03i00016ent;

ARCHITECTURE c04s02b00x00p06n03i00016arch OF c04s02b00x00p06n03i00016ent IS

BEGIN
  TESTING: PROCESS
    -- Define a subtype of a subtype.
    subtype ZERO is NATURAL;

    -- Define variables of these subtypes.
    variable ZEROV     : ZERO := 0;
    variable NATURALV  : NATURAL := 0;
  BEGIN
    -- Verify that these two variables have the same base type.
    assert NOT( Naturalv = zerov and zerov = zero'low )
      report "***PASSED TEST: c04s02b00x00p06n03i00016"
      severity NOTE;
    assert ( Naturalv = zerov and zerov = zero'low )
      report "***FAILED TEST: c04s02b00x00p06n03i00016 - The base type of a subtype is the base type of the type mark."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s02b00x00p06n03i00016arch;
