
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
-- $Id: tc1551.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s09b00x00p10n03i01551ent IS
END c08s09b00x00p10n03i01551ent;

ARCHITECTURE c08s09b00x00p10n03i01551arch OF c08s09b00x00p10n03i01551ent IS

  type t1 is (a,b);
  type t2 is (b,c);
  type t3 is (c,d);
BEGIN
  TESTING: PROCESS
    variable k : integer := 0;
  BEGIN
    for i in c downto b loop
      k := 5;
    end loop;
    assert NOT( k=5 )
      report "***PASSED TEST: c08s09b00x00p10n03i01551"
      severity NOTE;
    assert ( k=5 )
      report "***FAILED TEST: c08s09b00x00p10n03i01551 - Each iteration of a loop statement with a for iteration scheme, the corresponding value of the discrete range is assigned to the loop parameter, these values are assigned in left to rigth order"
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s09b00x00p10n03i01551arch;
