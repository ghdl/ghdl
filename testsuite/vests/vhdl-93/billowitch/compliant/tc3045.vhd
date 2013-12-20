
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
-- $Id: tc3045.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s02b02x00p02n03i03045ent IS
END c12s02b02x00p02n03i03045ent;

ARCHITECTURE c12s02b02x00p02n03i03045arch OF c12s02b02x00p02n03i03045ent IS

BEGIN
  bl1: block
    generic (i1:integer; i2:integer; i3:integer; i4:integer);
    generic map(3, -5, i4=>-4, i3=>6);
  begin
    assert (i1=3)
      report "Generic association for first element I1 incorrect"
      severity failure;
    assert (i2=-5)
      report "Generic association for second element I2 incorrect"
      severity failure;
    assert (i3=6)
      report "Generic association for third element I3 incorrect"
      severity failure;
    assert (i4=-4)
      report "Generic association for fourth element I4 incorrect"
      severity failure;
    assert NOT( i1=3 and i2=-5 and i3=6 and i4=-4 )
      report "***PASSED TEST: c12s02b02x00p02n03i03045"
      severity NOTE;
    assert ( i1=3 and i2=-5 and i3=6 and i4=-4 )
      report "***FAILED TEST: c12s02b02x00p02n03i03045 - Named association and positional association of generics creates constnats without the correct values."
      severity ERROR;
  end block;

END c12s02b02x00p02n03i03045arch;
