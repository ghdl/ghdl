
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
-- $Id: tc3038.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s02b02x00p01n02i03038ent IS
END c12s02b02x00p01n02i03038ent;

ARCHITECTURE c12s02b02x00p01n02i03038arch OF c12s02b02x00p01n02i03038ent IS

BEGIN
  -- test for middle element association
  bl4: block
    generic(i:integer:=10; r:real:=3.4; b:bit:='1');
    generic map(r=>6.7);
  begin
    assert (i=10)
      report "Default value for integer generic not correct"
      severity failure;
    assert (r=6.7)
      report "Generic map value for real generic not correct"
      severity failure;
    assert (b='1')
      report "Default value for bit generic not correct"
      severity failure;

    assert NOT( i=10 and r=6.7 and b='1')
      report "***PASSED TEST: c12s02b02x00p01n02i03038"
      severity NOTE;
    assert ( i=10 and r=6.7 and b='1')
      report "***FAILED TEST: c12s02b02x00p01n02i03038 - The actual part of an implicit association element is the default expression test failed."
      severity ERROR;
  end block;


END c12s02b02x00p01n02i03038arch;
