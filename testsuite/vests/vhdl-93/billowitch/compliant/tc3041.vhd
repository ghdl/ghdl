
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
-- $Id: tc3041.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s02b02x00p01n02i03041ent IS
END c12s02b02x00p01n02i03041ent;

ARCHITECTURE c12s02b02x00p01n02i03041arch OF c12s02b02x00p01n02i03041ent IS
  type c_a is array(integer range <>) of integer;
  type c_r is
    record
      i : integer;
      r : real;
      b : bit;
    end record;
BEGIN
  -- test for first associations
  bl2 : block
    generic(i:c_a(1 to 3):=(10,10,10); r:c_r:=(10,3.4,'1'));
    generic map(i=>(5,5,5));
  begin
    assert ((i(1)=5) and (i(2)=5) and (i(3)=5))
      report "Generic map value for array generic not correct"
      severity failure;
    assert ((r.i=10) and (r.r=3.4) and (r.b='1'))
      report "Default value for record generic not correct"
      severity failure;

    assert NOT((i(1)=5) and (i(2)=5) and (i(3)=5) and (r.i=10) and (r.r=3.4) and (r.b='1'))
      report "***PASSED TEST: c12s02b02x00p01n02i03041"
      severity NOTE;
    assert ((i(1)=5) and (i(2)=5) and (i(3)=5) and (r.i=10) and (r.r=3.4) and (r.b='1'))
      report "***FAILED TEST: c12s02b02x00p01n02i03041 - The actual part of an implicit association element is the default expression test failed."
      severity ERROR;
  end block;


END c12s02b02x00p01n02i03041arch;
