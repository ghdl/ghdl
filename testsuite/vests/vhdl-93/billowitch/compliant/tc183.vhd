
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
-- $Id: tc183.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c04s04b00x00p04n02i00183pkg is
  attribute    a1    : integer;
  attribute    a2    : integer;
  attribute    a1    of c04s04b00x00p04n02i00183pkg : package is 3;
  constant     c1    : integer  := c04s04b00x00p04n02i00183pkg'a1;
  attribute    a2    of c04s04b00x00p04n02i00183pkg : package is c1 * 2;
  function    fn1    return integer;
  function    fn2    return integer;
end c04s04b00x00p04n02i00183pkg;

package body c04s04b00x00p04n02i00183pkg is
  constant t1 : integer := 3; --testgen'a1;
  constant t2 : integer := 6; --testgen'a2;
  function fn1 return integer is
  begin
    return t1;
  end;
  function fn2 return integer is
  begin
    return t2;
  end;
end c04s04b00x00p04n02i00183pkg;


use work.c04s04b00x00p04n02i00183pkg.all;
ENTITY c04s04b00x00p04n02i00183ent IS
END c04s04b00x00p04n02i00183ent;

ARCHITECTURE c04s04b00x00p04n02i00183arch OF c04s04b00x00p04n02i00183ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    wait for 5 ns;
    assert NOT( c1 = 3 and fn1 = c1 and fn2 = c1+c1 )   
      report "***PASSED TEST: c04s04b00x00p04n02i00183"
      severity NOTE;
    assert ( c1 = 3 and fn1 = c1 and fn2 = c1+c1 )   
      report "***FAILED TEST: c04s04b00x00p04n02i00183 - Package attribute test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s04b00x00p04n02i00183arch;
