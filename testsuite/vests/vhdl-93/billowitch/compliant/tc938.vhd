
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
-- $Id: tc938.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c10s04b00x00p07n01i00938pkg is
  type color is ( red, white, green, blue );
  constant x : color := green;
end c10s04b00x00p07n01i00938pkg;

ENTITY c10s04b00x00p07n01i00938ent IS
END c10s04b00x00p07n01i00938ent;

ARCHITECTURE c10s04b00x00p07n01i00938arch OF c10s04b00x00p07n01i00938ent IS
  type    rgb is ( red, green, blue );
  constant    x : rgb := red; -- homograph of x
  use work.c10s04b00x00p07n01i00938pkg.all;
  procedure xxx is
  begin
    -- takes the local x
    assert (x = red) report "x /= red" severity FAILURE;
    assert NOT( x=red )
      report "***PASSED TEST: c10s04b00x00p07n01i00938"
      severity NOTE;
    assert ( x=red )
      report "***FAILED TEST: c10s04b00x00p07n01i00938 - A oitentially visible declaration is made visible within the immediate scope of a homograph if the declaration is an enumeration literal."
      severity ERROR;
  end xxx;
BEGIN
  xxx;
END c10s04b00x00p07n01i00938arch;
