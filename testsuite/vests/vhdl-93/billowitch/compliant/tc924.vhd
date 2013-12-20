
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
-- $Id: tc924.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c10s03b00x00p20n03i00924pkg is
  type primary is ( red, green, blue );
end c10s03b00x00p20n03i00924pkg;


ENTITY c10s03b00x00p20n03i00924ent IS
  type primary is ( yellow, pink, orange );
END c10s03b00x00p20n03i00924ent;

ARCHITECTURE c10s03b00x00p20n03i00924arch OF c10s03b00x00p20n03i00924ent IS
  procedure xxx is
    use    work.c10s03b00x00p20n03i00924pkg.all;
    variable x : work.c10s03b00x00p20n03i00924pkg.primary;
  begin
    x := red;
    assert NOT( x=red )
      report "***PASSED TEST: c10s03b00x00p20n03i00924"
      severity NOTE;
    assert ( x=red )
      report "***FAILED TEST: c10s03b00x00p20n03i00924 - A use clause can make a declaration visible and hide a local declaration."
      severity ERROR;
  end xxx;
BEGIN
  xxx;

END c10s03b00x00p20n03i00924arch;
