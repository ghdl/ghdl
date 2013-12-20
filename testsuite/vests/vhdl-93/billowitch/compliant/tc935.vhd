
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
-- $Id: tc935.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c10s04b00x00p06n01i00935pkg is
  constant x : integer := 10;
end c10s04b00x00p06n01i00935pkg;

ENTITY c10s04b00x00p06n01i00935ent IS
END c10s04b00x00p06n01i00935ent;

ARCHITECTURE c10s04b00x00p06n01i00935arch OF c10s04b00x00p06n01i00935ent IS
  procedure xxx is
    constant x : integer := 5; -- homograph of x
    -- here we place the declaration after the local homograph !
    use work.c10s04b00x00p06n01i00935pkg.all;
  begin
    assert NOT( x=5 )
      report "***PASSED TEST: c10s04b00x00p06n01i00935"
      severity NOTE;
    assert ( x=5 )
      report "***FAILED TEST: c10s04b00x00p06n01i00935 - A potentially visible declaration is not visible within the immediate scope of a homograph."
      severity ERROR;
  end xxx;
BEGIN
  xxx;

END c10s04b00x00p06n01i00935arch;
