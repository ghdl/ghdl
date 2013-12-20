
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
-- $Id: tc180.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s04b00x00p03n01i00180ent IS
END c04s04b00x00p03n01i00180ent;

ARCHITECTURE c04s04b00x00p03n01i00180arch OF c04s04b00x00p03n01i00180ent IS
  attribute p: POSITIVE;
  signal s: integer;
  attribute p of s: signal is 10;  -- Success_here
BEGIN
  TESTING: PROCESS
  BEGIN
    wait for 10 ns;
    assert NOT( s'p=10 )
      report "***PASSED TEST: c04s04b00x00p03n01i00180"
      severity NOTE;
    assert ( s'p=10 )
      report "***FAILED TEST: c04s04b00x00p03n01i00180 - In attribute declaration, the reserved word attribute must be followed by an identifier, a colon, a type mark and a semicolon."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s04b00x00p03n01i00180arch;
