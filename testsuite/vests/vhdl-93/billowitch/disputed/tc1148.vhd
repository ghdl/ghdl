
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
-- $Id: tc1148.vhd,v 1.2 2001-10-26 16:30:03 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s05b00x00p07n01i01148ent IS
END c06s05b00x00p07n01i01148ent;

ARCHITECTURE c06s05b00x00p07n01i01148arch OF c06s05b00x00p07n01i01148ent IS
  type   A is array (10 downto 1) of integer;
BEGIN
  TESTING: PROCESS
    variable var : A := (66,66,others=>66);
  BEGIN
    wait for 5 ns;
    assert NOT( var(1 downto 1) = 66 )
      report "***PASSED TEST: c06s05b00x00p07n01i01148"
      severity NOTE;
    assert ( var(1 downto 1) = 66 )
      report "***FAILED TEST: c06s05b00x00p07n01i01148 - A(N downto N) should be a slice that contains one element."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s05b00x00p07n01i01148arch;
