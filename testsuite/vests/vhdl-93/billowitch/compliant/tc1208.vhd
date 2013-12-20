
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
-- $Id: tc1208.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s01b00x00p24n01i01208ent IS
END c08s01b00x00p24n01i01208ent;

ARCHITECTURE c08s01b00x00p24n01i01208arch OF c08s01b00x00p24n01i01208ent IS
  type    WOR is array (0 to 3) of BIT;
  signal   TS   : WOR := "0000";
BEGIN
  TESTING: PROCESS
  BEGIN
    TS <= "0101" after 20 ns;
    wait on TS until (TS(1) = '1');
    assert NOT( TS(1) = '1' )
      report "***PASSED TEST: c08s01b00x00p24n01i01208"
      severity NOTE;
    assert ( TS(1) = '1' )
      report "***FAILED TEST: c08s01b00x00p24n01i01208 - Composite signal in teh sensitivity list of the wait statement is equivalent to having each subelement of that composite signal in the list."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s01b00x00p24n01i01208arch;
