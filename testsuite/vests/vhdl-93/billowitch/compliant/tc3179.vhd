
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
-- $Id: tc3179.vhd,v 1.2 2001-10-26 16:29:52 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c14s01b00x00p71n01i03179ent IS
END c14s01b00x00p71n01i03179ent;

ARCHITECTURE c14s01b00x00p71n01i03179arch OF c14s01b00x00p71n01i03179ent IS
  subtype fourbit is integer range 0 to 15;
  subtype roufbit is integer range 15 downto 0;
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT(    fourbit'pred(15) = 14   and
                   roufbit'pred(15) = 14   )
      report "***PASSED TEST: c14s01b00x00p71n01i03179"
      severity NOTE;
    assert (    fourbit'pred(15) = 14   and
                roufbit'pred(15) = 14   )
      report "***FAILED TEST: c14s01b00x00p71n01i03179 - Predefined attribute PRED for integer subtype test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c14s01b00x00p71n01i03179arch;
