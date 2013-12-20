
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
-- $Id: tc1801.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s01b00x00p03n01i01801ent IS
END c07s01b00x00p03n01i01801ent;

ARCHITECTURE c07s01b00x00p03n01i01801arch OF c07s01b00x00p03n01i01801ent IS
  -- architecture declaration section
BEGIN
  -- architecture statement part
  TESTING: PROCESS
  BEGIN
    -- testcase code
    Assert FALSE
      Report "***PASSED TEST: c07s01b00x00p03n01i01801"
      Severity NOTE;
    -- testcase code
    Assert FALSE
      Report "***FAILED TEST: c07s01b00x00p03n01i01801"
      Severity ERROR;
    wait; -- forever
  END PROCESS TESTING;
END c07s01b00x00p03n01i01801arch;

-- CONFIGURATION c07s01b00x00p03n01i01801cfg OF c07s01b00x00p03n01i01801ent IS
--     FOR c07s01b00x00p03n01i01801arch
--     END FOR;
-- END c07s01b00x00p03n01i01801cfg;

