
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
-- $Id: tc1710.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s02b00x00p10n01i01710ent IS
END c09s02b00x00p10n01i01710ent;

ARCHITECTURE c09s02b00x00p10n01i01710arch OF c09s02b00x00p10n01i01710ent IS
  -- architecture declaration section
BEGIN
  -- architecture statement part
  TESTING: PROCESS
  BEGIN
    -- testcase code
    Assert FALSE
      Report "***PASSED TEST: c09s02b00x00p10n01i01710"
      Severity NOTE;
    -- testcase code
    Assert FALSE
      Report "***FAILED TEST: c09s02b00x00p10n01i01710"
      Severity ERROR;
    wait; -- forever
  END PROCESS TESTING;
END c09s02b00x00p10n01i01710arch;

-- CONFIGURATION c09s02b00x00p10n01i01710cfg OF c09s02b00x00p10n01i01710ent IS
--     FOR c09s02b00x00p10n01i01710arch
--     END FOR;
-- END c09s02b00x00p10n01i01710cfg;

