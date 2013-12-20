
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
-- $Id: tc2678.vhd,v 1.2 2001-10-26 16:30:21 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c13s03b01x00p05n02i02678ent IS
END c13s03b01x00p05n02i02678ent;

ARCHITECTURE c13s03b01x00p05n02i02678arch OF c13s03b01x00p05n02i02678ent IS
  constant   UPPER_CASE : integer := 27;
  -- ERROR: double declaration due to case insensitivity;
  signal       upper_case:integer;     -- failure_here.
BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c13s03b01x00p05n02i02678 - Identifiers differing only in the use of corresponding upper and lower case letters are considered as the same."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c13s03b01x00p05n02i02678arch;
