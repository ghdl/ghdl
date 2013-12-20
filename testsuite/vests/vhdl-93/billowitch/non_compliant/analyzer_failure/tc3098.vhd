
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
-- $Id: tc3098.vhd,v 1.2 2001-10-26 16:30:25 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c05s01b00x00p09n02i03098ent IS
END c05s01b00x00p09n02i03098ent;

ARCHITECTURE c05s01b00x00p09n02i03098arch OF c05s01b00x00p09n02i03098ent IS

BEGIN
  TESTING: PROCESS
    attribute    ATT    : integer;
    type       T1    is range 1 to 100000 ;
    variable    V1    : Integer := 0 ;
    attribute    Att    of T1,V1 : type is 2 ;  -- Failure_here
    -- ERROR: only name which belong to the entity class are permitted in an entity name list.
  BEGIN
    assert FALSE
      report "***FAILED TEST: c05s01b00x00p09n02i03098 - Entity name does not belong to entity name list."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c05s01b00x00p09n02i03098arch;
