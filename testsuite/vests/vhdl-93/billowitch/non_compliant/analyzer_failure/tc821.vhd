
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
-- $Id: tc821.vhd,v 1.2 2001-10-26 16:30:28 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c01s02b02x00p02n01i00821ent IS
END c01s02b02x00p02n01i00821ent;

ARCHITECTURE c01s02b02x00p02n01i00821arch OF c01s02b02x00p02n01i00821ent IS
  signal err : boolean := true;
BEGIN

  case err is                           -- illegal location for case statement
    when true | false =>
      assert false
        report "'case' statement accepted in an entity statement."
        severity note ;
  end case;

  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c01s02b02x00p02n01i00821 - Architecture statement can only have concurrent statement."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c01s02b02x00p02n01i00821arch;
