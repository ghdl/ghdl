
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
-- $Id: tc1506.vhd,v 1.2 2001-10-26 16:30:10 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s08b00x00p14n04i01506ent IS
END c08s08b00x00p14n04i01506ent;

ARCHITECTURE c08s08b00x00p14n04i01506arch OF c08s08b00x00p14n04i01506ent IS

BEGIN
  TESTING: PROCESS
    type day is (sun,mon,tue,wed,thu,fri,sat);

    type rec_type is
      record 
        element : day;
      end record;

    variable s_day ; day;
  BEGIN
    case s_day is
      when sun   => NULL;
      when mon   => NULL;
      when elements   => NULL;
      when others   => NULL;
    end case;
    assert FALSE 
      report "***FAILED TEST: c08s08b00x00p14n04i01506 - A simple name is not allowed as an alternative in a CASE statement"
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s08b00x00p14n04i01506arch;
