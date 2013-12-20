
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
-- $Id: tc1465.vhd,v 1.2 2001-10-26 16:30:10 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s08b00x00p02n01i01465ent IS
END c08s08b00x00p02n01i01465ent;

ARCHITECTURE c08s08b00x00p02n01i01465arch OF c08s08b00x00p02n01i01465ent IS

BEGIN
  TESTING: PROCESS
    variable x : integer := 1;
    variable k : integer := 0;
  BEGIN
    case  is 
      when 1 => k := 5;
    when 2 => NULL;
    when 3 => NULL;
    when others => NULL;
  end case;
  assert FALSE 
    report "***FAILED TEST: c08s08b00x00p02n01i01465 - missing expression after the reserved word 'case'"
    severity ERROR;
  wait;
END PROCESS TESTING;

END c08s08b00x00p02n01i01465arch;
