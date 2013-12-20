
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
-- $Id: tc1495.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s08b00x00p09n01i01495ent IS
END c08s08b00x00p09n01i01495ent;

ARCHITECTURE c08s08b00x00p09n01i01495arch OF c08s08b00x00p09n01i01495ent IS
  SUBTYPE string_30 is STRING(1 to 30);
  SUBTYPE string_4  is STRING(1 to 4);
BEGIN
  TESTING: PROCESS
    VARIABLE str : string_30 := "1234567890abcdefghijlkmnopqrst";
    variable k   : integer   := 0;
  BEGIN
    case string_4'(str(1 to 4)) is
      when "1234" =>   k := 5; 
      when OTHERS =>   k := 6; 
    end case;
    assert NOT(k=5) 
      report "***PASSED TEST: c08s08b00x00p09n01i01495" 
      severity NOTE;
    assert (k=5) 
      report "***FAILED TEST: c08s08b00x00p09n01i01495 - Expression being a qualified expression failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s08b00x00p09n01i01495arch;
