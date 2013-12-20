
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
-- $Id: tc1499.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s08b00x00p14n01i01499ent IS
END c08s08b00x00p14n01i01499ent;

ARCHITECTURE c08s08b00x00p14n01i01499arch OF c08s08b00x00p14n01i01499ent IS

BEGIN
  TESTING: PROCESS
    subtype sub_int is integer range 1 to 2;
    variable V1 : integer;
    variable k1 : integer := 0;
    variable k2 : integer := 0;
    variable k3 : integer := 0;
    variable k4 : integer := 0;
  BEGIN
    V1 := 0;
    case V1 is
      when sub_int'low
        to sub_int'high => assert (false)
                             report "V1 in specified range"
                             severity failure;
      when others       => k1 := 1;
    end case;
    V1 := 1;
    case V1 is
      when sub_int'low
        to sub_int'high => k2 := 1;
      when others       => assert (false)
                             report "V1 NOT in specified range"
                             severity failure;
    end case;
    V1 := 2;
    case V1 is
      when sub_int'low
        to sub_int'high => k3 := 1;
      when others       => assert (false)
                             report "V1 NOT in specified range"
                             severity failure;
    end case;
    V1 := 3;
    case V1 is
      when sub_int'low
        to sub_int'high => assert (false)
                             report "V1 in specified range"
                             severity failure;
      when others       => k4 := 1;
    end case;
    assert NOT(k1=1 and k2=1 and k3=1 and k4=1) 
      report "***PASSED TEST: c08s08b00x00p14n01i01499" 
      severity NOTE;
    assert (k1=1 and k2=1 and k3=1 and k4=1) 
      report "***FAILED TEST: c08s08b00x00p14n01i01499 - A choice can be a discrete range using attributes." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s08b00x00p14n01i01499arch;
