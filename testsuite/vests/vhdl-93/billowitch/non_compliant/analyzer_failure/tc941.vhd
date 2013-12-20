
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
-- $Id: tc941.vhd,v 1.2 2001-10-26 16:30:28 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c10s05b00x00p03n02i00941pkg1 is
  type COLOR is (RED,YELLOW,GREEN,BROWN,TAN,WHITE,BLUE);
end c10s05b00x00p03n02i00941pkg1;

package c10s05b00x00p03n02i00941pkg2 is
  type LIGHTS is (RED,YELLOW,GREEN,BROWN,TAN,WHITE,BLUE);
end c10s05b00x00p03n02i00941pkg2;

use work.c10s05b00x00p03n02i00941pkg1.all, work c10s05b00x00p03n02i00941pkg2.all;
ENTITY c10s05b00x00p03n02i00941ent IS
END c10s05b00x00p03n02i00941ent;

ARCHITECTURE c10s05b00x00p03n02i00941arch OF c10s05b00x00p03n02i00941ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    if RED > BLUE then  -- Failure_here
      --ERROR: type cannot be determined from context
    else
      case TRUE is
        when (TAN = TAN) => null;  -- Failure_here
        --ERROR: type cannot be determined from context
        when others  => null; -- Failure_here
      end case;
    end if;
    assert FALSE 
      report "***FAILED TEST: c10s05b00x00p03n02i00941 - Multiple interpretations of constituents of the innermost complete context are not allowed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c10s05b00x00p03n02i00941arch;
