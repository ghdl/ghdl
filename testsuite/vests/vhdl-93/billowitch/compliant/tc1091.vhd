
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
-- $Id: tc1091.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c06s05b00x00p02n01i01091pkg is
  type FIVE    is range 1 to 5;
  type ABASE    is array (FIVE range <>) of BOOLEAN;
  subtype A1    is ABASE(FIVE);
  attribute    AT1 : A1;
  function    fat1(i:integer) return a1;
end c06s05b00x00p02n01i01091pkg;

package body c06s05b00x00p02n01i01091pkg is
  function fat1(i:integer) return a1 is
    variable va1 : a1;
  begin
    return Va1;
  end fat1;
end c06s05b00x00p02n01i01091pkg;

use work.c06s05b00x00p02n01i01091pkg.all;
ENTITY c06s05b00x00p02n01i01091ent IS
  port (PT: BOOLEAN) ;

  attribute AT1 of PT : signal is fat1(8);
END c06s05b00x00p02n01i01091ent;

ARCHITECTURE c06s05b00x00p02n01i01091arch OF c06s05b00x00p02n01i01091ent IS

BEGIN
  TESTING: PROCESS
    variable V1 : A1;
  BEGIN
    V1(2 to 4) := PT'AT1(2 to 4);
    assert NOT(V1(2 to 4)=(false,false,false))
      report "***PASSED TEST: c06s05b00x00p02n01i01091"
      severity NOTE;
    assert (V1(2 to 4)=(false,false,false))
      report "***FAILED TEST: c06s05b00x00p02n01i01091 - Slice name consists of a single discrete range enclosed within parentheses."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s05b00x00p02n01i01091arch;
