
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
-- $Id: tc1206.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s01b00x00p08n04i01206ent IS
END c08s01b00x00p08n04i01206ent;

ARCHITECTURE c08s01b00x00p08n04i01206arch OF c08s01b00x00p08n04i01206ent IS
  signal cll : integer := 0;
  signal del : integer := 0;
BEGIN
  TESTING: PROCESS
  BEGIN
    cll   <= 5 after 55 ns;
    del   <= 5 after 55 ns;   
    wait until (cll = 5 or  del = 5);
    assert NOT( cll=5 )
      report "***PASSED TEST: c08s01b00x00p08n04i01206"
      severity NOTE;
    assert ( cll=5 )
      report "***FAILED TEST: c08s01b00x00p08n04i01206 - if no sensitivity clause appears, the sensitivity set will contain the signals denoted by the longest static prefix of each signal name that appears as a primary in the condition of the condirion clause."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s01b00x00p08n04i01206arch;
