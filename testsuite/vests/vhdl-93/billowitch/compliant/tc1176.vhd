
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
-- $Id: tc1176.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s00b00x00p01n02i01176ent IS
END c08s00b00x00p01n02i01176ent;

ARCHITECTURE c08s00b00x00p01n02i01176arch OF c08s00b00x00p01n02i01176ent IS
  signal k : integer := 0;
BEGIN
  L1 : process
  begin
    k <= 5;
    wait for 1 ns;
    --  Avoid infinite simulation
    wait;
  end process L1;

  TESTING: PROCESS
  BEGIN
    wait for 5 ns;
    assert NOT(k = 5) 
      report "***PASSED TEST: c08s00b00x00p01n02i01176"
      severity NOTE;
    assert (k = 5) 
      report "***FAILED TEST: c08s00b00x00p01n02i01176 - Sequential statement are executed in the order in which they appear."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s00b00x00p01n02i01176arch;
