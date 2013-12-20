
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
-- $Id: tc2332.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b07x00p02n02i02332ent IS
END c07s02b07x00p02n02i02332ent;

ARCHITECTURE c07s02b07x00p02n02i02332arch OF c07s02b07x00p02n02i02332ent IS
  type NEW_INT is range INTEGER'LOW to INTEGER'HIGH;
  signal k : NEW_INT := 5;
BEGIN
  TESTING: PROCESS
  BEGIN
    k <= 2 ** 2;
    wait for 1 ns;
    assert NOT(k=4) 
      report "***PASSED TEST: c07s02b07x00p02n02i02332" 
      severity NOTE;
    assert (k=4) 
      report "***FAILED TEST: c07s02b07x00p02n02i02332 - Exponent can only be of type Integer."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b07x00p02n02i02332arch;
