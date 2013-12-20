
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
-- $Id: tc1720.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s06b01x00p01n02i01720ent IS
END c12s06b01x00p01n02i01720ent;

ARCHITECTURE c12s06b01x00p01n02i01720arch OF c12s06b01x00p01n02i01720ent IS

  -- Global type declaration.
  type NIBBLE is array( 0 to 3 ) of BIT;

  -- Global signals.
  SIGNAL B   : BIT := '1';
  SIGNAL N   : NIBBLE := B"1111";

BEGIN
  TESTING: PROCESS
  BEGIN
    -- If one driver created, it will take on the indicated value.
    B <= '0' after 10 ns;
    N <= B"0000" after 10 ns;
    wait on N,B;
    assert NOT( B='0' and N=B"0000" )
      report "***PASSED TEST: c12s06b01x00p01n02i01720"
      severity NOTE;
    assert ( B='0' and N=B"0000" )
      report "***FAILED TEST: c12s06b01x00p01n02i01720 - At least one driver gets created for eah signal which is assigned to either directly or indirectly inside of a process."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c12s06b01x00p01n02i01720arch;
