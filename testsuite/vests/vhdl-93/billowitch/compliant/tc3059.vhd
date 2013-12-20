
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
-- $Id: tc3059.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s03b01x05p01n02i03059ent IS
END c12s03b01x05p01n02i03059ent;

ARCHITECTURE c12s03b01x05p01n02i03059arch OF c12s03b01x05p01n02i03059ent IS
  signal R_NUM : BIT_VECTOR(0 to 15) := "1010101001010101";
  alias  NUMB  : BIT_VECTOR(7 downto 0) is R_NUM(8 to 15);
BEGIN
  TESTING: PROCESS
  BEGIN
    wait for 5 ns;
    assert NOT( NUMB = "01010101" )
      report "***PASSED TEST: c12s03b01x05p01n02i03059"
      severity NOTE;
    assert ( NUMB = "01010101" )
      report "***FAILED TEST: c12s03b01x05p01n02i03059 - Alias for an array object has a matching element for each element of the named object."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c12s03b01x05p01n02i03059arch;
