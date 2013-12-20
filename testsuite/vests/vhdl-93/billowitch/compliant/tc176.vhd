
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
-- $Id: tc176.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b03x01p03n02i00176ent IS
END c04s03b03x01p03n02i00176ent;

ARCHITECTURE c04s03b03x01p03n02i00176arch OF c04s03b03x01p03n02i00176ent IS

BEGIN
  TESTING: PROCESS
    variable REAL_NUMBER: BIT_VECTOR(0 to 31);
    alias    SIGN:        bit          is REAL_NUMBER(0);
    alias    MANTISSA:    BIT_VECTOR(23 downto 0)    is REAL_NUMBER(8 to 31);
    alias    EXPONENT:    BIT_VECTOR(1 to 7)    is REAL_NUMBER(1 to 7);
  BEGIN
    REAL_NUMBER := "00011011000110111110010011100100";
    wait for 10 ns;
    assert NOT(   SIGN   = '0'         and
                  MANTISSA= "000110111110010011100100"and
                  EXPONENT= "0011011"   )
      report "***PASSED TEST:c04s03b03x01p03n02i00176" 
      severity NOTE;
    assert (   SIGN   = '0'         and
               MANTISSA= "000110111110010011100100"and
               EXPONENT= "0011011"   )
      report "***FAILED TEST: c04s03b03x01p03n02i00176 - A single dimensional array test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b03x01p03n02i00176arch;
