
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
-- $Id: tc1075.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s04b00x00p03n02i01075ent IS
END c06s04b00x00p03n02i01075ent;

ARCHITECTURE c06s04b00x00p03n02i01075arch OF c06s04b00x00p03n02i01075ent IS

BEGIN
  TESTING: PROCESS
    type CSTRING is array (CHARACTER range <>) of CHARACTER;

    constant C1 : CSTRING('A' to 'H') := "BCDEFGHA";
    constant C2 : CSTRING('A' to 'H') := "CDEFGHAB";
    constant C3 : CSTRING('A' to 'H') := "DEFGHABC";

    variable V1 : CHARACTER;
    variable V2 : CHARACTER;
    variable V3 : CHARACTER;
  BEGIN
    V1 := C1('A');          --      A -> B
    assert V1 = 'B';
    V2 := C2(C1('A'));      --      A -> B -> D
    assert V2 = 'D';
    V3 := C3(C2(C1('A')));  --      A -> B -> H
    assert V3 = 'G';
    wait for 5 ns;
    assert NOT( V1 = 'B' and V2 = 'D' and V3 = 'G' ) 
      report "***PASSED TEST: c06s04b00x00p03n02i01075" 
      severity NOTE;
    assert ( V1 = 'B' and V2 = 'D' and V3 = 'G' ) 
      report "***FAILED TEST: c06s04b00x00p03n02i01075 - The expresion for index name check test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s04b00x00p03n02i01075arch;
