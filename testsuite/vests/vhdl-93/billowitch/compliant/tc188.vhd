
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
-- $Id: tc188.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s04b00x00p13n01i00188ent IS
  port ( S2 : in    integer;
         V2 : inout Real    ) ;
  
  attribute    V1 : REAL;
  attribute    V1 of V2 : signal is 1.0;
  alias    A2 : real is V2;
  
  attribute    S1 : INTEGER;
  attribute    S1 of S2 : signal is 1;
  alias    A1 : integer is S2;
END c04s04b00x00p13n01i00188ent;

ARCHITECTURE c04s04b00x00p13n01i00188arch OF c04s04b00x00p13n01i00188ent IS

BEGIN
  TESTING: PROCESS
    subtype    BTRUE    is BOOLEAN range TRUE to TRUE;
    variable    B1    : BTRUE;
  BEGIN
    assert NOT( (A1'S1 = S2'S1) and (A2'V1 = V2'V1) )
      report "***PASSED TEST: c04s04b00x00p13n01i00188"
      severity NOTE;
    assert ( (A1'S1 = S2'S1) and (A2'V1 = V2'V1) )
      report "***FAILED TEST: c04s04b00x00p13n01i00188 - Attribute of an object applies to any alias of the object."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s04b00x00p13n01i00188arch;
