
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
-- $Id: tc1121.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s05b00x00p03n02i01121ent IS
END c06s05b00x00p03n02i01121ent;

ARCHITECTURE c06s05b00x00p03n02i01121arch OF c06s05b00x00p03n02i01121ent IS

BEGIN
  TESTING: PROCESS
    type     ENUM1  is (M1, M2, M3, M4, M5, M6);
    type     A1     is array (ENUM1 range <>) of BOOLEAN;
    subtype  A11    is A1 (M1 to M3);
    subtype  A12    is A1 (M4 to M6);
    variable V1     : A1 (M1 to M6) ;
    variable V11    : A11;
    variable V12    : A12;
    variable k      : integer;
  BEGIN
    if (
      (V11 = V12)
      and (V11(M2 to M3) = V12(M4 to M5))
      and (V1 (M1 to M3) = V11(M1 to M3))
      and (V1 (M2 to M3) = V12(M4 to M5))
      ) then
      k := 5;
    end if;
    assert NOT( k=5 )
      report "***PASSED TEST: c06s05b00x00p03n02i01121"
      severity NOTE;
    assert ( k=5 )
      report "***FAILED TEST: c06s05b00x00p03n02i01121 - The type of the slice is the same as the base type of the one-dimensional array."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s05b00x00p03n02i01121arch;
