
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
-- $Id: tc2264.vhd,v 1.2 2001-10-26 16:29:46 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b06x00p11n01i02264ent IS
END c07s02b06x00p11n01i02264ent;

ARCHITECTURE c07s02b06x00p11n01i02264arch OF c07s02b06x00p11n01i02264ent IS

BEGIN
  TESTING: PROCESS
    variable V1,V2,V3 : Integer ;
    variable A : Integer := 10 ;
    variable B : Integer := 5  ;
  BEGIN
    V1 := (-A)/B ;
    V2 := A/(-B) ;
    assert NOT(V1 = V2)
      report "***PASSED TEST: c07s02b06x00p11n01i02264"
      severity NOTE;
    assert (V1 = V2)
      report "***FAILED TEST: c07s02b06x00p11n01i02264 - Integer division satisfies the following identity: (-A)/B = -(A/B) = A/(-B)." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b06x00p11n01i02264arch;
