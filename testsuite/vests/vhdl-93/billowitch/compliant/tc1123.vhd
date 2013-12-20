
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
-- $Id: tc1123.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s05b00x00p03n02i01123ent IS
END c06s05b00x00p03n02i01123ent;

ARCHITECTURE c06s05b00x00p03n02i01123arch OF c06s05b00x00p03n02i01123ent IS
  TYPE       colors       is ( red, green, blue, yellow, orange, black );
  TYPE       ncolor_array    is array ( NATURAL range <> ) of colors;
  SUBTYPE       ncolor_4    is ncolor_array ( 0 to 3 );
  TYPE       pcolor_array    is array ( POSITIVE range <> ) of colors;
  SUBTYPE       pcolor_4    is ncolor_array ( 1 to 4 );
BEGIN
  TESTING: PROCESS
    variable vn : ncolor_4 := ( red, red, green, black );
    variable vp : pcolor_4 := ( blue, yellow, yellow, orange );
  BEGIN
    vn(1 to 3) := vp(2 to 4);
    assert NOT( vn = ( red, yellow, yellow, orange ) ) 
      report "***PASSED TEST: c06s05b00x00p03n02i01123"
      severity NOTE;
    assert ( vn = ( red, yellow, yellow, orange ) ) 
      report "***FAILED TEST: c06s05b00x00p03n02i01123 - The base type of the array type is the type of the slice." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s05b00x00p03n02i01123arch;
