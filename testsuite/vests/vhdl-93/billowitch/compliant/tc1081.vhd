
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
-- $Id: tc1081.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s05b00x00p01n02i01081ent IS
END c06s05b00x00p01n02i01081ent;

ARCHITECTURE c06s05b00x00p01n02i01081arch OF c06s05b00x00p01n02i01081ent IS
  SUBTYPE    bit_vector_4 is bit_vector ( 0 to 3 );
  SUBTYPE    bit_vector_8 is bit_vector ( 0 to 7 );

BEGIN
  TESTING: PROCESS
    VARIABLE var : bit_vector_8 := B"1110_0010";
    VARIABLE v1  : bit_vector_4 := B"0011";
    VARIABLE v2  : bit_vector_4 := B"1111";
  BEGIN
    var (0 to 3) := v1;
    var (4 to 7) := v2;
    assert NOT( var = B"0011_1111" )
      report "***PASSED TEST: c06s05b00x00p01n02i01081"
      severity NOTE;
    assert ( var = B"0011_1111" )
      report "***FAILED TEST: c06s05b00x00p01n02i01081 - Slices of a variable may be the target of a variable assignment." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s05b00x00p01n02i01081arch;
