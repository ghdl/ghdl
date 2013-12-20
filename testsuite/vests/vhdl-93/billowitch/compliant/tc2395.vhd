
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
-- $Id: tc2395.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x00p07n02i02395ent IS
END c07s03b02x00p07n02i02395ent;

ARCHITECTURE c07s03b02x00p07n02i02395arch OF c07s03b02x00p07n02i02395ent IS

BEGIN
  TESTING: PROCESS
    -- Declare ascending and descending ranges.
    subtype ONE is BIT_VECTOR( 0 to 0);

    -- Declare array variables of these types.
    variable ONEV1 : ONE;
    variable ONEV2 : ONE;
  BEGIN
    ONEV1 := ONE'( 0 => '0' );
    assert( ONEV1( 0 ) = '0' );
    ONEV2 := ONE'( 0 => '1' );
    assert( ONEV2( 0 ) = '1' );
    assert NOT(( ONEV1( 0 ) = '0' )   and ( ONEV2( 0 ) = '1' ))
      report "***PASSED TEST: c07s03b02x00p07n02i02395" 
      severity NOTE;
    assert (( ONEV1( 0 ) = '0' )   and ( ONEV2( 0 ) = '1' ))
      report "***FAILED TEST: c07s03b02x00p07n02i02395 - Aggregate specification should be using named association."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x00p07n02i02395arch;
