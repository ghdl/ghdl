
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
-- $Id: tc2396.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x00p07n01i02396ent IS
END c07s03b02x00p07n01i02396ent;

ARCHITECTURE c07s03b02x00p07n01i02396arch OF c07s03b02x00p07n01i02396ent IS

BEGIN
  TESTING: PROCESS
    -- Declare ascending and descending ranges.
    subtype BYTE is BIT_VECTOR( 0 to 7 );

    -- Declare array variables of these types.
    variable BYTEV : BYTE;
  BEGIN
    BYTEV := BYTE'( 7 => '0', 6 => '1', 4 => '1',
                    2 => '1', 0 => '1', 5 => '0',
                    3 => '0', 1 => '0' );
    assert( BYTEV( 1 ) = '0' );
    assert( BYTEV( 3 ) = '0' );
    assert( BYTEV( 5 ) = '0' );
    assert( BYTEV( 7 ) = '0' );
    assert( BYTEV( 0 ) = '1' );
    assert( BYTEV( 2 ) = '1' );
    assert( BYTEV( 4 ) = '1' );
    assert( BYTEV( 6 ) = '1' );
    wait for 1 ns;
    assert NOT(   ( BYTEV( 1 ) = '0' )   and 
                  ( BYTEV( 3 ) = '0' )   and 
                  ( BYTEV( 5 ) = '0' )   and 
                  ( BYTEV( 7 ) = '0' )   and 
                  ( BYTEV( 0 ) = '1' )   and 
                  ( BYTEV( 2 ) = '1' )   and 
                  ( BYTEV( 4 ) = '1' )   and 
                  ( BYTEV( 6 ) = '1' ))
      report "***PASSED TEST: c07s03b02x00p07n01i02396" 
      severity NOTE;
    assert (   ( BYTEV( 1 ) = '0' )   and 
               ( BYTEV( 3 ) = '0' )   and 
               ( BYTEV( 5 ) = '0' )   and 
               ( BYTEV( 7 ) = '0' )   and 
               ( BYTEV( 0 ) = '1' )   and 
               ( BYTEV( 2 ) = '1' )   and 
               ( BYTEV( 4 ) = '1' )   and 
               ( BYTEV( 6 ) = '1' ))
      report "***FAILED TEST: c07s03b02x00p07n01i02396 - Named association should be able to appear in any order."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x00p07n01i02396arch;
