
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
-- $Id: tc2408.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x00p08n05i02408ent IS
END c07s03b02x00p08n05i02408ent;

ARCHITECTURE c07s03b02x00p08n05i02408arch OF c07s03b02x00p08n05i02408ent IS

BEGIN
  TESTING: PROCESS
    -- Declare ascending and descending ranges.
    subtype BYTE is BIT_VECTOR( 0 to 7 );

    -- Declare array variables of these types.
    variable BYTEV1 : BYTE;
    variable BYTEV2 : BYTE;
  BEGIN
    BYTEV1 := BYTE'( others => '1' );
    assert( BYTEV1( 0 ) = '1' );
    assert( BYTEV1( 1 ) = '1' );
    assert( BYTEV1( 2 ) = '1' );
    assert( BYTEV1( 3 ) = '1' );
    assert( BYTEV1( 4 ) = '1' );
    assert( BYTEV1( 5 ) = '1' );
    assert( BYTEV1( 6 ) = '1' );
    assert( BYTEV1( 7 ) = '1' );
    BYTEV2 := BYTE'( others => '0' );
    assert( BYTEV2( 0 ) = '0' );
    assert( BYTEV2( 1 ) = '0' );
    assert( BYTEV2( 2 ) = '0' );
    assert( BYTEV2( 3 ) = '0' );
    assert( BYTEV2( 4 ) = '0' );
    assert( BYTEV2( 5 ) = '0' );
    assert( BYTEV2( 6 ) = '0' );
    assert( BYTEV2( 7 ) = '0' );
    wait for 5 ns;
    assert NOT(   ( BYTEV1( 0 ) = '1' )   and
                  ( BYTEV1( 1 ) = '1' )   and
                  ( BYTEV1( 2 ) = '1' )   and
                  ( BYTEV1( 3 ) = '1' )   and
                  ( BYTEV1( 4 ) = '1' )   and
                  ( BYTEV1( 5 ) = '1' )   and
                  ( BYTEV1( 6 ) = '1' )   and
                  ( BYTEV1( 7 ) = '1' )   and
                  ( BYTEV2( 0 ) = '0' )   and
                  ( BYTEV2( 1 ) = '0' )   and
                  ( BYTEV2( 2 ) = '0' )   and
                  ( BYTEV2( 3 ) = '0' )   and
                  ( BYTEV2( 4 ) = '0' )   and
                  ( BYTEV2( 5 ) = '0' )   and
                  ( BYTEV2( 6 ) = '0' )   and
                  ( BYTEV2( 7 ) = '0' )   ) 
      report "***PASSED TEST: c07s03b02x00p08n05i02408" 
      severity NOTE;
    assert (   ( BYTEV1( 0 ) = '1' )   and
               ( BYTEV1( 1 ) = '1' )   and
               ( BYTEV1( 2 ) = '1' )   and
               ( BYTEV1( 3 ) = '1' )   and
               ( BYTEV1( 4 ) = '1' )   and
               ( BYTEV1( 5 ) = '1' )   and
               ( BYTEV1( 6 ) = '1' )   and
               ( BYTEV1( 7 ) = '1' )   and
               ( BYTEV2( 0 ) = '0' )   and
               ( BYTEV2( 1 ) = '0' )   and
               ( BYTEV2( 2 ) = '0' )   and
               ( BYTEV2( 3 ) = '0' )   and
               ( BYTEV2( 4 ) = '0' )   and
               ( BYTEV2( 5 ) = '0' )   and
               ( BYTEV2( 6 ) = '0' )   and
               ( BYTEV2( 7 ) = '0' )   ) 
      report "***FAILED TEST: c07s03b02x00p08n05i02408 - Others should work well by itself."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x00p08n05i02408arch;
