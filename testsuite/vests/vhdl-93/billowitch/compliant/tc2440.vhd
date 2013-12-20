
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
-- $Id: tc2440.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x02p01n01i02440ent IS
END c07s03b02x02p01n01i02440ent;

ARCHITECTURE c07s03b02x02p01n01i02440arch OF c07s03b02x02p01n01i02440ent IS

BEGIN
  TESTING: PROCESS
    -- Range types are all predefined enumerated types.
    type    CHAR_ARR    is ARRAY( CHARACTER range <> ) of BIT;
    subtype    CHAR_PART    is CHAR_ARR( 'a' to 'z' );
    subtype    CHAR_PART_DESC    is CHAR_ARR( 'z' downto 'a' );

    type    BIT_ARR    is ARRAY( BIT range <> ) of BIT;
    subtype    BIT_PART    is BIT_ARR( bit'('0') to bit'('0') );
    subtype    BIT_PART_DESC    is BIT_ARR( bit'('1') downto bit'('0') );

    type    BOOL_ARR    is ARRAY( BOOLEAN range <> ) of BIT;
    subtype    BOOL_PART    is BOOL_ARR( TRUE to TRUE );
    subtype    BOOL_PART_DESC    is BOOL_ARR( TRUE downto FALSE );

    type    SEV_ARR    is ARRAY( SEVERITY_LEVEL range <> ) of BIT;
    subtype    SEV_PART    is SEV_ARR( WARNING to FAILURE );
    subtype    SEV_PART_DESC    is SEV_ARR( FAILURE downto WARNING );

    -- Declare variables of these types.
    variable CHARV : CHAR_PART;
    variable BITV  : BIT_PART;
    variable BOOLV : BOOL_PART;
    variable SEVV  : SEV_PART;
    variable OKtest: integer := 0;
  BEGIN
    -- Assign each of these arrays using aggregates.
    -- 1. Individual aggregates.
    CHARV := CHAR_PART'( 'a' => '1', 'b' => '0', 'c' to 'z' => '1' );                                    for C in 'a' to 'z' loop
                                                                                                           if  (C = 'a') then
                                                                                                             assert( CHARV( C ) = '1' );
                                                                                                             if NOT( CHARV( C ) = '1' ) then
                                                                                                               OKtest := 1;
                                                                                                             end if;
                                                                                                           elsif (C = 'b') then
                                                                                                             assert( CHARV( C ) = '0' );
                                                                                                             if NOT( CHARV( C ) = '0' ) then
                                                                                                               OKtest := 1;
                                                                                                             end if;
                                                                                                           else
                                                                                                             assert( CHARV( C ) = '1' );
                                                                                                             if NOT( CHARV( C ) = '1' ) then
                                                                                                               OKtest := 1;
                                                                                                             end if;
                                                                                                           end if;
  end loop;
  BITV  := BIT_PART'( '0' => '0' );
  assert( BITV( '0' ) = '0' );
  if NOT( BITV( '0' ) = '0' ) then
    OKtest := 1;
  end if;
  BOOLV  := BOOL_PART'( TRUE => '1' );
  assert( BOOLV( TRUE ) = '1' );
  if NOT( BOOLV( TRUE ) = '1' ) then
    OKtest := 1;
  end if;
  SEVV  := SEV_PART'( WARNING => '1', ERROR => '0',
                      FAILURE => '1' );
  assert( SEVV( WARNING ) = '1' );
  assert( SEVV( ERROR ) = '0' );
  assert( SEVV( FAILURE ) = '1' );
  if NOT((SEVV(WARNING)='1')and(SEVV(ERROR)='0')and(SEVV(FAILURE)='1'))then
    OKtest := 1;
  end if;
  
  -- 2. Groups of aggregates.
  CHARV := CHAR_PART'( 'a' | 'b' => '1', 'c' to 'z' => '0' );
  for C in 'a' to 'z' loop
    if  (C = 'a') then
      assert( CHARV( C ) = '1' );
      if NOT( CHARV( C ) = '1' ) then
        OKtest := 1;
      end if;
    elsif (C = 'b') then
      assert( CHARV( C ) = '1' );
      if NOT( CHARV( C ) = '1' ) then
        OKtest := 1;
      end if;
    else
      assert( CHARV( C ) = '0' );
      if NOT( CHARV( C ) = '0' ) then
        OKtest := 1;
      end if;
    end if;
  end loop;
  BITV  := BIT_PART'( '0' to '0' => '0' );
  assert( BITV( '0' ) = '0' );
  if NOT( BITV( '0' ) = '0' ) then
    OKtest := 1;
  end if;
  BOOLV  := BOOL_PART'( TRUE to TRUE => '1' );
  assert( BOOLV( TRUE ) = '1' );
  if NOT( BOOLV( TRUE ) = '1' ) then
    OKtest := 1;
  end if;
  SEVV  := SEV_PART'( ERROR => '0', WARNING | FAILURE => '1' );
  assert( SEVV( WARNING ) = '1' );
  assert( SEVV( ERROR ) = '0' );
  assert( SEVV( FAILURE ) = '1' );
  if NOT((SEVV(WARNING)='1')and(SEVV(ERROR)='0')and(SEVV(FAILURE)='1'))then
    OKtest := 1;
  end if;

  -- 3. Use of 'others' in these aggregates.
  CHARV := CHAR_PART'( 'a' | 'b' => '0', others => '1' );
  for C in 'a' to 'z' loop
    if  (C = 'a') then
      assert( CHARV( C ) = '0' );
      if NOT( CHARV( C ) = '0' ) then
        OKtest := 1;
      end if;
    elsif (C = 'b') then
      assert( CHARV( C ) = '0' );
      if NOT( CHARV( C ) = '0' ) then
        OKtest := 1;
      end if;
    else
      assert( CHARV( C ) = '1' );
      if NOT( CHARV( C ) = '1' ) then
        OKtest := 1;
      end if;
    end if;
  end loop;
  BITV  := BIT_PART'( others => '1' );
  assert( BITV( '0' ) = '1' );
  if NOT( BITV( '0' ) = '1' ) then
    OKtest := 1;
  end if;
  BOOLV  := BOOL_PART'( others => '0' );
  assert( BOOLV( TRUE ) = '0' );
  if NOT( BOOLV( TRUE ) = '0' ) then
    OKtest := 1;
  end if;
  SEVV  := SEV_PART'( ERROR => '0', others => '1' );
  assert( SEVV( WARNING ) = '1' );
  assert( SEVV( ERROR ) = '0' );
  assert( SEVV( FAILURE ) = '1' );
  if NOT((SEVV(WARNING)='1')and(SEVV(ERROR)='0')and(SEVV(FAILURE)='1'))then
    OKtest := 1;
  end if;
  wait for 5 ns;
  assert NOT(OKtest = 0) 
    report "***PASSED TEST: c07s03b02x02p01n01i02440" 
    severity NOTE;
  assert (OKtest = 0) 
    report "***FAILED TEST: c07s03b02x02p01n01i02440 - Aggregates with different range types test failed."
    severity ERROR;
  wait;
END PROCESS TESTING;

END c07s03b02x02p01n01i02440arch;
