
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
-- $Id: tc2439.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x02p01n01i02439ent IS
END c07s03b02x02p01n01i02439ent;

ARCHITECTURE c07s03b02x02p01n01i02439arch OF c07s03b02x02p01n01i02439ent IS

BEGIN
  TESTING: PROCESS
    -- Range types are all predefined enumerated types.
    type CHAR_ARR    is ARRAY( CHARACTER )       of BIT;
    type BIT_ARR    is ARRAY( BIT )       of BIT;
    type BOOL_ARR    is ARRAY( BOOLEAN )       of BIT;
    type SEV_ARR    is ARRAY( SEVERITY_LEVEL )    of BIT;

    -- Declare variables of these types.
    variable CHARV : CHAR_ARR;
    variable BITV  : BIT_ARR;
    variable BOOLV : BOOL_ARR;
    variable SEVV  : SEV_ARR;
    variable OKtest: integer := 0;
  BEGIN
    -- Assign each of these arrays using aggregates.
    -- 1. Individual aggregates.
    CHARV := CHAR_ARR'( 'a' => '1', 'b' => '0', NUL to '`' => '1',
                        'c' to character'high => '1' );
    for C in CHARACTER loop
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
    BITV  := BIT_ARR'( '0' => '0', '1' => '1' );
    assert( BITV( '0' ) = '0' );
    if NOT( BITV( '0' ) = '0' ) then
      OKtest := 1;
    end if;
    assert( BITV( '1' ) = '1' );
    if NOT( BITV( '1' ) = '1' ) then
      OKtest := 1;
    end if;
    BOOLV  := BOOL_ARR'( FALSE => '0', TRUE => '1' );
    assert( BOOLV( FALSE ) = '0' );
    if NOT( BOOLV( FALSE ) = '0' ) then
      OKtest := 1;
    end if;
    assert( BOOLV( TRUE ) = '1' );
    if NOT( BOOLV( TRUE ) = '1' ) then
      OKtest := 1;
    end if;
    SEVV  := SEV_ARR'( NOTE => '0', WARNING => '1', ERROR => '0',
                       FAILURE => '1' );
    assert( SEVV( NOTE ) = '0' );
    assert( SEVV( WARNING ) = '1' );
    assert( SEVV( ERROR ) = '0' );
    assert( SEVV( FAILURE ) = '1' );
    if NOT((SEVV(NOTE)='0')and(SEVV(WARNING) ='1')and(SEVV(ERROR)='0')and(SEVV(FAILURE)='1')) then
      OKtest := 1;
    end if;
    
    -- 2. Groups of aggregates.
    CHARV := CHAR_ARR'( 'a' | 'b' => '1', NUL to '`' => '0',
                        'c' to character'high => '0' );
    for C in CHARACTER loop
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
    BITV  := BIT_ARR'( '0' | '1' => '0' );
    assert( BITV( '0' ) = '0' );
    assert( BITV( '1' ) = '0' );
    if NOT((BITV('0')='0') and (BITV('1')='0')) then
      OKtest := 1;
    end if;
    BOOLV  := BOOL_ARR'( FALSE | TRUE => '1' );
    assert( BOOLV( FALSE ) = '1' );
    assert( BOOLV( TRUE ) = '1' );
    if NOT((BOOLV(FALSE)='1') and (BOOLV(TRUE)='1')) then
      OKtest := 1;
    end if;
    SEVV  := SEV_ARR'( NOTE | ERROR => '0', WARNING | FAILURE => '1' );
    assert( SEVV( NOTE ) = '0' );
    assert( SEVV( WARNING ) = '1' );
    assert( SEVV( ERROR ) = '0' );
    assert( SEVV( FAILURE ) = '1' );
    if NOT((SEVV(NOTE)='0')and(SEVV(WARNING) ='1')and(SEVV(ERROR)='0')and(SEVV(FAILURE)='1')) then
      OKtest := 1;
    end if;

    -- 3. Use of 'others' in these aggregates.
    CHARV := CHAR_ARR'( 'a' | 'b' => '0', others => '1' );
    for C in CHARACTER loop
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
    BITV  := BIT_ARR'( others => '1' );
    assert( BITV( '0' ) = '1' );
    assert( BITV( '1' ) = '1' );
    if NOT(( BITV( '0' ) = '1' )and( BITV( '1' ) = '1' ))then
      OKtest := 1;
    end if;
    BOOLV  := BOOL_ARR'( FALSE => '1', others => '0' );
    assert( BOOLV( FALSE ) = '1' );
    assert( BOOLV( TRUE ) = '0' );
    if NOT(( BOOLV( FALSE ) = '1' )and( BOOLV( TRUE ) = '0' ))then
      OKtest := 1;
    end if;
    SEVV  := SEV_ARR'( NOTE | ERROR => '0', others => '1' );
    assert( SEVV( NOTE ) = '0' );
    assert( SEVV( WARNING ) = '1' );
    assert( SEVV( ERROR ) = '0' );
    assert( SEVV( FAILURE ) = '1' );
    if NOT((SEVV(NOTE)='0')and(SEVV(WARNING) ='1')and(SEVV(ERROR)='0')and(SEVV(FAILURE)='1')) then
      OKtest := 1;
    end if;
    wait for 5 ns;
    assert NOT(OKtest = 0) 
      report "***PASSED TEST: c07s03b02x02p01n01i02439" 
      severity NOTE;
    assert (OKtest = 0) 
      report "***FAILED TEST: c07s03b02x02p01n01i02439 - Aggregates with different range types test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x02p01n01i02439arch;
