
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
-- $Id: tc1375.vhd,v 1.2 2001-10-26 16:29:40 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c08s05b00x00p03n01i01375pkg is

  -- Type declarations.
  subtype     BV2  is BIT_VECTOR( 0 to 1 );
  subtype     CH2  is STRING( 1 to 2 );

  -- Constant declarations.
  constant BVC  : BV2 := B"00";
  constant CHC  : CH2 := "bb";

  -- Function returns BV2.
  function  returnBV2 return BV2;

  -- Function returns CH2.
  function  returnCH2 return CH2;
  
end c08s05b00x00p03n01i01375pkg;

package body c08s05b00x00p03n01i01375pkg is
  
  -- Function returns BV2.
  function  returnBV2 return BV2 is
  begin
    return ( BVC );
  end returnBV2;
  
  -- Function returns CH2.
  function  returnCH2 return CH2 is
  begin
    return( CHC );
  end returnCH2;
  
end c08s05b00x00p03n01i01375pkg;

use work.c08s05b00x00p03n01i01375pkg.all;
ENTITY c08s05b00x00p03n01i01375ent IS
END c08s05b00x00p03n01i01375ent;

ARCHITECTURE c08s05b00x00p03n01i01375arch OF c08s05b00x00p03n01i01375ent IS

BEGIN
  TESTING: PROCESS
    
    -- local variables
    variable BITV     : BV2 := B"11";
    variable STRV     : CH2 := "ab";
    
    variable S, T        : BIT;
    variable S1, T1        : BIT;
    variable S11, T11        : BIT;
    variable C1, C2      : CHARACTER;
    variable C11, C22      : CHARACTER;
    variable C111, C222      : CHARACTER;
    
  BEGIN
    -- Assign with a variable as the expression.
    ( S, T )  := BITV;
    
    ( C1,C2 ) := STRV;
    
    -- Assign with a function return value.
    ( S1, T1 )  := returnBV2;
    
    ( C11,C22 ) := returnCH2;
    
    -- Assign with a qualified expression.
    ( S11, T11 )  := BV2'( '0', '1' );
    
    ( C111,C222 ) := CH2'( 'c', 'c' );

    assert NOT(((S = BITV( 0 )) and (T = BITV( 1 )))
               and ((C1 = STRV( 1 )) and (C2 = STRV( 2 )))
               and ((S1 = BVC( 0 )) and (T1 = BVC( 1 )))
               and ((C11 = CHC( 1 )) and (C22 = CHC( 2 )))
               and ((S11 = '0') and (T11 = '1'))
               and ((C111 = 'c') and (C222 = 'c')))
      report "***PASSED TEST: c08s05b00x00p03n01i01375" 
      severity NOTE;
    assert (((S = BITV( 0 )) and (T = BITV( 1 )))
            and ((C1 = STRV( 1 )) and (C2 = STRV( 2 )))
            and ((S1 = BVC( 0 )) and (T1 = BVC( 1 )))
            and ((C11 = CHC( 1 )) and (C22 = CHC( 2 )))
            and ((S11 = '0') and (T11 = '1'))
            and ((C111 = 'c') and (C222 = 'c')))
      report "***FAILED TEST: c08s05b00x00p03n01i01375 - Legal aggregate variable assignment fail." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s05b00x00p03n01i01375arch;
