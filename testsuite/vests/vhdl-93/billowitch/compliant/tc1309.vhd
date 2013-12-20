
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
-- $Id: tc1309.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c08s04b00x00p07n01i01309pkg is

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

end c08s04b00x00p07n01i01309pkg;

package body c08s04b00x00p07n01i01309pkg is
  
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
  
end c08s04b00x00p07n01i01309pkg;

use work.c08s04b00x00p07n01i01309pkg.all;
ENTITY c08s04b00x00p07n01i01309ent IS
END c08s04b00x00p07n01i01309ent;

ARCHITECTURE c08s04b00x00p07n01i01309arch OF c08s04b00x00p07n01i01309ent IS
  -- Local signals.
  signal S       : BIT;
  signal T       : BIT;
  
  signal C1, C2  : CHARACTER;
BEGIN
  TESTING: PROCESS
    
    -- local variables
    variable BITV     : BV2 := B"11";
    variable STRV     : CH2 := "ab";
    variable ShouldBeTime : TIME;
    
    variable k : integer := 0;
    
  BEGIN
    -- Assign with a variable as the expression.
    ( S, T )  <= BITV after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on S,T;
    if (ShouldBeTime /= Now or S /= BITV(0) or T /= BITV(1)) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert ((S = BITV( 0 )) and (T = BITV( 1 )));
    
    ( C1,C2 ) <= STRV after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on C1,C2;
    if (ShouldBeTime /= Now or C1 /= STRV(1) or C2 /= STRV(2)) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert ((C1 = STRV( 1 )) and (C2 = STRV( 2 )));
    
    -- Assign with a function return value.
    ( S, T )  <= returnBV2 after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on S,T;
    if (ShouldBeTime /= Now or S /= BVC(0) or T /= BVC(1)) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert ((S = BVC( 0 )) and (T = BVC( 1 )));
    
    ( C1,C2 ) <= returnCH2 after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on C1,C2;
    if (ShouldBeTime /= Now or C1 /= CHC(1) or C2 /= CHC(2)) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert ((C1 = CHC( 1 )) and (C2 = CHC( 2 )));
    
    -- Assign with a qualified expression.
    ( S, T )  <= BV2'( '0', '1' ) after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on S,T;
    if (ShouldBeTime /= Now or S /= '0' or T /= '1') then
      k := 1;   
    end if;
    assert (ShouldBeTime = NOW);
    assert ((S = '0') and (T = '1'));
    
    ( C1,C2 ) <= CH2'( 'c', 'c' ) after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on C1,C2;
    if (ShouldBeTime /= Now or C1 /= 'c' or C2 /= 'c') then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert ((C1 = 'c') and (C2 = 'c'));
    
    assert NOT( k = 0 ) 
      report "***PASSED TEST: c08s04b00x00p07n01i01309"
      severity NOTE;
    assert ( k = 0 ) 
      report "***FAILED TEST: c08s04b00x00p07n01i01309 - If the target of the signal assignment statement is in the form of an aggregate, then the type of the aggregate must be determinable from the context."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s04b00x00p07n01i01309arch;
