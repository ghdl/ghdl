
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
-- $Id: tc1205.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c08s01b00x00p08n03i01205pkg is
  
  -- Type declarations.
  type    SWITCH_LEVEL   is ( '0', '1', 'X' );
  type    S_logic_vector is array(positive range <>) of SWITCH_LEVEL;

  -- Define the bus resolution function.
  function switchf( s : S_logic_vector ) return SWITCH_LEVEL;
  
  -- Further type declarations.
  subtype SWITCH_T       is switchF SWITCH_LEVEL;
  type    WORD           is array(0 to 31) of SWITCH_T;

end c08s01b00x00p08n03i01205pkg;

package body c08s01b00x00p08n03i01205pkg is

  function switchf( s : S_logic_vector ) return SWITCH_LEVEL is
    
  begin
    return( S(1) );
  end switchf;
  
end c08s01b00x00p08n03i01205pkg;



use work.c08s01b00x00p08n03i01205pkg.all;
entity c08s01b00x00p08n03i01205ent_a is
  
  generic ( GenOne : in INTEGER ; GenTwo :INTEGER);
  
end c08s01b00x00p08n03i01205ent_a;

-------------------------------------------------------------------------

architecture c08s01b00x00p08n03i01205arch_a of c08s01b00x00p08n03i01205ent_a is
  -- Type definitions.
  type WORD2 is array( 0 to 31 ) of SWITCH_LEVEL;
  
  -- Local signals.
  signal A, B    : WORD;
  signal UnResolved    : WORD2;
  
  
begin
  
  TEST_PROCESS:  process
    -- Constant declarations.
    constant One : INTEGER := 1;
    constant Two : INTEGER := 2;
    
    -- Local variables.
    variable ShouldBeTime : TIME;
    variable I            : INTEGER;

    variable k    : integer := 0;
    
  begin
    -- Test locally static signals.
    A( 1 ) <= 'X' after 10 ns;
    A( 2 ) <= 'X' after 5 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on A(1);
    
    -- Should wake up when the A(1) assignment takes place.
    assert (A(1) = 'X');
    assert (ShouldBeTime = NOW);
    
    if (A(1) /= 'X' and ShouldBeTime /= Now) then
      k := 1;
    end if;
    
    -- Perform same test, but with a constant.
    A( One ) <= '1' after 10 ns;
    A( Two ) <= '1' after 5 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on A(One);
    
    -- Should wake up when the A(1) assignment takes place.
    assert (A(One) = '1');
    assert (ShouldBeTime = NOW);

    if (A(One) /= '1' and ShouldBeTime /= Now) then
      k := 1;
    end if;
    
    -- Perform same test, but with a generic.   (globally static)
    A( GenOne ) <= 'X' after 10 ns;
    A( GenTwo ) <= 'X' after 5 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on A(GenOne);
    
    -- Should wake up when the A(1) assignment takes place.
    assert (A(GenOne) = 'X');
    assert (ShouldBeTime = NOW);
    
    if (A(GenOne) /= 'X' and ShouldBeTime /= Now) then
      k := 1;
    end if;
    
    -- Perform same test, but assigning to the whole thing.
    A <= ('1','1','1','1','1','1','1','1','1','1',
          '1','1','1','1','1','1','1','1','1','1',
          '1','1','1','1','1','1','1','1','1','1',
          '1','1') after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on A(GenOne);
    
    -- Should wake up when the all assignments take place.
    assert (A(GenOne) = '1');
    assert (ShouldBeTime = NOW);

    if (A(GenOne) /= '1' and ShouldBeTime /= Now) then
      k := 1;
    end if;
    
    -- Now, perform same test but assigning to a composite
    -- signal which is NOT resolved at the scalar subelement
    -- level.
    UnResolved <= ('1','1','1','1','1','1','1','1','1','1',
                   '1','1','1','1','1','1','1','1','1','1',
                   '1','1','1','1','1','1','1','1','1','1',
                   '1','1') after 10 ns;
    ShouldBeTime := NOW + 10 ns;
    wait on UnResolved(GenOne);
    
    -- Should wake up when the all assignments take place.
    assert (UnResolved(GenOne) = '1');
    assert (ShouldBeTime = NOW);
    
    if (UnResolved(GenOne) /= '1' and ShouldBeTime /= Now) then
      k := 1;   
    end if;
    
    assert NOT(k = 0) 
      report "***PASSED TEST: c08s01b00x00p08n03i01205" 
      severity NOTE;
    assert (k = 0) 
      report "***FAILED TEST: c08s01b00x00p08n03i01205 - All statically indexed signal names (both locally and globally static) may be used in the sensitivity clause of a wait statement." 
      severity ERROR;
    wait;
  end process TEST_PROCESS;
  
end c08s01b00x00p08n03i01205arch_a;

use work.c08s01b00x00p08n03i01205pkg.all;
ENTITY c08s01b00x00p08n03i01205ent IS
END c08s01b00x00p08n03i01205ent;

ARCHITECTURE c08s01b00x00p08n03i01205arch OF c08s01b00x00p08n03i01205ent IS
  
  component c08s01b00x00p08n03i01205ent_a
    generic( GenOne : in INTEGER;  GenTwo : INTEGER );
  end component;
  for T1 : c08s01b00x00p08n03i01205ent_a use entity work.c08s01b00x00p08n03i01205ent_a(c08s01b00x00p08n03i01205arch_a); 

BEGIN

  T1 : c08s01b00x00p08n03i01205ent_a generic map ( 1, 2 );

END c08s01b00x00p08n03i01205arch;
