
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
-- $Id: tc1207.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c08s01b00x00p08n04i01207pkg is

  -- Type declarations.
  type    SWITCH_LEVEL   is ( '0', '1', 'X' );
  type    S_logic_vector is array(positive range <>) of SWITCH_LEVEL;

  -- Define the bus resolution function.
  function switchf( s : S_logic_vector ) return SWITCH_LEVEL;

  -- Further type declarations.
  subtype SWITCH_T       is switchF SWITCH_LEVEL;
  type    WORD           is array(0 to 31) of SWITCH_T;

end c08s01b00x00p08n04i01207pkg;

package body c08s01b00x00p08n04i01207pkg is

  function switchf( s : S_logic_vector ) return SWITCH_LEVEL is
  begin
    return( S(1) );
  end switchf;

end c08s01b00x00p08n04i01207pkg;

use work.c08s01b00x00p08n04i01207pkg.all;
ENTITY c08s01b00x00p08n04i01207ent IS
END c08s01b00x00p08n04i01207ent;

ARCHITECTURE c08s01b00x00p08n04i01207arch OF c08s01b00x00p08n04i01207ent IS

  -- Local types.
  type WORD2 is array(0 to 31) of SWITCH_LEVEL;
  
  -- Local signals.
  signal A, B    : WORD;
  signal UnResolved    : WORD2;

BEGIN
  TESTING: PROCESS
    -- Constant declarations.
    constant One : INTEGER := 1;
    constant Two : INTEGER := 2;
    
    -- Local variables.
    variable ShouldBeTime : TIME;
    variable I            : INTEGER;
    variable k        : integer := 0;
  BEGIN
    -- 1. Test waiting on static signals.
    for I in 0 to 31 loop
      A( I ) <= 'X' after (I * 1 ns);
    end loop;
    ShouldBeTime := NOW + 31 ns;
    wait until (A(31) = 'X');
    
    -- Should wake up when the A(31) assignment takes place.
    if (A(31) /= 'X' and ShouldBeTime /= NOW) then
      k := 1;
    end if;
    assert (A(31) = 'X');
    assert (ShouldBeTime = NOW);
    
    -- 2. Test waiting on non-static signals.  (should still have same behavior, but just be slower)
    for I in 0 to 31 loop
      A( I ) <= '1' after (I * 1 ns);
    end loop;
    ShouldBeTime := NOW + 31 ns;
    I := 31;
    wait until (A(I) = '1');
    
    -- Should wake up when the A(31) assignment takes place.
    if (A(I) /= '1' and ShouldBeTime /= NOW) then
      k := 1;
    end if;
    assert (A(I) = '1');
    assert (ShouldBeTime = NOW);
    
    -- 3. Test that waiting on a variable expression merely times-out. 
    ShouldBeTime := NOW + 35 ns;
    wait until (I = 47) for 35 ns;
    if (ShouldBeTime /= NOW) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    
    -- 4. Perform same test as '1' on a signal not resolved at the scalar subelement level.
    UnResolved <= ( 'X','X','X','X','X','X','X','X','X','X',
                    'X','X','X','X','X','X','X','X','X','X',
                    'X','X','X','X','X','X','X','X','X','X',
                    'X','X' ) after 31 ns;
    ShouldBeTime := NOW + 31 ns;
    wait until (UnResolved(31) = 'X');
    
    -- Should wake up when the UnResolved(31) assignment takes place.
    if (UnResolved(31) /= 'X' and ShouldBeTime /= NOW) then
      k := 1;
    end if;
    assert (UnResolved(31) = 'X');
    assert (ShouldBeTime = NOW);

    assert NOT(k=0) 
      report "***PASSED TEST: c08s01b00x00p08n04i01207"
      severity NOTE;
    assert ( k=0 )
      report "***FAILED TEST: c08s01b00x00p08n04i01207 - The sensitivity set of a wait statement will contain the signal denoted by the longest static prefix of each signal name if no sensitivity clause." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s01b00x00p08n04i01207arch;
