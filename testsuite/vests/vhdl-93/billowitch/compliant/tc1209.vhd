
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
-- $Id: tc1209.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c08s01b00x00p24n01i01209pkg is

  -- Type declarations.
  type    SWITCH_LEVEL   is ( '0', '1', 'X' );
  type    S_logic_vector is array(positive range <>) of SWITCH_LEVEL;

  -- Define the bus resolution function.
  function switchf( s : S_logic_vector ) return SWITCH_LEVEL;

  -- Further type declarations.
  subtype SWITCH_T       is switchF SWITCH_LEVEL;
  type    WORD           is array(0 to 31) of SWITCH_T;

end c08s01b00x00p24n01i01209pkg;

package body c08s01b00x00p24n01i01209pkg is

-- A dumb resolution function.
  function switchf( s : S_logic_vector ) return SWITCH_LEVEL is
  begin
    return( S(1) );
  end switchf;

end c08s01b00x00p24n01i01209pkg;


use work.c08s01b00x00p24n01i01209pkg.all;
ENTITY c08s01b00x00p24n01i01209ent IS
END c08s01b00x00p24n01i01209ent;

ARCHITECTURE c08s01b00x00p24n01i01209arch OF c08s01b00x00p24n01i01209ent IS

  -- Local types
  type WORD2 is array(0 to 31) of SWITCH_LEVEL;
  type REC   is RECORD
                  R1    : SWITCH_T;
                  R2    : SWITCH_T;
                end RECORD;
  
  -- Local signals.
  signal A : WORD;
  signal UnResolved : WORD2;
  signal RecSig     : REC;
  
BEGIN
  TESTING: PROCESS
    -- Constant declarations.
    constant One : INTEGER := 1;
    constant Two : INTEGER := 2;
    
    -- Local variables.
    variable ShouldBeTime : TIME;
    variable I            : INTEGER;
    variable   k    : integer := 0;


  BEGIN
    --1. Test waiting on an array of scalar resolved elements.
    for I in 0 to 31 loop
      ShouldBeTime := NOW + 1 ns;
      A( I ) <= 'X' after 1 ns;
      wait on A;
      if (A(I) /= 'X' and ShouldBeTime /= Now) then
        k := 1;
      end if; 
      -- Verify that we waited the right amount of time.
      assert (ShouldBeTime = NOW);
      assert (A( I ) = 'X');
    end loop;
    
    -- 2. Test waiting on an array of scalar unresolved elements.
    ShouldBeTime := NOW + 1 ns;
    UnResolved <= ( '1','1','1','1','1','1','1','1','1','1',
                    '1','1','1','1','1','1','1','1','1','1',
                    '1','1','1','1','1','1','1','1','1','1',
                    '1','1' ) after 1 ns;
    wait on UnResolved;
    if (UnResolved /= ( '1','1','1','1','1','1','1','1','1','1',
                        '1','1','1','1','1','1','1','1','1','1',
                        '1','1','1','1','1','1','1','1','1','1',
                        '1','1' )  and ShouldBeTime /= Now) then
      k := 1;
    end if;
    -- Verify that we waited allright.
    assert (ShouldBeTime = NOW);
    for I in 0 to 31 loop
      assert ( UnResolved( I ) = '1');
    end loop;
    
    -- 3. Test waiting on a record.
    RECSIG.R1 <= 'X' after 1 ns;
    RECSIG.R2 <= 'X' after 2 ns;
    ShouldBeTime := NOW + 1 ns;
    wait on RECSIG;
    if (RECSIG.R1 /= 'X' and ShouldBeTime /= Now) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (RECSIG.R1 = 'X');
    ShouldBeTime := NOW + 1 ns;
    wait on RECSIG;
    if (RECSIG.R2 /= 'X' and ShouldBeTime /= Now) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW);
    assert (RECSIG.R2 = 'X');

    assert NOT( k=0 ) 
      report "***PASSED TEST: c08s01b00x00p24n01i01209"
      severity NOTE;
    assert ( k=0 ) 
      report "***FAILED TEST: c08s01b00x00p24n01i01209 - The effect of a signal name denotes a signal of a composite type is as if name of each scalar subelement of that signal appears in the list." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s01b00x00p24n01i01209arch;
