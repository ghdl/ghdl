
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
-- $Id: tc882.vhd,v 1.2 2001-10-26 16:30:04 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity c10s01b00x00p07n01i00882ent_a is
  generic (
    GS1: INTEGER := 3;
    GS2: INTEGER := 9
    );
  port (
    PS1: out INTEGER;
    PS2: out INTEGER
    );
end c10s01b00x00p07n01i00882ent_a;

architecture c10s01b00x00p07n01i00882arch_a of c10s01b00x00p07n01i00882ent_a is
  
begin
  process
  begin
    PS1 <= GS1 + 1;
    PS2 <= GS2 + 2;
    wait; -- forever
  end process;
end c10s01b00x00p07n01i00882arch_a;

use WORK.c10s01b00x00p07n01i00882ent_a;
ENTITY c10s01b00x00p07n01i00882ent IS
END c10s01b00x00p07n01i00882ent;

ARCHITECTURE c10s01b00x00p07n01i00882arch OF c10s01b00x00p07n01i00882ent IS
  
  signal G1: INTEGER;
  signal G2: INTEGER;
  signal A : INTEGER;
  signal B : INTEGER;
  component c10s01b00x00p07n01i00882ent_a
    generic ( G1, G2: INTEGER );
    port ( A, B: out INTEGER );
  end component;
  signal S1: INTEGER;
  signal S2: INTEGER;
  
BEGIN
  
  A1: c10s01b00x00p07n01i00882ent_a generic map ( 3, 9 ) port map ( S1, S2 );
  
  -- verification
  TESTING: PROCESS
  BEGIN
    wait for 5 ns;
    assert NOT( S1=4 and S2=11 )
      report "***PASSED TEST: c10s01b00x00p07n01i00882"
      severity NOTE;
    assert ( S1=4 and S2=11 )
      report "***FAILED TEST: c10s01b00x00p07n01i00882 - A declarative region is formed by the text of a component declaration."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c10s01b00x00p07n01i00882arch;

configuration c10s01b00x00p07n01i00882cfg of c10s01b00x00p07n01i00882ent is
  for c10s01b00x00p07n01i00882arch
    for A1: c10s01b00x00p07n01i00882ent_a
      use entity c10s01b00x00p07n01i00882ent_a (c10s01b00x00p07n01i00882arch_a )
        generic map ( G1, G2 )
        port map ( A, B );
    end for;
  end for;
end c10s01b00x00p07n01i00882cfg;
