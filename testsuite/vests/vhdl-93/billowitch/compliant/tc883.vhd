
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
-- $Id: tc883.vhd,v 1.2 2001-10-26 16:30:01 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c10s01b00x00p08n01i00883ent IS
END c10s01b00x00p08n01i00883ent;

ARCHITECTURE c10s01b00x00p08n01i00883arch OF c10s01b00x00p08n01i00883ent IS

  signal S1   : INTEGER;
  signal S2   : INTEGER;
  signal GS1   : INTEGER;
  signal GS2   : INTEGER;
  signal PS1   : INTEGER;
  signal PS2   : INTEGER;

BEGIN

  -- initialization block and process
  ALIST1SUB:
  block
    generic (
      GS1: INTEGER := 3;
      GS2: INTEGER := 9
      );
    generic map ( 3, 9 );
    port (
      PS1: out INTEGER;
      PS2: out INTEGER
      );
    port map ( S1, S2 );
  begin
    process
    begin
      PS1 <= GS1 + 1;
      PS2 <= GS2 + 2;
      wait;
    end process; -- forever, initialization complete
  end block ALIST1SUB;

  -- verification process
  TESTING: PROCESS
  BEGIN
    wait for 1 ns;
    assert NOT( S1=4 and S2=11 )
      report "***PASSED TEST: c10s01b00x00p08n01i00883"
      severity NOTE;
    assert ( S1=4 and S2=11 )
      report "***FAILED TEST: c10s01b00x00p08n01i00883 - A single declaration region is formed by the text of a block statement."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c10s01b00x00p08n01i00883arch;
