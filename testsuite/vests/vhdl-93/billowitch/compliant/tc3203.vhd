
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
-- $Id: tc3203.vhd,v 1.3 2001-10-29 02:12:45 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library std;
use std.textio.all;
ENTITY c14s03b00x00p56n01i03203ent IS
END c14s03b00x00p56n01i03203ent;

ARCHITECTURE c14s03b00x00p56n01i03203arch OF c14s03b00x00p56n01i03203ent IS

BEGIN
  TESTING: PROCESS
    file F   : TEXT open write_mode is "iofile.61";
    variable    L   : LINE;
    variable    BV   : Bit_Vector(1 to 60);
  BEGIN
    -- Write an arbitrary line...
    L := new STRING'("hello world");
    WRITELINE(F, L);
    
    -- Write a blank line...
    WRITELINE(F, L);
    
    -- Write some BITs...
    for i in 1 to 10 loop
      WRITE(L, Bit'('0'), RIGHT, i);
      WRITE(L, String'("**"), RIGHT, 12-i);
      WRITE(L, Bit'('1'), LEFT, i);
      WRITE(L, String'("**"), LEFT, 0);
      WRITELINE(F, L);
    end loop;
    WRITELINE(F, L);
    
    -- Write some Bit vectors...
    for i in BV'Range loop
      BV(i) := Bit'Val(Boolean'Pos(BV'Right mod i = 0));
    end loop;
    
    for i in 15 downto 1 loop
      WRITE(L, BV((15-i)*2+1 to 30), RIGHT, 30);
      WRITE(L, String'("**"), RIGHT, 3);
      WRITE(L, BV(1 to 2*i), LEFT, 30);
      WRITELINE(F, L);
    end loop;
    WRITELINE(F, L);
    
    -- Write some BOOLEANs...
    for i in 10 downto 1 loop
      WRITE(L, Boolean'(FALSE), RIGHT, i);
      WRITE(L, String'("**"), RIGHT, 12-i);
      WRITE(L, Boolean'(TRUE), RIGHT, 11-i);
      WRITELINE(F, L);
    end loop;
    WRITELINE(F, L);
    wait for 10 ns;
    assert FALSE
      report "***PASSED TEST: c14s03b00x00p56n01i03203 - This test file will output an TEXT file with predefined data type BIT, BITVECOTR, BOOLEAN and STRING, and next test file will be used to verify the correctness of the this writing test."
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c14s03b00x00p56n01i03203arch;
