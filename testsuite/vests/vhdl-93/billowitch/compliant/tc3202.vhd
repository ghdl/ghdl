
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
-- $Id: tc3202.vhd,v 1.3 2001-10-29 02:12:45 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library std;
use std.textio.all;
ENTITY c14s03b00x00p56n01i03202ent IS
END c14s03b00x00p56n01i03202ent;

ARCHITECTURE c14s03b00x00p56n01i03202arch OF c14s03b00x00p56n01i03202ent IS

BEGIN
  TESTING: PROCESS
    file F   : TEXT open read_mode is "iofile.61";
    variable    L   : LINE;
    variable    Bi   : BIT;
    variable    Bo   : BOOLEAN;
    variable    BV   : Bit_Vector(1 to 60);
    variable    BV2   : Bit_Vector(1 to 60);

    -- Define the ScanForStars subprogram
    procedure ScanForStars(L: inout Line) is
      variable Index : Natural := 1;
      variable C1, C2: Character := ' ';
    begin
      while C1 = ' ' loop
        Read(L, C1);
      end loop;
      Read(L, C2);
      assert C1 = '*' and C2 = '*'
        report "Could not find two stars";
    end;
  BEGIN
    -- Read the entire line..
    READLINE(F, L);
    assert L.all = "hello world"
      report "Could not find opening banner...";
    
    -- Read the blank line...
    READLINE(F, L);
    assert L.all = ""
      report "Could not find blank line...";
    
    -- Read some BITS...
    for width in 1 to 10 loop
      READLINE(F, L);
      READ(L, Bi);
      assert Bi = '0'
        report "Failed in Read(BIT).  (Should be '0')";
      ScanForStars(L);
      READ(L, Bi);
      assert Bi = '1'
        report "Failed in Read(BIT).  (Should be '1')";
      ScanForStars(L);
    end loop;
    READLINE(F, L);
    
    -- Read some Bit vectors...
    for i in BV'Range loop
      BV(i) := Bit'Val(Boolean'Pos(BV'Right mod i = 0));
    end loop;
    
    for width in 15 downto 1 loop
      READLINE(F, L);
      READ(L, BV2(1 to 2*width));
      assert BV2(1 to 2*width) = BV((15-width)*2+1 to 30)
        report "Failed in Read(BIT_VECTOR). (Left side)";
      ScanForStars(L);
      READ(L, BV2(1 to 2*width));
      assert BV2(1 to 2*width) = BV(1 to 2*width)
        report "Failed in Read(BIT_VECTOR). (Right side)";
    end loop;
    READLINE(F, L);
    
    -- Read some BOOLEANs...
    for i in 10 downto 1 loop
      READLINE(F, L);
      READ(L, Bo);
      assert Bo = FALSE
        report "Failed in Read(BOOLEAN).  (Left side)";
      ScanForStars(L);
      READ(L, Bo);
      assert Bo = TRUE
        report "Failed in Read(BOOLEAN).  (Right side)";
    end loop;
    READLINE(F, L);
    assert FALSE
      report "***PASSED TEST: c14s03b00x00p56n01i03202 - This test file will read in an TEXT file with predefined data type BIT, BITVECOTR, BOOLEAN and STRING, and needs manual check to make sure that there is no ERROR assertion note."
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c14s03b00x00p56n01i03202arch;
