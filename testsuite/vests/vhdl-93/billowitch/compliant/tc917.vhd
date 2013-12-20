
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
-- $Id: tc917.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity c04s04b00x00p02n01i00917ent_a is
  generic ( i_generic : integer; r_generic : real );
end c04s04b00x00p02n01i00917ent_a;

architecture c04s04b00x00p02n01i00917arch_a of c04s04b00x00p02n01i00917ent_a is
begin
  TESTING : PROCESS
  BEGIN
    assert NOT( i_generic = 0 and r_generic = 15.0 )
      report "***PASSED TEST: c04s04b00x00p02n01i00917"
      severity NOTE;
    assert ( i_generic = 0 and r_generic = 15.0 )
      report "***FAILED TEST: c04s04b00x00p02n01i00917 - "
      severity ERROR;
    wait;
  END PROCESS TESTING;
end c04s04b00x00p02n01i00917arch_a;


ENTITY c04s04b00x00p02n01i00917ent IS
  subtype    register16    is bit_vector(15 downto 0);
  constant    reg :    register16 := B"1001_0001_1010_1111";
END c04s04b00x00p02n01i00917ent;

ARCHITECTURE c04s04b00x00p02n01i00917arch OF c04s04b00x00p02n01i00917ent IS
  component d
    generic ( i_generic : integer := 1; r_generic : real := 2.0 );
  end component;
  for instance : d use entity work.c04s04b00x00p02n01i00917ent_a(c04s04b00x00p02n01i00917arch_a);
BEGIN
  instance : d generic map (reg'low, real(reg'high));
  assert ( reg'low   = 0 )  report "reg'low   /= 0"  severity FAILURE;
  assert ( reg'high  = 15 ) report "reg'high  /= 15" severity FAILURE;
  assert ( reg'right = 0 )  report "reg'right /= 0"  severity FAILURE;
  assert ( reg'left  = 15 ) report "reg'left  /= 15" severity FAILURE;

END c04s04b00x00p02n01i00917arch;
