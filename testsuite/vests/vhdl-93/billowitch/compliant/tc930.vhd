
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
-- $Id: tc930.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c10s04b00x00p03n01i00930pkg is
  constant x : integer := 2;
  constant y : real := 5.0;
  subtype register16 is bit_vector(15 downto 0);
  function "+" (l,r : bit_vector) return bit_vector;
end c10s04b00x00p03n01i00930pkg;

package body c10s04b00x00p03n01i00930pkg is
  function "+" (l,r : bit_vector) return bit_vector is
  begin
    return (B"1111010100101010");
  end;
end c10s04b00x00p03n01i00930pkg;


use work.c10s04b00x00p03n01i00930pkg."+";
use work.c10s04b00x00p03n01i00930pkg.register16;
ENTITY c10s04b00x00p03n01i00930ent IS
END c10s04b00x00p03n01i00930ent;

ARCHITECTURE c10s04b00x00p03n01i00930arch OF c10s04b00x00p03n01i00930ent IS
  signal   i_sig  :  register16 := B"1010_1110_1010_0011";
BEGIN
  TESTING: PROCESS
  BEGIN
    i_sig <= i_sig + i_sig after 10 ns;
    wait for 11 ns;
    assert NOT(i_sig = "1111010100101010")
      report "***PASSED TEST: c10s04b00x00p03n01i00930"
      severity NOTE;
    assert (i_sig = "1111010100101010")
      report "***FAILED TEST: c10s04b00x00p03n01i00930 - The operator is visible in the declaration region if the suffix of a selected name in a use clause is an operator."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c10s04b00x00p03n01i00930arch;
