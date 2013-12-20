
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
-- $Id: tc151.vhd,v 1.2 2001-10-26 16:30:10 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x02p16n01i00151ent_a IS
  port (signal input_1 : in  bit;
        signal input_2 : in  bit_vector;
        signal output  : out bit);
END c04s03b02x02p16n01i00151ent_a;

ARCHITECTURE c04s03b02x02p16n01i00151arch_a OF c04s03b02x02p16n01i00151ent_a IS
BEGIN
END c04s03b02x02p16n01i00151arch_a;


ENTITY c04s03b02x02p16n01i00151ent IS
  port (X: in BIT; Z: out BIT);
END c04s03b02x02p16n01i00151ent;

ARCHITECTURE c04s03b02x02p16n01i00151arch OF c04s03b02x02p16n01i00151ent IS
  component input2
    port (signal input_1 : in  bit;
          signal input_2 : in  bit_vector;
          signal output  : out bit);
  end component;
  for G1 : input2 use entity work.ch04030202_p01601_02_ent_a(ch04030202_p01601_02_arch_a);
  type    bit_vector    is array (positive range <>) of bit;
  signal    A1       :  bit_vector;
BEGIN

  G1: input2 port map (X, A1, Z); -- Failure_here

  TESTING: PROCESS
  BEGIN
    assert FALSE
      report "***FAILED TEST: c04s03b02x02p16n01i00151 - The type of an actual should be same as that of the formal."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x02p16n01i00151arch;
