
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
-- $Id: tc878.vhd,v 1.2 2001-10-26 16:30:01 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c10s01b00x00p03n01i00878pkg is
  constant UNIT_DELAY: TIME := 1 ns;
end c10s01b00x00p03n01i00878pkg;

-- a nand gate
entity ENT1 is
  port ( BITIN1, BITIN2 : in  BIT;
         BITOUT: out BIT );
end ENT1;

use WORK.c10s01b00x00p03n01i00878pkg.UNIT_DELAY;
architecture ARC1 of ENT1 is
begin
  BITOUT <= ( BITIN1 nand BITIN2 ) after UNIT_DELAY;
end ARC1;

configuration CON1 of ENT1 is
  for ARC1
  end for;
end CON1;

-- build an inverter from nand-nand logic
entity ENT2 is
  port ( GOING_IN: in BIT;
         COMING_OUT: out BIT );
end ENT2;

architecture ARC2 of ENT2 is
  component NAND_BOX
    port ( IN1, IN2: in BIT; OUT1: out BIT );
  end component;
  signal STUCKAT_HIGH: BIT := '1';
begin
  NAND_COMP: NAND_BOX port map ( GOING_IN, STUCKAT_HIGH, COMING_OUT );
end ARC2;

use WORK.CON1;
configuration CON2 of ENT2 is
  for ARC2
    for NAND_COMP: NAND_BOX
      use configuration CON1
        port map ( IN1, IN2, OUT1 );
    end for;
  end for;
end CON2;

-- declare a test bench
ENTITY c10s01b00x00p03n01i00878ent IS
END c10s01b00x00p03n01i00878ent;

use WORK.c10s01b00x00p03n01i00878pkg.UNIT_DELAY;
ARCHITECTURE c10s01b00x00p03n01i00878arch OF c10s01b00x00p03n01i00878ent IS
  component INV
    port ( ENTRA: in BIT; SALE: out BIT );
  end component;
  signal SIGIN, SIGOUT: BIT;
BEGIN
  INVERTER: INV port map ( SIGIN, SIGOUT );
  TESTING: PROCESS
    variable   k : integer := 0;
  BEGIN
    SIGIN <= '0';
    wait for ( 2 * UNIT_DELAY );
    if (SIGOUT /= '1') then
      k := 1;
    end if;
    assert ( SIGOUT = '1' )
      report "didn't invert low to high" severity FAILURE;
    wait for ( 3 * UNIT_DELAY );
    SIGIN <= '1';
    wait for ( 2 * UNIT_DELAY );
    if (SIGOUT /= '0') then
      k := 1;
    end if;
    assert ( SIGOUT = '0' )
      report "didn't invert high to low" severity FAILURE;
    assert NOT( k=0 )
      report "***PASSED TEST: c10s01b00x00p03n01i00878"
      severity NOTE;
    assert ( k=0 )
      report "***FAILED TEST: c10s01b00x00p03n01i00878 - A declartive region is formed by the text of a configuration declaration."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c10s01b00x00p03n01i00878arch;

use WORK.CON2;
configuration c10s01b00x00p03n01i00878cfg of c10s01b00x00p03n01i00878ent is
  for c10s01b00x00p03n01i00878arch
    for INVERTER: INV
      use configuration CON2
        port map ( ENTRA, SALE );
    end for;
  end for;
end c10s01b00x00p03n01i00878cfg;
