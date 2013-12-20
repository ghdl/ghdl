
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
-- $Id: tc1312.vhd,v 1.2 2001-10-26 16:30:09 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s04b00x00p07n03i01312ent IS
END c08s04b00x00p07n03i01312ent;

ARCHITECTURE c08s04b00x00p07n03i01312arch OF c08s04b00x00p07n03i01312ent IS
  type   BIT_VECTOR is array (natural range <>) of bit;
  type   INDEX   is range 3 downto 0;
  subtype   BVI   is BIT_VECTOR(INDEX);
  signal   S   : BVI;
BEGIN
  TESTING: PROCESS
    variable k : Index;
  BEGIN
    (S(3), S(k), S(1), S(0)) <= BVI'('1', others => '0');
    wait for 1 ns;
    assert FALSE 
      report "***FAILED TEST: c08s04b00x00p07n03i01312 - The expression in the element association is not locally static."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s04b00x00p07n03i01312arch;
