
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
-- $Id: tc1737.vhd,v 1.2 2001-10-26 16:30:03 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s04b00x00p09n01i01737ent IS
END c09s04b00x00p09n01i01737ent;

ARCHITECTURE c09s04b00x00p09n01i01737arch OF c09s04b00x00p09n01i01737ent IS
  signal   s1 : bit;
  signal    s2 : bit;
BEGIN

  s1   <= not s1 after 70 ns;
  s2   <= not s2 after 30 ns;

  block_label1 : BLOCK (s1 = '1')
  begin
    assert (s2 = s2'last_value)
      report "PASSED TEST"
      severity NOTE;
    TESTING: PROCESS(s2)
    BEGIN
      assert FALSE 
        report "***PASSED TEST: c09s04b00x00p09n01i01737 - This test needs manual check, depend on the simulation time, the assertion 'PASSED TEST' should fire every time s2 is changed regardless of the value of the signal GUARD."
        severity NOTE;
    END PROCESS TESTING;
  end block block_label1;

END c09s04b00x00p09n01i01737arch;
