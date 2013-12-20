
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
-- $Id: tc777.vhd,v 1.2 2001-10-26 16:30:00 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c01s01b01x02p10n01i00777ent_a IS
  port      (A : linkage integer;
             B : linkage integer;
             C : linkage integer;
             D : linkage integer);
END c01s01b01x02p10n01i00777ent_a;

ARCHITECTURE c01s01b01x02p10n01i00777arch_a OF c01s01b01x02p10n01i00777ent_a IS

BEGIN
  test : process
  begin
    wait;
  end process test;
END c01s01b01x02p10n01i00777arch_a;



ENTITY c01s01b01x02p10n01i00777ent IS
  port (X : linkage    integer;
        Y : buffer    integer;
        Z : inout    integer);
END c01s01b01x02p10n01i00777ent;

ARCHITECTURE c01s01b01x02p10n01i00777arch OF c01s01b01x02p10n01i00777ent IS
  component c01s01b01x02p10n01i00777ent_b
    port      (A : linkage integer;
               B : linkage integer;
               C : linkage integer;
               D : linkage integer);
  end component;
  for L : c01s01b01x02p10n01i00777ent_b use entity work.c01s01b01x02p10n01i00777ent_a(c01s01b01x02p10n01i00777arch_a);

  signal M : integer;

BEGIN
  L:c01s01b01x02p10n01i00777ent_b port map 
    ( A => M,
      B => X,
      C => Y,
      D => Z);
  TESTING: PROCESS
  BEGIN
    assert FALSE
      report "***PASSED TEST: c01s01b01x02p10n01i00777"
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c01s01b01x02p10n01i00777arch;
