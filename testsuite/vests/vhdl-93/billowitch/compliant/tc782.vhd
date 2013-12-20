
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
-- $Id: tc782.vhd,v 1.2 2001-10-26 16:30:00 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c01s01b01x02p12n04i00782ent_a IS
  port (
    C2 : inout      Bit;
    C3 : linkage    Bit;
    C4 : out        Bit;
    C5 : Buffer     Bit
    );
END c01s01b01x02p12n04i00782ent_a;

ARCHITECTURE c01s01b01x02p12n04i00782arch_a OF c01s01b01x02p12n04i00782ent_a IS
BEGIN
END c01s01b01x02p12n04i00782arch_a;



ENTITY c01s01b01x02p12n04i00782ent IS
  port (
    A2 : inout      Bit;
    A3 : linkage    Bit;
    A4 : out        Bit;
    A5 : Buffer     Bit
    ) ;
END c01s01b01x02p12n04i00782ent;

ARCHITECTURE c01s01b01x02p12n04i00782arch OF c01s01b01x02p12n04i00782ent IS
  component c01s01b01x02p12n04i00782ent_b
    port (
      C2 : inout      Bit;
      C3 : linkage    Bit;
      C4 : out        Bit;
      C5 : Buffer     Bit
      );
  end component;
  for L : c01s01b01x02p12n04i00782ent_b use entity work.c01s01b01x02p12n04i00782ent_a(c01s01b01x02p12n04i00782arch_a);
BEGIN
  L : c01s01b01x02p12n04i00782ent_b port map ( C2 => open, C3 => open, C4 => open, C5 => open );
  TESTING: PROCESS
  BEGIN
    assert FALSE
      report "***PASSED TEST: c01s01b01x02p12n04i00782"
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c01s01b01x02p12n04i00782arch;
