
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
-- $Id: tc781.vhd,v 1.2 2001-10-26 16:30:27 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c01s01b01x02p12n04i00781ent_a IS
  port (
    C1 : in         Bit;
    C2 : inout      Bit;
    C3 : linkage    Bit;
    C4 : out        Bit;
    C5 : Buffer     Bit
    );
END c01s01b01x02p12n04i00781ent_a;

ARCHITECTURE c01s01b01x02p12n04i00781arch_a OF c01s01b01x02p12n04i00781ent_a IS
BEGIN
END c01s01b01x02p12n04i00781arch_a;



ENTITY c01s01b01x02p12n04i00781ent IS
  port (
    A1 : in         Bit;
    A2 : inout      Bit;
    A3 : linkage    Bit;
    A4 : out        Bit;
    A5 : Buffer     Bit
    ) ;
END c01s01b01x02p12n04i00781ent;

ARCHITECTURE c01s01b01x02p12n04i00781arch OF c01s01b01x02p12n04i00781ent IS
  component c01s01b01x02p12n04i00781ent_b
    port (
      C1 : in         Bit;
      C2 : inout      Bit;
      C3 : linkage    Bit;
      C4 : out        Bit;
      C5 : Buffer     Bit
      );
  end component;
  for L : c01s01b01x02p12n04i00781ent_b use entity work.c01s01b01x02p12n04i00781ent_a(c01s01b01x02p12n04i00781arch_a);
BEGIN
  L : c01s01b01x02p12n04i00781ent_b port map ( C1 => open, C2 => open, C3 => open, C4 => open, C5 => open );
  TESTING: PROCESS
  BEGIN
    assert FALSE
      report "***FAILED TEST: c01s01b01x02p12n04i00781 - A port of mode in may not be unconnected."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c01s01b01x02p12n04i00781arch;
