
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
-- $Id: tc1347.vhd,v 1.2 2001-10-26 16:29:40 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s04b01x00p07n01i01347ent IS
END c08s04b01x00p07n01i01347ent;

ARCHITECTURE c08s04b01x00p07n01i01347arch OF c08s04b01x00p07n01i01347ent IS
  signal Add_bus : integer := 67;
BEGIN
  TESTING: PROCESS
  BEGIN
    Add_bus <= 1 after 5 ns, 6 after 10 ns, 12 after 19 ns;
    Add_bus <= 6 after 12 ns, 20 after 19 ns, 6 after 21 ns;
    wait;
  END PROCESS TESTING;

  TEST : PROCESS(Add_bus)
    variable ok : integer := 1;
  BEGIN
    if (now = 5 ns) then
      if (Add_bus /= 67) then
        ok := 0;
      end if;
    elsif (now = 10 ns) then
      if (Add_bus /= 6) then
        ok := 0;
      end if;
    elsif (now = 12 ns) then
      if (Add_bus /= 6) then
        ok := 0;
      end if;
    elsif (now = 19 ns) then
      if (Add_bus /= 20) then
        ok := 0;
      end if;
    end if;
    if (now = 21 ns) then
      assert NOT( Add_bus = 6 and ok = 1)
        report "***PASSED TEST: c08s04b01x00p07n01i01347"
        severity NOTE;
      assert ( Add_bus = 6 and ok = 1)
        report "***FAILED TEST: c08s04b01x00p07n01i01347 - The sequence of transactions is used to update the projected output waveform representing the current and future values of the driver associated with the signal assignment statement. And this test failed."
        severity ERROR;
    end if;
  END PROCESS TEST;

END c08s04b01x00p07n01i01347arch;
