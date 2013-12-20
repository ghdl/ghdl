
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

ENTITY c03s04b01x00p01n01i00650ent IS
END c03s04b01x00p01n01i00650ent;

ARCHITECTURE c03s04b01x00p01n01i00650arch OF c03s04b01x00p01n01i00650ent IS

  type resistance is range -2147483647 to +2147483647
    units
      uOhm;
      mOhm    = 1000 uOhm;
      Ohm    = 1000 mOhm;
      KOhm     = 1000 Ohm;
    end units;

  type resistance_file is file of resistance;
  constant C47 : resistance := 1 Ohm;

  signal       k    : integer := 0;

BEGIN
  TESTING: PROCESS
    file filein    : resistance_file open read_mode is "iofile.63";
    variable  v    : resistance;
  BEGIN
    for i in 1 to 100 loop
      assert(endfile(filein) = false) report"end of file reached before expected";
      read(filein,v);
      if (v /= C47) then
        k <= 1;
      end if;
    end loop;
    wait for 1 ns;
    assert NOT(k = 0)
      report "***PASSED TEST: c03s04b01x00p01n01i00650"
      severity NOTE;
    assert (k = 0)
      report "***FAILED TEST: c03s04b01x00p01n01i00650 - File reading operation (resistance_file type) failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s04b01x00p01n01i00650arch;
