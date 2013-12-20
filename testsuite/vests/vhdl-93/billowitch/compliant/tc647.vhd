
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

ENTITY c03s04b01x00p01n01i00647ent IS
END c03s04b01x00p01n01i00647ent;

ARCHITECTURE c03s04b01x00p01n01i00647arch OF c03s04b01x00p01n01i00647ent IS

  type current is range -2147483647 to +2147483647
    units
      nA;
      uA = 1000 nA;
      mA = 1000 uA;
      A  = 1000 mA;
    end units;

  type current_file is file of current;
  constant C47 : current := 1 A;

  signal       k    : integer := 0;

BEGIN
  TESTING: PROCESS
    file filein    : current_file open read_mode is "iofile.62";
    variable  v    : current;
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
      report "***PASSED TEST: c03s04b01x00p01n01i00647"
      severity NOTE;
    assert (k = 0)
      report "***FAILED TEST: c03s04b01x00p01n01i00647 - File reading operation (current_file type) failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s04b01x00p01n01i00647arch;
