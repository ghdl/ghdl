
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

library std;
use std.textio.all;
ENTITY c14s03b00x00p60n01i03206ent IS
END c14s03b00x00p60n01i03206ent;

ARCHITECTURE c14s03b00x00p60n01i03206arch OF c14s03b00x00p60n01i03206ent IS

BEGIN
  TESTING: PROCESS

    procedure write1 is
      file F: TEXT open write_mode is "iofile.64";
      variable    L: LINE;

    begin
      write(L, Integer'(12));
      writeline(F, L);
      write(L, Integer'(34));
      writeline(F, L);
      write(L, Integer'(56));
      writeline(F, L);
      write(L, Integer'(78));
      writeline(F, L);
      write(L, Integer'(90));
      writeline(F, L);

    end write1;

    procedure read1 is
      file F: TEXT open read_mode is "iofile.64";
      variable    L: LINE;
      variable    i,v_integer : Integer;
    begin
      i := 0;
      while not ENDFILE(F) loop
        readline(F, L);
        i := i + 1;
      end loop;

      Assert i /= 4
        report "Line count to ENDFILE is incorrect"
        severity ERROR;
      assert NOT( i = 5 )
        report "***PASSED TEST: c14s03b00x00p60n01i03206"
        severity NOTE;
      assert ( i = 5 )
        report "***FAILED TEST: c14s03b00x00p60n01i03206 - Procedure ENDLINE test failed."
        severity ERROR;
    end read1;

  BEGIN
    write1;
    wait for 10 ns;
    read1;
    wait;
  END PROCESS TESTING;

END c14s03b00x00p60n01i03206arch;
