
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
-- $Id: tc3204.vhd,v 1.3 2001-10-29 02:12:45 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library STD;
use STD.TEXTIO.all;
ENTITY c14s03b00x00p59n01i03204ent IS
END c14s03b00x00p59n01i03204ent;

ARCHITECTURE c14s03b00x00p59n01i03204arch OF c14s03b00x00p59n01i03204ent IS

BEGIN
  TESTING: PROCESS
    file F_out   : Text open write_mode is "iofile.47";
    variable    L_out   : Line;
    type       TA    is array (INTEGER range <>) of TIME;
    constant    A   : TA := (-1 fs, 37582 ns, 1 ms + 1 ns + 1 ps);
  BEGIN

    for i in A'RANGE loop
      Write(L_out, A(i), UNIT=>fs, FIELD=>25);
      WriteLine(F_out, L_out);
      Write(L_out, A(i), UNIT=>ps, FIELD=>25);
      WriteLine(F_out, L_out);
      Write(L_out, A(i), UNIT=>ns, FIELD=>25);
      WriteLine(F_out, L_out);
      WriteLine(F_out, L_out);
    end loop;
    wait for 10 fs;
    assert FALSE
      report "***PASSED TEST: c14s03b00x00p59n01i03204 - This test will output an output file, and that file will be compared with s010401.ref file."
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c14s03b00x00p59n01i03204arch;
