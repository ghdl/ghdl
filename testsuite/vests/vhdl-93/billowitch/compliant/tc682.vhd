
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
-- $Id: tc682.vhd,v 1.3 2001-10-29 02:12:46 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------


--                 ****************************               --
-- Ported to VHDL 93 by port93.pl - Tue Nov  5 16:38:01 1996  --
--                 ****************************               --



--                 ****************************                   --
-- Reversed to VHDL 87 by reverse87.pl - Tue Nov  5 11:26:33 1996  --
--                 ****************************                    --



--                 ****************************               --
-- Ported to VHDL 93 by port93.pl - Mon Nov  4 17:36:40 1996  --
--                 ****************************               --


ENTITY c03s04b01x00p23n01i00682ent IS
END c03s04b01x00p23n01i00682ent;

ARCHITECTURE c03s04b01x00p23n01i00682arch OF c03s04b01x00p23n01i00682ent IS
  type FT is file of INTEGER;
BEGIN
  TESTING: PROCESS
    variable i3, i2, i1: INTEGER;
    file S1: FT open read_mode is "iofile.47";
  BEGIN
    wait for 10 ns;
    READ(S1,i3);
    READ(S1,i2);
    READ(S1,i1);
    wait for 10 ns;
    assert NOT( (i3 = 3) and (i2 = 2) and (i1 = 1) )
      report "***PASSED TEST: c03s04b01x00p23n01i00682"
      severity NOTE;
    assert ( (i3 = 3) and (i2 = 2) and (i1 = 1) )
      report "***FAILED TEST: c03s04b01x00p23n01i00682 - Procedure READ retrieves the next value from a file."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s04b01x00p23n01i00682arch;
