
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
-- $Id: tc710.vhd,v 1.3 2001-10-29 02:12:46 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------


--                 ****************************               --
-- Ported to VHDL 93 by port93.pl - Tue Nov  5 16:38:09 1996  --
--                 ****************************               --



ENTITY c03s04b01x00p23n01i00710ent IS
END c03s04b01x00p23n01i00710ent;

ARCHITECTURE c03s04b01x00p23n01i00710arch OF c03s04b01x00p23n01i00710ent IS
  -- Some constants...
  constant StringLength: INTEGER := 16;
  constant NumOfStrings: INTEGER := 5;
  
  type t2 is array(1 to 5) of INTEGER;
  type t3 is array(INTEGER range <>) of t2;
  
  -- Objects...
  constant integer_array: t3(1 to 8) :=
    ( (0, 1, 2, 3, 4),    (2, 4, 6, 8, 10),
      (-2, -1, 0, 1, 2),    (13, 2, -45, 6, 1),
      (1, 4, 16, 64, 256),    (1, 4, 9, 16, 25),
      (1, 2, 4, 8, 16),    (5, 4, 3, 2, 1) );

  type    ft3 is file of t3;
BEGIN
  TESTING: PROCESS
    -- Declare the actual file to write.
    file FILEV : ft3 open write_mode is "iofile.60";

    -- Declare a variable.
    variable VAR : t3(3 downto 0);
  BEGIN
    -- Write out the file.
    for I in 1 to 2  loop
      VAR := integer_array((i-1)*4+1 to i*4);
      write(FILEV, VAR);
    end loop;
    assert FALSE
      report "***PASSED TEST: c03s04b01x00p23n01i00710 - The output file will tested by test file s010442.vhd"
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c03s04b01x00p23n01i00710arch;
