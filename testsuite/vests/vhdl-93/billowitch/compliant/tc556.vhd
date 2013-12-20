
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
-- $Id: tc556.vhd,v 1.3 2001-10-29 02:12:45 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------


--                 ****************************               --
-- Ported to VHDL 93 by port93.pl - Tue Nov  5 16:37:29 1996  --
--                 ****************************               --



--                 ****************************                   --
-- Reversed to VHDL 87 by reverse87.pl - Tue Nov  5 11:25:26 1996  --
--                 ****************************                    --



--                 ****************************               --
-- Ported to VHDL 93 by port93.pl - Mon Nov  4 17:36:02 1996  --
--                 ****************************               --



ENTITY c03s04b01x00p01n01i00556ent IS
END c03s04b01x00p01n01i00556ent;

ARCHITECTURE c03s04b01x00p01n01i00556arch OF c03s04b01x00p01n01i00556ent IS
  type natural_vector  is array (natural range <>) of natural;
  type natural_vector_file is file of natural_vector;
BEGIN
  TESTING: PROCESS
    file filein : natural_vector_file open write_mode is "iofile.25";
  BEGIN
    for i in 1 to 100 loop
      write(filein,(1,2,3,4));
    end loop;
    assert FALSE
      report "***PASSED TEST: c03s04b01x00p01n01i00556 - The output file will be verified by test s010234.vhd."
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c03s04b01x00p01n01i00556arch;
