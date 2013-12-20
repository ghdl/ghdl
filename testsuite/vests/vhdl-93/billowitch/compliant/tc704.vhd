
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
-- $Id: tc704.vhd,v 1.3 2001-10-29 02:12:46 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------


--                 ****************************               --
-- Ported to VHDL 93 by port93.pl - Tue Nov  5 16:38:07 1996  --
--                 ****************************               --



ENTITY c03s04b01x00p23n01i00704ent IS
END c03s04b01x00p23n01i00704ent;

ARCHITECTURE c03s04b01x00p23n01i00704arch OF c03s04b01x00p23n01i00704ent IS

BEGIN
  TESTING: PROCESS
    -- Declare the type and the file.
    subtype BIT_VECTOR5 is BIT_VECTOR( 1 to 5 );
    type FT is file of BIT_VECTOR5;

    -- Declare the actual file to write.
    file FILEV : FT open write_mode is "iofile.57";

    -- Declare a variable.
    constant CON : BIT_VECTOR5 := B"10101";
    variable VAR : BIT_VECTOR5 := CON;

  BEGIN
    -- Write out the file.
    for I in 1 to 100 loop
      WRITE( FILEV,VAR );
    end loop;
    assert FALSE
      report "***PASSED TEST: c03s04b01x00p23n01i00704 - The output file will tested by test file s010436.vhd"
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c03s04b01x00p23n01i00704arch;
