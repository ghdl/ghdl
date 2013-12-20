
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
-- $Id: tc1316.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s04b00x00p07n04i01316ent IS
END c08s04b00x00p07n04i01316ent;

ARCHITECTURE c08s04b00x00p07n04i01316arch OF c08s04b00x00p07n04i01316ent IS
  type sigrec is
    record
      B1 : bit;
      B2 : integer;
      B3 : boolean;
    end record;
  signal S1 : bit;
  signal S2 : integer;
  signal S3 : boolean;
BEGIN
  TESTING: PROCESS
  BEGIN
    (S1, S2, S3) <= sigrec'('0',2,false);
    wait for 10 ns;
    assert NOT( (S1 = '0') and (S2 = 2) and (S3 = false) )
      report "***PASSED TEST: c08s04b00x00p07n04i01316"
      severity NOTE;
    assert ( (S1 = '0') and (S2 = 2) and (S3 = false) )
      report "***FAILED TEST: c08s04b00x00p07n04i01316 - Right hand side values are assigned to the drivers associated with the signal named as the corresponding subelement of the aggreate."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s04b00x00p07n04i01316arch;
