
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
-- $Id: tc1023.vhd,v 1.2 2001-10-26 16:30:05 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s03b00x00p10n02i01023ent IS
END c06s03b00x00p10n02i01023ent;

ARCHITECTURE c06s03b00x00p10n02i01023arch OF c06s03b00x00p10n02i01023ent IS

BEGIN
  TESTING: PROCESS
    variable j : integer;
  BEGIN
    L1: for i in 1 to 10 loop
      e.j := L1.i;
    end loop;
    j := L1.i; -- illegal as reference to L1.i is allowed within the
    -- loop L1 only.
    assert FALSE 
      report "***FAILED TEST: c06s03b00x00p10n02i01023 - An expanded name denoting an entity declared within a named construct is allowed only within the construct." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s03b00x00p10n02i01023arch;
