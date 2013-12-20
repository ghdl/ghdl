
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
-- $Id: tc2402.vhd,v 1.2 2001-10-26 16:30:18 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x00p08n02i02402ent IS
END c07s03b02x00p08n02i02402ent;

ARCHITECTURE c07s03b02x00p08n02i02402arch OF c07s03b02x00p08n02i02402ent IS

BEGIN
  TESTING: PROCESS
    type rec_type is record
                       ele_1 : integer;
                       ele_2 : integer;
                     end record;
    variable v20 : rec_type;
  BEGIN
    v20 := (1 + 1 => 20, ele_2 => 0);  -- Failure_here
    -- simple expression
    -- associations allowed only in array agregates.
    assert FALSE 
      report "***FAILED TEST: c07s03b02x00p08n02i02402 - Element associations by simple expressions allowed only in array aggregates."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x00p08n02i02402arch;
