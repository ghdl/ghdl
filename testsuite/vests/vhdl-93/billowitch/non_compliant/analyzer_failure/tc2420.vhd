
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
-- $Id: tc2420.vhd,v 1.2 2001-10-26 16:30:18 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x01p01n01i02420ent IS
END c07s03b02x01p01n01i02420ent;

ARCHITECTURE c07s03b02x01p01n01i02420arch OF c07s03b02x01p01n01i02420ent IS

BEGIN
  TESTING: PROCESS
    type rec is record
                  ele_1 : integer;
                  ele_2 : real;
                  ele_3 : boolean;
                  ele_4 : integer;
                end record;
    variable p : rec :=
      (ele_3 => true, 
       ele_1 => 1,
       ele_2 => 3.4,
       ele_5 => 12); -- Failure_here
    -- ele_5 does not belong to the record type.
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c07s03b02x01p01n01i02420 - Element names must denote elments of the record type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x01p01n01i02420arch;
