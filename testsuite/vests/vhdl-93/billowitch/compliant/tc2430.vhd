
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
-- $Id: tc2430.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x01p01n04i02430ent IS
END c07s03b02x01p01n04i02430ent;

ARCHITECTURE c07s03b02x01p01n04i02430arch OF c07s03b02x01p01n04i02430ent IS
  type sigrec1 is
    record
      B1 : bit;
      B2 : integer;
      B3 : bit;
    end record;
  signal S1 : sigrec1;
BEGIN
  TESTING: PROCESS
  BEGIN
    S1 <= (('0' and '1'), 2, '1');
    wait for 1 ns;
    assert NOT(S1.B1='0' and S1.B2=2 and S1.B3='1') 
      report "***PASSED TEST: c07s03b02x01p01n04i02430" 
      severity NOTE;
    assert (S1.B1='0' and S1.B2=2 and S1.B3='1') 
      report "***FAILED TEST: c07s03b02x01p01n04i02430 - Expression of an element association must have the same type as the associated record element."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x01p01n04i02430arch;
