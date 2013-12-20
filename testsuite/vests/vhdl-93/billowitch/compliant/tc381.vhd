
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
-- $Id: tc381.vhd,v 1.2 2001-10-26 16:29:53 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x01p04n01i00381ent IS
END c03s02b01x01p04n01i00381ent;

ARCHITECTURE c03s02b01x01p04n01i00381arch OF c03s02b01x01p04n01i00381ent IS
  type bit_vctor is array (bit range '0' to '1') of integer;
BEGIN
  TESTING: PROCESS
    variable k : bit_vctor;
  BEGIN
    k('0') := 1;
    k('1') := 0;
    assert NOT (k('0') = 1 and
                k('1') = 0 ) 
      report "***PASSED TEST: c03s02b01x01p04n01i00381" 
      severity NOTE;
    assert     (k('0') = 1 and
                k('1') = 0 ) 
      report "***FAILED TEST: c03s02b01x01p04n01i00381 - An index constraint is compatible with the type denoted by the type mark if and only if the constraint defined by each discrete range is compatible with the corresponding subtype." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p04n01i00381arch;
