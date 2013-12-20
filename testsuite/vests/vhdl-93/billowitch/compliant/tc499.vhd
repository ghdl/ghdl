
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
-- $Id: tc499.vhd,v 1.2 2001-10-26 16:29:55 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b02x00p02n01i00499ent IS
END c03s02b02x00p02n01i00499ent;

ARCHITECTURE c03s02b02x00p02n01i00499arch OF c03s02b02x00p02n01i00499ent IS
  type rec_type is
    record
      x : integer;
      y : real;
      z : boolean;
      b : bit;
    end record;
  type rec2_type is
    record
      x : integer;
      y : integer;
      z : boolean;
      b : rec_type;
    end record;
BEGIN
  TESTING: PROCESS
    variable v2 : rec2_type;
  BEGIN
    v2.x  := 12;
    v2.y  := 10;
    v2.z  := true;
    v2.b.b  := bit'('0');
    assert NOT(v2.x=12 and v2.y=10 and v2.z=true and v2.b.b ='0')
      report "***PASSED TEST: c03s02b02x00p02n01i00499"
      severity NOTE;
    assert (v2.x=12 and v2.y=10 and v2.z=true and v2.b.b ='0')
      report "***FAILED TEST: c03s02b02x00p02n01i00499 - The record type definition consists of the reserved word record, one or more element declarations, and the reserved words end record."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b02x00p02n01i00499arch;
