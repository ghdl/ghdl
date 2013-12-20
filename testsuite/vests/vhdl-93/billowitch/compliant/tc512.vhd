
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
-- $Id: tc512.vhd,v 1.2 2001-10-26 16:29:55 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b02x00p07n01i00512ent IS
END c03s02b02x00p07n01i00512ent;

ARCHITECTURE c03s02b02x00p07n01i00512arch OF c03s02b02x00p07n01i00512ent IS
  type DATE is
    record
      DAY,D1,D2  : Integer;
      MONTH : Integer;
      YEAR : Integer;
    end record;
  type DAT is
    record
      DAY : Integer;
      D1  : Integer;
      D2  : Integer;
      MONTH : Integer;
      YEAR : Integer;
    end record;
BEGIN
  TESTING: PROCESS
    variable V1 : DATE    := (5,5,5,10,15) ;
    variable V2 : DAT    := (5,5,5,10,15);
  BEGIN
    assert NOT(V1.D1 = V2.D1 and V1.D2 = V2.D2 and V1.DAY = V2.DAY and V1.Month = V2.Month and V1.Year = V2.Year )
      report "***PASSED TEST: c03s02b02x00p07n01i00512"
      severity NOTE;
    assert (V1.D1 = V2.D1 and V1.D2 = V2.D2 and V1.DAY = V2.DAY and V1.Month = V2.Month and V1.Year = V2.Year )
      report "***FAILED TEST: c03s02b02x00p07n01i00512 - An element declaration with several identifiers is equivalent to a sequence of single element declarations."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b02x00p07n01i00512arch;
