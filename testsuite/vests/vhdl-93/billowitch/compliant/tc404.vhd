
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
-- $Id: tc404.vhd,v 1.2 2001-10-26 16:29:53 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x01p18n01i00404ent IS
END c03s02b01x01p18n01i00404ent;

ARCHITECTURE c03s02b01x01p18n01i00404arch OF c03s02b01x01p18n01i00404ent IS
  type MEM is array (positive range <>) of BIT;
  type ME1 is array (natural range <>) of Integer;
  subtype ME2 is ME1(0 to 3);
  subtype M1 is MEM (1 to 5) ;
  procedure F2 (X1 : in MEM) is
  begin
    assert NOT((X1'LOW = 1) and (X1'High = 5))
      report "***PASSED TEST: c03s02b01x01p18n01i00404" 
      SEVERITY NOTE;
    assert ((X1'LOW = 1) and (X1'High = 5))
      report "***FAILED TEST: c03s02b01x01p18n01i00404 - The index ranges are obtained from the actual designator." 
      SEVERITY ERROR;
  end F2;
  signal S1 : M1 := "01110" ;
BEGIN
  TESTING: PROCESS
  BEGIN
    F2(S1) ;  -- No_failure_here
    wait;
  END PROCESS TESTING;

END c03s02b01x01p18n01i00404arch;
