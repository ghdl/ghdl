
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
-- $Id: tc498.vhd,v 1.2 2001-10-26 16:29:55 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b02x00p02n01i00498ent IS
END c03s02b02x00p02n01i00498ent;

ARCHITECTURE c03s02b02x00p02n01i00498arch OF c03s02b02x00p02n01i00498ent IS
  type Month_name is (jan, dec);
  type Date is
    record
      Day      : integer range 1 to 31;
      Month   : Month_name;
      Year   : integer range 0 to 4000;
    end record;
BEGIN
  TESTING: PROCESS
    variable k : Date;
  BEGIN
    k.Day   :=   16;
    k.Month   :=   jan;
    k.Year   :=   1993;   
    assert NOT(k.Day=16 and k.Month=jan and k.Year =1993)
      report "***PASSED TEST: c03s02b02x00p02n01i00498"
      severity NOTE;
    assert (k.Day=16 and k.Month=jan and k.Year =1993)
      report "***FAILED TEST: c03s02b02x00p02n01i00498 - The record type definition consists of the reserved word record, one or more element declarations, and the reserved words end record."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b02x00p02n01i00498arch;
