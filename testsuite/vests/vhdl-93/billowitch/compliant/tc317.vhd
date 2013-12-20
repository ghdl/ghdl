
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
-- $Id: tc317.vhd,v 1.2 2001-10-26 16:29:52 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b00x00p03n02i00317ent IS
END c03s02b00x00p03n02i00317ent;

ARCHITECTURE c03s02b00x00p03n02i00317arch OF c03s02b00x00p03n02i00317ent IS
  type MVL is ('0','1','X','Z') ;
  type T1 is array (0 to 31) of BIT;
  type T2 is record
               D : Integer range 1 to 30;
               M : Integer range 1 to 12;
               Y : Integer range 0 to 2000;  -- No_failure_here
             end record;
BEGIN
  TESTING: PROCESS
    variable k : MVL := 'X';
  BEGIN
    assert NOT(k='X') 
      report "***PASSED TEST: c03s02b00x00p03n02i00317" 
      severity NOTE;
    assert (k='X') 
      report "***FAILED TEST: c03s02b00x00p03n02i00317 - A composite type may contain elements that are of scalar types and composite types (array and record types)." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b00x00p03n02i00317arch;
