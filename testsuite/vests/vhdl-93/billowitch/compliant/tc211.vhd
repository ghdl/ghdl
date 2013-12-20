
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
-- $Id: tc211.vhd,v 1.2 2001-10-26 16:29:45 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s01b00x00p09n01i00211ent IS
END c03s01b00x00p09n01i00211ent;

ARCHITECTURE c03s01b00x00p09n01i00211arch OF c03s01b00x00p09n01i00211ent IS
  type ascending_range is range 0 to 10 ;
  type descending_range is range 10 downto 0 ;
  subtype ascending_subrange is descending_range range 2 to 5 ;
  subtype descending_subrange is ascending_range range 5 downto 2 ;
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT((ascending_range'left = 0) and(descending_range'left = 10) and(ascending_subrange'right = 5) and (descending_subrange'right = 2)) 
      report "***PASSED TEST: c03s01b00x00p09n01i00211" 
      severity NOTE;
    assert ((ascending_range'left = 0) and(descending_range'left = 10) and(ascending_subrange'right = 5) and (descending_subrange'right = 2)) 
      report "***FAILED TEST: c03s01b00x00p09n01i00211 - The type of expression is not the same as the base type." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s01b00x00p09n01i00211arch;
