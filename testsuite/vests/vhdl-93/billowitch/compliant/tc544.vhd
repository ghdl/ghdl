
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
-- $Id: tc544.vhd,v 1.2 2001-10-26 16:29:56 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s04b00x00p03n01i00544ent IS
END c03s04b00x00p03n01i00544ent;

ARCHITECTURE c03s04b00x00p03n01i00544arch OF c03s04b00x00p03n01i00544ent IS
  type L is                           -- constrained array decl
    array (1 to 1023, 31 downto 0) of Bit;

  type M is                           -- record type decl
    record
      A: Integer;
      B: L;
    end record;

  type O is                           -- file decl
    file of M;  -- No_failure_here
BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***PASSED TEST: c03s04b00x00p03n01i00544"
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c03s04b00x00p03n01i00544arch;
