
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
-- $Id: tc1778.vhd,v 1.2 2001-10-26 16:30:12 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s06b00x00p02n01i01778ent IS
END c09s06b00x00p02n01i01778ent;

ARCHITECTURE c09s06b00x00p02n01i01778arch OF c09s06b00x00p02n01i01778ent IS
  component C
    generic ( c2 : in integer := 122903 );
  end component;
BEGIN
  C generic map (p);  -- Failure_here
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c09s06b00x00p02n01i01778 - A colon(:) is expected after the instantiation label." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c09s06b00x00p02n01i01778arch;
