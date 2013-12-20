
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
-- $Id: tc3139.vhd,v 1.2 2001-10-26 16:29:52 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c05s02b02x00p02n01i03139ent IS
END c05s02b02x00p02n01i03139ent;

ARCHITECTURE c05s02b02x00p02n01i03139arch OF c05s02b02x00p02n01i03139ent IS

begin
  l : block
    component c05s02b02x00p02n01i03139ent_a
    end component;
    for comp1 : c05s02b02x00p02n01i03139ent_a use OPEN;
  BEGIN

    comp1 : c05s02b02x00p02n01i03139ent_a;

    TESTING: PROCESS
    BEGIN
      assert FALSE
        report "***PASSED TEST: c05s02b02x00p02n01i03139"
        severity NOTE;
      wait;
    END PROCESS TESTING;
  end block;

END c05s02b02x00p02n01i03139arch;
