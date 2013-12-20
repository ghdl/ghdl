
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
-- $Id: tc1779.vhd,v 1.2 2001-10-26 16:30:04 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s06b00x00p02n01i01779ent_a IS
  port (signal input_1 : in  bit;
        signal input_2 : in  bit;
        signal output  : out bit);
END c09s06b00x00p02n01i01779ent_a;

ARCHITECTURE c09s06b00x00p02n01i01779arch_a OF c09s06b00x00p02n01i01779ent IS

BEGIN

END c09s06b00x00p02n01i01779arch_a;

ENTITY c09s06b00x00p02n01i01779ent IS
END c09s06b00x00p02n01i01779ent;

ARCHITECTURE c09s06b00x00p02n01i01779arch OF c09s06b00x00p02n01i01779ent IS

  component input2
    port (signal input_1 : in  bit;
          signal input_2 : in  bit;
          signal output  : out bit);
  end component;
  for C : input2 use entity work.c09s06b00x00p02n01i01779ent_a(c09s06b00x00p02n01i01779arch_a);

  signal A1, A2, A3 : bit;

BEGIN

  C : input2 port map (A1,A2,A3);

  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***PASSED TEST: c09s06b00x00p02n01i01779" 
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c09s06b00x00p02n01i01779arch;
