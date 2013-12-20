
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
-- $Id: tc1780.vhd,v 1.2 2001-10-26 16:30:12 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s06b00x00p04n01i01780ent IS
END c09s06b00x00p04n01i01780ent;

ARCHITECTURE c09s06b00x00p04n01i01780arch OF c09s06b00x00p04n01i01780ent IS
  signal a, b, p, q: bit;

  component comp1
    port (p1, p2:bit);
  end component;

  for L1 : comp1 use entity work.ch0906_p00401_01_ent;
BEGIN
  L1:comp2                    -- Failure_here: comp2 not declared
    port map (q, p);
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c09s06b00x00p04n01i01780 - The component name in the component instantiation statement must be the name of a component declared in a component declaration."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c09s06b00x00p04n01i01780arch;
