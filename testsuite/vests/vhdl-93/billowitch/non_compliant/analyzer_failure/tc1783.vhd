
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
-- $Id: tc1783.vhd,v 1.2 2001-10-26 16:30:12 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s06b00x00p04n06i01783ent IS
  port (X: in BIT; Y: in BIT_VECTOR; Z: out BIT);
END c09s06b00x00p04n06i01783ent;

ARCHITECTURE c09s06b00x00p04n06i01783arch OF c09s06b00x00p04n06i01783ent IS
  component input2
    generic (g1: integer );
    port (signal input_1 : in  bit;
          signal input_2 : in  bit_vector;
          signal output  : out bit);
  end component;
BEGIN
  G1: input2
    port map (X,Y,Z);

  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c09s06b00x00p04n06i01783 - Each local generic must be associated at least once."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c09s06b00x00p04n06i01783arch;
