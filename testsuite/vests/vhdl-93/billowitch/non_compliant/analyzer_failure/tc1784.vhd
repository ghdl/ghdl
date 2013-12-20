
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
-- $Id: tc1784.vhd,v 1.2 2001-10-26 16:30:12 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s06b00x00p04n06i01784ent IS
  generic (X: in BIT_VECTOR(0 to 2); Z: in BIT);
END c09s06b00x00p04n06i01784ent;

ARCHITECTURE c09s06b00x00p04n06i01784arch OF c09s06b00x00p04n06i01784ent IS
  component input2
    generic (input_1 : in  bit;
             input_2 : in  bit;
             output  : in  bit);
  end component;

  constant A1 : bit := '1';
BEGIN
  G1: input2
    generic map (input_1 => X(0), input_1 => X(1), output => A1);
  --    Failure_here

  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c09s06b00x00p04n06i01784 - Each local generic must be associated exactly once."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c09s06b00x00p04n06i01784arch;
