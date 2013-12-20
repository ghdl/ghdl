
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
-- $Id: tc2948.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s02b00x00p08n02i02948ent IS
END c02s02b00x00p08n02i02948ent;

ARCHITECTURE c02s02b00x00p08n02i02948arch OF c02s02b00x00p08n02i02948ent IS
  procedure PX (signal I1: in Bit; signal I2 : out Bit; signal I3 : inout Integer) is
  begin
    I2   <= '1';
    I3   <=  6;
  end PX;    -- No_failure_here

  signal S1 : Bit    := '1';
  signal S2 : Integer    := 5;
  signal S3 : Bit;
BEGIN
  TESTING: PROCESS
  BEGIN
    PX(S1,S3,S2);
    wait for 5 ns;
    assert NOT(S3='1' and S2=6)
      report "***PASSED TEST: c02s02b00x00p08n02i02948"
      severity NOTE;
    assert (S3='1' and S2=6)
      report "***FAILED TEST: c02s02b00x00p08n02i02948 - Designator at the end of subprogram body is not the same as the designator of the subprogram."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s02b00x00p08n02i02948arch;
