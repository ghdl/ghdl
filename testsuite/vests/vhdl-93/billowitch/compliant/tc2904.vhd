
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
-- $Id: tc2904.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s01b01x01p02n03i02904ent IS
END c02s01b01x01p02n03i02904ent;

ARCHITECTURE c02s01b01x01p02n03i02904arch OF c02s01b01x01p02n03i02904ent IS
  procedure PX (I1 : in Bit; I2 : out Bit; I3 : inout Integer);
  procedure PX (I1 : in Bit; I2 : out Bit; I3 : inout Integer) is
  begin
    I2 := I1;
    I3 := 10;
  end PX;
BEGIN
  TESTING: PROCESS
    variable V1 : Bit;
    variable V2 : Integer;
  BEGIN
    PX('1',V1,V2);
    wait for 5 ns;
    assert NOT( V1='1' and V2=10 )
      report "***PASSED TEST: c02s01b01x01p02n03i02904"
      severity NOTE;
    assert ( V1='1' and V2=10 )
      report "***FAILED TEST: c02s01b01x01p02n03i02904 - Mode out for procedures are not copied properly"
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s01b01x01p02n03i02904arch;
