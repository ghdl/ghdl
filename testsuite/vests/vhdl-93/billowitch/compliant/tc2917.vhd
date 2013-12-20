
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
-- $Id: tc2917.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s01b01x02p05n01i02917ent IS
END c02s01b01x02p05n01i02917ent;

ARCHITECTURE c02s01b01x02p05n01i02917arch OF c02s01b01x02p05n01i02917ent IS
  type    t1 is (one,two,three);
  signal    s1 : t1;
  signal    s2 : integer;

  procedure proc1(signal ss1:inout t1; signal ss2:out integer) is
  begin
    ss1<=two after 5 ns;
    ss2<=2 after 5 ns;
  end proc1;
BEGIN
  TESTING: PROCESS
  BEGIN
    s1<=three;
    s2<=3;
    wait for 5 ns;
    assert (s1=three)
      report "Error in initial conditions detected"
      severity failure;
    assert (s2=3)
      report "Error in initial conditions detected"
      severity failure;
    proc1(s1,s2);
    wait for 10 ns;
    assert (s1=two)
      report "Error detected in signal assignment for S1"
      severity failure;
    assert (s2=2)
      report "Error detected in signal assignment for S2"
      severity failure;
    assert NOT( s1=two and s2=2 )
      report "***PASSED TEST: c02s01b01x02p05n01i02917"
      severity NOTE;
    assert ( s1=two and s2=2 )
      report "***FAILED TEST: c02s01b01x02p05n01i02917 - Error detected in signal assignemnts."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s01b01x02p05n01i02917arch;
