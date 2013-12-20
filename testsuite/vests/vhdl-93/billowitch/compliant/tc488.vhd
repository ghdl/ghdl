
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
-- $Id: tc488.vhd,v 1.2 2001-10-26 16:29:55 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b02x00p01n01i00488ent IS
END c03s02b02x00p01n01i00488ent;

ARCHITECTURE c03s02b02x00p01n01i00488arch OF c03s02b02x00p01n01i00488ent IS
  type T0 is record
               el1 : real;
               el2 : real;
               el3 : real;
               el4 : real;
               el5 : real;
               el6 : real;
               el7 : real;
               el8 : real;
               el9 : real;
               el10 : real;
             end record;
  type T1 is record
               el1 : real;
               el2 : real;
               el3 : real;
               el4 : real;
               el5 : real;
               el6 : real;
             end record;
  type T2 is record
               el5 : real;
               el6 : real;
               el7 : real;
             end record;
  function FUNC1(recd1: T0) return T1 is
    variable recd2:T1;
  begin -- procedure FUNC1
    recd2.el1 := recd1.el6;
    recd2.el2 := recd1.el1;
    recd2.el3 := recd1.el3;
    recd2.el4 := recd1.el2;
    recd2.el5 := recd1.el6;
    recd2.el6 := recd1.el10;
    return recd2;
  end FUNC1;

  function FUNC3(recd1: T0) return T2 is
    variable recd2:T2;
  begin -- procedure FUNC3
    recd2.el5 := recd1.el5;
    recd2.el6 := recd1.el6;
    recd2.el7 := recd1.el1;
    return recd2;
  end FUNC3;
  
  function FUNC4(recd1: T2) return T2 is
    variable recd2:T2;
  begin -- procedure FUNC4
    recd2.el5 := recd1.el7;
    recd2.el6 := recd1.el5;
    recd2.el7 := recd1.el5;
    return recd2;
  end FUNC4;

BEGIN
  TESTING: PROCESS
    variable rec1: T0;
    variable v1,v2:T1;
    variable v3,v4:T2;
  BEGIN
    rec1 := (1.1,2.2,3.3,4.4,5.5,6.6,7.7,8.8,9.9,10.01);
    wait for 1 ns;
    v1 := FUNC1(rec1);
    v3 := FUNC3(rec1);
    v4 := FUNC4(v3);
    wait for 1 ns;
    assert NOT( (v1 = (6.6,1.1,3.3,2.2,6.6,10.01))    AND
                (v3 = (5.5,6.6,1.1))       AND
                (v4 = (1.1,5.5,5.5)))
      report "***PASSED TEST: c03s02b02x00p01n01i00488"
      severity NOTE;
    assert (    (v1 = (6.6,1.1,3.3,2.2,6.6,10.01))    AND
                (v3 = (5.5,6.6,1.1))       AND
                (v4 = (1.1,5.5,5.5)))
      report "***FAILED TEST: c03s02b02x00p01n01i00488 - Values of a record object consist of the value of its elements." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b02x00p01n01i00488arch;
