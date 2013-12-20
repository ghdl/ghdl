
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
-- $Id: tc491.vhd,v 1.2 2001-10-26 16:29:55 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b02x00p01n01i00491ent IS
END c03s02b02x00p01n01i00491ent;

ARCHITECTURE c03s02b02x00p01n01i00491arch OF c03s02b02x00p01n01i00491ent IS
  type etype is (one,two,three,four,five,six,seven);
  type T1 is record
               t    : time;
               b    : bit;
               i    : integer;
               bo    : boolean;
               r    : real;
               bv    : bit_vector (0 to 3);
               e    : etype;
               c    : character;
             end record;
  function FUNC1 return T1 is
    variable recd2:T1;
  begin
    recd2.bv    := "0001";
    recd2.b    := '1';
    recd2.bo    := true;
    recd2.i    := 777;
    recd2.r    := 333.767;
    recd2.t    := 44 ms;
    recd2.e    := seven;
    recd2.c    := '%';
    return recd2;
  end FUNC1;
BEGIN
  TESTING: PROCESS
    variable var2: T1;
    variable OkayCount: integer := 0;
  BEGIN
    wait for 1 ns;
    var2 := (bv=>"0000",b=>'0',bo=>false,i=>0,r=>0.0,t=>1 ms,e=>one,c=>'a');
    var2 := FUNC1;
    if var2 = (bv=>"0001",b=>'1',bo=>true,i=>777,r=>333.767,t=>44 ms,e=>seven,c=>'%') then
      OkayCount := OkayCount + 1;
    else
      assert false report "bad return on FUNC1" severity note;
    end if;
    var2 := (bv=>"0000",b=>'0',bo=>false,i=>0,r=>0.0,t=>1 ms,e=>one,c=>'a');
    if var2 = (bv=>"0000",b=>'0',bo=>false,i=>0,r=>0.0,t=>1 ms,e=>one,c=>'a') then
      OkayCount := OkayCount + 1;
    end if;
    var2.i    := FUNC1.i;
    var2.b    := FUNC1.b;
    var2.bo := FUNC1.bo;
    var2.bv := FUNC1.bv;
    var2.r    := FUNC1.r;
    var2.t    := FUNC1.t;
    var2.e    := FUNC1.e;
    var2.c    := FUNC1.c;
    
    if var2 = (bv=>"0001",b=>'1',bo=>true,i=>777,r=>333.767,t=>44 ms,e=>seven,c=>'%') then
      OkayCount := OkayCount + 1;
    else
      assert false report "bad return on FUNC1.element" severity note;
    end if;
    wait for 1 ns;
    assert NOT( OkayCount = 3 )
      report "***PASSED TEST: c03s02b02x00p01n01i00491"
      severity NOTE;
    assert ( OkayCount = 3 )
      report "***FAILED TEST: c03s02b02x00p01n01i00491 - Problem assigning record subelements in function."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b02x00p01n01i00491arch;
