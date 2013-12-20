
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
-- $Id: tc748.vhd,v 1.2 2001-10-26 16:29:59 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c01s01b01x01p05n02i00748pkg is
  type boolean_vector       is array (natural range <>) of boolean;
  type severity_level_vector    is array (natural range <>) of severity_level;
  type integer_vector       is array (natural range <>) of integer;
  type real_vector       is array (natural range <>) of real;
  type time_vector       is array (natural range <>) of time;
  type natural_vector       is array (natural range <>) of natural;
  type positive_vector    is array (natural range <>) of positive;
  type record_std_package is record
                               a:boolean;
                               b:bit;
                               c:character;
                               d:severity_level;
                               e:integer;
                               f:real;
                               g:time;
                               h:natural;
                               i:positive;
                               j:string(1 to 7);
                               k:bit_vector(0 to 3);
                             end record;

  type array_rec_std is array (integer range <>) of record_std_package;

  procedure P1(inp : boolean_vector; a:integer; b:integer; c:integer; d:integer; e:integer;ot:out boolean_vector) ;
  procedure P2(inp : bit_vector; a:integer; b:integer; c:integer; d:integer; e:integer;ot:out bit_vector) ;
  procedure P3(inp : string; a:integer; b:integer; c:integer; d:integer; e:integer; ot:out string);
  procedure P4(inp : severity_level_vector; a:integer; b:integer; c:integer; d:integer; e:integer;ot:out severity_level_vector);
  procedure P5(inp : integer_vector; a:integer; b:integer; c:integer; d:integer; e:integer; ot:out integer_vector) ;
  procedure P6(inp : real_vector; a:integer; b:integer; c:integer; d:integer; e:integer; ot:out real_vector) ;
  procedure P7(inp : time_vector; a:integer; b:integer; c:integer; d:integer; e:integer; ot:out time_vector) ;
  procedure P8(inp : natural_vector; a:integer; b:integer; c:integer; d:integer; e:integer;ot:out natural_vector) ;
  procedure P9(inp : positive_vector; a:integer; b:integer; c:integer; d:integer; e:integer;ot:out positive_vector) ;
  procedure P10(inp : array_rec_std; a:integer; b:integer; c:integer; d:integer; e:integer;ot:out array_rec_std) ;
end c01s01b01x01p05n02i00748pkg;

package body c01s01b01x01p05n02i00748pkg is
  procedure P1(inp : boolean_vector; a:integer; b:integer; c:integer; d:integer; e:integer;ot:out boolean_vector) is
  begin 
    for i in 0 to 15 loop
      assert(inp(i) = true) report"wrong initialization of S1" severity error;
    end loop;
    ot := inp;
  end P1;   
  procedure P2(inp : bit_vector; a:integer; b:integer; c:integer; d:integer; e:integer;ot:out bit_vector) is
  begin
    for i in 0 to 3 loop
      assert(inp(i) = '0') report"wrong initialization of S2" severity error;
    end loop;
    ot := inp;
  end P2;
  procedure P3(inp : string; a:integer; b:integer; c:integer; d:integer; e:integer; ot:out string) is
  begin
    for i in 1 to 7 loop
      assert(inp(i) = 's') report"wrong initialization of S3" severity error;
    end loop;
    ot := inp;
  end P3;
  procedure P4(inp : severity_level_vector; a:integer; b:integer; c:integer; d:integer; e:integer;ot:out severity_level_vector) is
  begin
    for i in 0 to 15 loop
      assert(inp(i) = note) report"wrong initialization of S4" severity error;
    end loop;
    ot := inp;
  end P4;
  procedure P5(inp : integer_vector;  a:integer; b:integer; c:integer; d:integer; e:integer;ot:out integer_vector) is
  begin
    for i in 0 to 15 loop
      assert(inp(i) = 3) report"wrong initialization of S5" severity error;
    end loop;
    ot := inp;
  end P5;
  procedure P6(inp : real_vector;  a:integer; b:integer; c:integer; d:integer; e:integer;ot:out real_vector) is
  begin
    for i in 0 to 15 loop
      assert(inp(i) = 3.0) report"wrong initialization of S6" severity error;
    end loop;
    ot := inp;
  end P6;
  procedure P7(inp : time_vector;  a:integer; b:integer; c:integer; d:integer; e:integer;ot:out time_vector) is
  begin
    for i in 0 to 15 loop
      assert(inp(i) = 3 ns) report"wrong initialization of S7" severity error;
    end loop;
    ot := inp;
  end P7;
  procedure P8(inp : natural_vector; a:integer; b:integer; c:integer; d:integer; e:integer;ot:out natural_vector) is
  begin
    for i in 0 to 15 loop
      assert(inp(i) = 1) report"wrong initialization of S8" severity error;
    end loop;
    ot :=  inp;
  end P8;
  procedure P9(inp : positive_vector; a:integer; b:integer; c:integer; d:integer; e:integer;ot:out positive_vector) is
  begin
    for i in 0 to 15 loop
      assert(inp(i) = 1) report"wrong initialization of S9" severity error;
    end loop;
    ot := inp;
  end P9;
  procedure P10(inp : array_rec_std; a:integer; b:integer; c:integer; d:integer; e:integer;ot:out array_rec_std) is
  begin
    for i in 0 to 7 loop
      assert(inp(i) = (true,'1','s',note,3,3.0,3 ns, 1,1,"sssssss","0000")) report"wrong initialization of S10" severity error;
    end loop;
    ot :=  inp;
  end P10;
end c01s01b01x01p05n02i00748pkg;

use work.c01s01b01x01p05n02i00748pkg.all;
ENTITY c01s01b01x01p05n02i00748ent IS
  generic(
    zero : integer := 0;
    one  : integer := 1;
    two  : integer := 2;
    three: integer := 3;
    four : integer := 4;
    five : integer := 5;
    six  : integer := 6;
    seven: integer := 7;
    eight: integer := 8;
    nine : integer := 9;
    fifteen:integer:= 15;
    C1 : boolean    := true;
    C2 : bit       := '1';
    C3 : character    := 's';
    C4 : severity_level:= note;
    C5 : integer    := 3;
    C6 : real       := 3.0;
    C7 : time       := 3 ns;
    C8 : natural    := 1;
    C9 : positive    := 1;
    C10 : string    := "sssssss";
    C11 : bit_vector    := B"0000";
    C48 : record_std_package := (true,'1','s',note,3,3.0,3 ns,1,1,"sssssss","0000")
    );
  port(
    S1 : boolean_vector(zero to fifteen)    := (others => C1);
    S2 : severity_level_vector(zero to fifteen)    := (others => C4);
    S3 : integer_vector(zero to fifteen)   := (others => C5);
    S4 : real_vector(zero to fifteen)   := (others => C6);
    S5 : time_vector (zero to fifteen)   := (others => C7);
    S6 : natural_vector(zero to fifteen)   := (others => C8);
    S7 : positive_vector(zero to fifteen)   := (others => C9);
    S8 : string(one to seven)      := C10;
    S9 : bit_vector(zero to three)      := C11;
    S48: array_rec_std(zero to seven)   := (others => C48)
    );
END c01s01b01x01p05n02i00748ent;

ARCHITECTURE c01s01b01x01p05n02i00748arch OF c01s01b01x01p05n02i00748ent IS
BEGIN
  TESTING: PROCESS

    variable var1 : boolean_vector(zero to fifteen);
    variable var4 : severity_level_vector(zero to fifteen);
    variable var5 : integer_vector(zero to fifteen);
    variable var6 : real_vector(zero to fifteen);
    variable var7 : time_vector(zero to fifteen);
    variable var8 : natural_vector(zero to fifteen);
    variable var9 : positive_vector(zero to fifteen);
    variable var2 : bit_vector(zero to three);
    variable var3 : string(one to seven);
    variable var48: array_rec_std(zero to seven);

  BEGIN
    P1(S1,zero,one,three,seven,fifteen,var1);
    P2(S9,zero,one,three,seven,fifteen,var2);
    P3(S8,zero,one,three,seven,fifteen,var3);
    P4(S2,zero,one,three,seven,fifteen,var4);
    P5(S3,zero,one,three,seven,fifteen,var5);
    P6(S4,zero,one,three,seven,fifteen,var6);
    P7(S5,zero,one,three,seven,fifteen,var7);
    P8(S6,zero,one,three,seven,fifteen,var8);
    P9(S7,zero,one,three,seven,fifteen,var9);
    P10(S48,zero,one,three,seven,fifteen,var48);
    wait for 1 ns;

    assert(var1(0) = true)    report"wrong assignment of S1" severity error;
    assert(var2(0) = '0')    report"wrong assignment of S2" severity error;
    assert(var3(1) = 's')    report"wrong assignment of S3" severity error;
    assert(var4(0) = note)    report"wrong assignment of S4" severity error;
    assert(var5(0) = 3)    report"wrong assignment of S5" severity error;
    assert(var6(0) = 3.0)    report"wrong assignment of S6" severity error;
    assert(var7(0) = 3 ns)    report"wrong assignment of S7" severity error;
    assert(var8(0) = 1)    report"wrong assignment of S8" severity error;
    assert(var9(0) = 1)    report"wrong assignment of S9" severity error;
    assert(var48(0) = (true,'1','s',note,3,3.0,3 ns, 1,1,"sssssss","0000"))    report"wrong assignment of S10" severity error;

    assert NOT(    (var1(0) = true)   and
                   (var2(0) = '0')    and
                   (var3(1) = 's')    and
                   (var4(0) = note)    and
                   (var5(0) = 3)       and
                   (var6(0) = 3.0)    and
                   (var7(0) = 3 ns)    and
                   (var8(0) = 1)       and
                   (var9(0) = 1)       and
                   (var48(0) = (true,'1','s',note,3,3.0,3 ns, 1,1,"sssssss","0000")) )
      report "***PASSED TEST: c01s01b01x01p05n02i00748"
      severity NOTE;
    assert (    (var1(0) = true)   and
                (var2(0) = '0')    and
                (var3(1) = 's')    and
                (var4(0) = note)    and
                (var5(0) = 3)       and
                (var6(0) = 3.0)    and
                (var7(0) = 3 ns)    and
                (var8(0) = 1)       and
                (var9(0) = 1)       and
                (var48(0) = (true,'1','s',note,3,3.0,3 ns, 1,1,"sssssss","0000")) )
      report "***FAILED TEST: c01s01b01x01p05n02i00748 - Generic can be used to specify the size of ports."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c01s01b01x01p05n02i00748arch;
