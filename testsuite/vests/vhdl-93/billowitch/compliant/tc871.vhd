
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
-- $Id: tc871.vhd,v 1.2 2001-10-26 16:30:01 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c01s03b01x00p12n01i00871pkg is
  constant    low_number    : integer := 0;
  constant    hi_number    : integer := 3;
  subtype    hi_to_low_range    is integer range low_number to hi_number;
  type    boolean_vector       is array (natural range <>) of boolean;
  type    severity_level_vector    is array (natural range <>) of severity_level;
  type    integer_vector       is array (natural range <>) of integer;
  type    real_vector       is array (natural range <>) of real;
  type    time_vector       is array (natural range <>) of time;
  type    natural_vector       is array (natural range <>) of natural;
  type    positive_vector    is array (natural range <>) of positive;
  type record_std_package is record
                               a: boolean;
                               b: bit;
                               c:character;
                               d:severity_level;
                               e:integer;
                               f:real;
                               g:time;
                               h:natural;
                               i:positive;
                             end record;
  type    array_rec_std       is array (natural range <>) of record_std_package;
  type four_value is ('Z','0','1','X');                                 
--enumerated type
  constant C1 : boolean    := true;
  constant C2 : bit    := '1';
  constant C3 : character := 's';
  constant C4 : severity_level := note;
  constant C5 : integer    := 3;
  constant C6 : real    := 3.0;
  constant C7 : time    := 3 ns;
  constant C8 : natural    := 1;
  constant C9 : positive    := 1;

  signal Sin1 : bit_vector(0 to 5) ;
  signal Sin2 : boolean_vector(0 to 5) ;
  signal Sin4 : severity_level_vector(0 to 5) ;
  signal Sin5 : integer_vector(0 to 5) ;
  signal Sin6 : real_vector(0 to 5) ;
  signal Sin7 : time_vector(0 to 5) ;
  signal Sin8 : natural_vector(0 to 5) ;
  signal Sin9 : positive_vector(0 to 5) ;
  signal Sin10: array_rec_std(0 to 5) ;
end c01s03b01x00p12n01i00871pkg;

use work.c01s03b01x00p12n01i00871pkg.all;
entity test is
  port(
    sigin1  : in  boolean ;
    sigout1 : out boolean ;
    sigin2  : in  bit ;
    sigout2 : out bit ;
    sigin4  : in  severity_level ;
    sigout4 : out severity_level ;
    sigin5  : in  integer ;
    sigout5 : out integer ;
    sigin6  : in  real ;
    sigout6 : out real ;
    sigin7  : in  time ;
    sigout7 : out time ;
    sigin8  : in  natural ;
    sigout8 : out natural ;
    sigin9  : in  positive ;
    sigout9 : out positive ;
    sigin10  : in  record_std_package ;
    sigout10 : out record_std_package
    );
end;

architecture test of test is
begin
  sigout1 <= sigin1;
  sigout2 <= sigin2;
  sigout4 <= sigin4;
  sigout5 <= sigin5;
  sigout6 <= sigin6;
  sigout7 <= sigin7;
  sigout8 <= sigin8;
  sigout9 <= sigin9;
  sigout10 <= sigin10;
end;

configuration testbench of test is
  for test
  end for;
end;

use work.c01s03b01x00p12n01i00871pkg.all;
entity test1 is
  port(
    sigin1  : in  boolean ;
    sigout1 : out boolean ;
    sigin2  : in  bit ;
    sigout2 : out bit ;
    sigin4  : in  severity_level ;
    sigout4 : out severity_level ;
    sigin5  : in  integer ;
    sigout5 : out integer ;
    sigin6  : in  real ;
    sigout6 : out real ;
    sigin7  : in  time ;
    sigout7 : out time ;
    sigin8  : in  natural ;
    sigout8 : out natural ;
    sigin9  : in  positive ;
    sigout9 : out positive ;
    sigin10  : in  record_std_package ;
    sigout10 : out record_std_package
    );
end;

architecture test1 of test1 is
begin
  sigout1 <= false;
  sigout2 <= '0';
  sigout4 <= error;
  sigout5 <= 6;
  sigout6 <= 6.0;
  sigout7 <= 6 ns;
  sigout8 <= 6;
  sigout9 <= 6;
  sigout10 <= (false,'0','h',error,6,6.0,6 ns,6,6);
end;

configuration test1bench of test1 is
  for test1
  end for;
end;

use work.c01s03b01x00p12n01i00871pkg.all;
ENTITY c01s03b01x00p12n01i00871ent IS
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
    fifteen:integer:= 15);
  port(
    dumy : inout bit_vector(zero to three));
END c01s03b01x00p12n01i00871ent;

ARCHITECTURE c01s03b01x00p12n01i00871arch OF c01s03b01x00p12n01i00871ent IS
  component test
    port(
      sigin1  : in  boolean ;
      sigout1 : out boolean ;
      sigin2  : in  bit ;
      sigout2 : out bit ;
      sigin4  : in  severity_level ;
      sigout4 : out severity_level ;
      sigin5  : in  integer ;
      sigout5 : out integer ;
      sigin6  : in  real ;
      sigout6 : out real ;
      sigin7  : in  time ;
      sigout7 : out time ;
      sigin8  : in  natural ;
      sigout8 : out natural ;
      sigin9  : in  positive ;
      sigout9 : out positive ;
      sigin10  : in  record_std_package ;
      sigout10 : out record_std_package
      );
  end component;
begin
  Sin1(zero) <='1';
  Sin2(zero) <= true;
  Sin4(zero) <= note;
  Sin5(zero) <= 3;
  Sin6(zero) <= 3.0;
  Sin7(zero) <= 3 ns;
  Sin8(zero) <= 1;
  Sin9(zero) <= 1;
  Sin10(zero) <= (C1,C2,C3,C4,C5,C6,C7,C8,C9);
  K:block

  BEGIN
    T5 : test
      port map
      (
        Sin2(4),Sin2(5),
        Sin1(4),Sin1(5),
        Sin4(4),Sin4(5),
        Sin5(4),Sin5(5),
        Sin6(4),Sin6(5),
        Sin7(4),Sin7(5),
        Sin8(4),Sin8(5),
        Sin9(4),Sin9(5),
        Sin10(4),Sin10(5)
        );
    G: for i in zero to three generate
      T1:test
        port map
        (
          Sin2(i),Sin2(i+1),
          Sin1(i),Sin1(i+1),
          Sin4(i),Sin4(i+1),
          Sin5(i),Sin5(i+1),
          Sin6(i),Sin6(i+1),
          Sin7(i),Sin7(i+1),
          Sin8(i),Sin8(i+1),
          Sin9(i),Sin9(i+1),
          Sin10(i),Sin10(i+1)
          );
    end generate;
  end block;
  TESTING: PROCESS
    variable dumb : bit_vector(zero to three);
  BEGIN
    wait for 1 ns;
    assert Sin1(0) = Sin1(4) report "assignment of Sin1(0) to Sin1(4) is invalid through entity port" severity failure;
    assert Sin2(0) = Sin2(4) report "assignment of Sin2(0) to Sin2(4) is invalid through entity port" severity failure;
    assert Sin4(0) = Sin4(4) report "assignment of Sin4(0) to Sin4(4) is invalid through entity port" severity failure;
    assert Sin5(0) = Sin5(4) report "assignment of Sin5(0) to Sin5(4) is invalid through entity port" severity failure;
    assert Sin6(0) = Sin6(4) report "assignment of Sin6(0) to Sin6(4) is invalid through entity port" severity failure;
    assert Sin7(0) = Sin7(4) report "assignment of Sin7(0) to Sin7(4) is invalid through entity port" severity failure;
    assert Sin8(0) = Sin8(4) report "assignment of Sin8(0) to Sin8(4) is invalid through entity port" severity failure;
    assert Sin9(0) = Sin9(4) report "assignment of Sin9(0) to Sin9(4) is invalid through entity port" severity failure;
    assert Sin10(0) = Sin10(4) report "assignment of Sin10(0) to Sin10(4) is invalid through entity port" severity failure;
    assert Sin1(5) = '1' report "assignment of Sin1(5) to Sin1(4) is invalid through entity port" severity failure;
    assert Sin2(5) = true report "assignment of Sin2(5) to Sin2(4) is invalid through entity port" severity failure;
    assert Sin4(5) = note report "assignment of Sin4(5) to Sin4(4) is invalid through entity port" severity failure;
    assert Sin5(5) = 3 report "assignment of Sin5(5) to Sin5(4) is invalid through entity port" severity failure;
    assert Sin6(5) = 3.0 report "assignment of Sin6(5) to Sin6(4) is invalid through entity port" severity failure;
    assert Sin7(5) = 3 ns report "assignment of Sin7(5) to Sin7(4) is invalid through entity port" severity failure;
    assert Sin8(5) = 1 report "assignment of Sin8(5) to Sin8(4) is invalid through entity port" severity failure;
    assert Sin9(5) = 1 report "assignment of Sin9(5) to Sin9(4) is invalid through entity port" severity failure;
    assert Sin10(5) = (true,'1','s',note,3,3.0,3 ns,1,1) report "assignment of Sin10(5) to Sin10(4) is invalid through entity port" severity failure;
    
    assert NOT(    Sin1(0) = sin1(4)   and
                   Sin2(0) = Sin2(4)   and
                   Sin4(0) = Sin4(4)   and
                   Sin5(0) = Sin5(4)   and
                   Sin6(0) = Sin6(4)   and
                   Sin7(0) = Sin7(4)   and
                   Sin8(0) = Sin8(4)   and
                   Sin9(0) = Sin9(4)   and
                   Sin10(0)= Sin10(4)   and
                   Sin1(5) = '1'      and
                   Sin2(5) = TRUE      and
                   Sin4(5) = note      and
                   Sin5(5) = 3      and
                   Sin6(5) = 3.0      and
                   Sin7(5) = 3 ns      and
                   Sin8(5) = 1      and
                   Sin9(5) = 1      and
                   Sin10(5)=(True,'1','s',note,3,3.0,3 ns,1,1))
      report "***PASSED TEST: c01s03b01x00p12n01i00871"
      severity NOTE;
    assert (    Sin1(0) = sin1(4)   and
                Sin2(0) = Sin2(4)   and
                Sin4(0) = Sin4(4)   and
                Sin5(0) = Sin5(4)   and
                Sin6(0) = Sin6(4)   and
                Sin7(0) = Sin7(4)   and
                Sin8(0) = Sin8(4)   and
                Sin9(0) = Sin9(4)   and
                Sin10(0)= Sin10(4)   and
                Sin1(5) = '1'      and
                Sin2(5) = TRUE      and
                Sin4(5) = note      and
                Sin5(5) = 3      and
                Sin6(5) = 3.0      and
                Sin7(5) = 3 ns      and
                Sin8(5) = 1      and
                Sin9(5) = 1      and
                Sin10(5)=(True,'1','s',note,3,3.0,3 ns,1,1))
      report "***FAILED TEST: c01s03b01x00p12n01i00871 - If such a block configuration contains an index specification that is a discrete range, then the block configuration applies to those implicit block statements that are generated for the specified range of values of the corresponding generate index."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c01s03b01x00p12n01i00871arch;

configuration c01s03b01x00p12n01i00871cfg of c01s03b01x00p12n01i00871ent is
  for c01s03b01x00p12n01i00871arch
    for K
      for all:test use configuration work.test1bench;
      end for;
      for G(0 to 3)
        for T1 :test
          use configuration work.testbench;
        end for;
      end for;
    end for;
  end for;
end;
