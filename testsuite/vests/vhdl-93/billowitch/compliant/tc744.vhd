
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
-- $Id: tc744.vhd,v 1.2 2001-10-26 16:29:59 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c01s01b01x01p05n02i00744pkg is
  type boolean_vector       is array (natural range <>) of boolean;
  type severity_level_vector    is array (natural range <>) of severity_level;
  type integer_vector       is array (natural range <>) of integer;
  type real_vector       is array (natural range <>) of real;
  type time_vector       is array (natural range <>) of time;
  type natural_vector       is array (natural range <>) of natural;
  type positive_vector    is array (natural range <>) of positive;

  type record_std_package is record
                               a: boolean;
                               b: bit;
                               c: character;
                               d: severity_level;
                               e: integer;
                               f: real;
                               g: time;
                               h: natural;
                               i: positive;
                               j: string(1 to 7);
                               k: bit_vector(0 to 3);
                             end record;

  type array_rec_std is array (integer range <>) of record_std_package;

end c01s01b01x01p05n02i00744pkg;

use work.c01s01b01x01p05n02i00744pkg.all;
ENTITY c01s01b01x01p05n02i00744ent IS
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
    C10 : string    := "shishir";
    C11 : bit_vector    := B"0011"
    );
  port(
    S1 : inout boolean_vector      (zero to fifteen);
    S2 : inout severity_level_vector   (zero to fifteen);
    S3 : inout integer_vector      (zero to fifteen);
    S4 : inout real_vector         (zero to fifteen);
    S5 : inout time_vector       (zero to fifteen);
    S6 : inout natural_vector      (zero to fifteen);
    S7 : inout positive_vector      (zero to fifteen);
    S48: inout array_rec_std      (zero to seven)
    );
END c01s01b01x01p05n02i00744ent;

ARCHITECTURE c01s01b01x01p05n02i00744arch OF c01s01b01x01p05n02i00744ent IS

BEGIN
  TESTING: PROCESS
    variable   k : integer := 0;
  BEGIN
    for i in S1'range loop
      S1(i) <= C1;
    end loop;
    for i in S2'range loop
      S2(i) <= C4;
    end loop;
    for i in S3'range loop
      S3(i) <= C5;
    end loop;
    for i in S4'range loop
      S4(i) <= C6;
    end loop;
    for i in S5'range loop
      S5(i) <= C7;
    end loop;
    for i in S6'range loop
      S6(i) <= C8;
    end loop;
    for i in S7'range loop
      S7(i) <= C9;
    end loop;
    for i in S48'range loop
      S48(i) <= (C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11);
    end loop;
    wait for 10 ns;
    for i in zero to 7 loop
      if (S1(i) /= true) then
        k := 1;
      end if;
      assert S1(i) = true report " boolean_vector(zero to fifteen) error in the left generic value" severity  error;
      if (S2(i) /= note) then
        k := 1;
      end if;
      assert S2(i) = note report " severity_level_vector(zero to fifteen) error in the left generic value" severity  error;
      if (S3(i) /= 3) then
        k := 1;
      end if;
      assert S3(i) = 3 report " integer_vector(zero to fifteen) error in the left generic value" severity  error;
      if (S4(i) /= 3.0) then
        k := 1;
      end if;
      assert S4(i) = 3.0 report " real_vector(zero to fifteen) error in the left generic value" severity  error;
      if (S5(i) /= 3 ns) then
        k := 1;
      end if;
      assert S5(i) = 3 ns report " time_vector (zero to fifteen) error in the left generic value" severity  error;
      if (S6(i) /= 1) then
        k := 1;
      end if;
      assert S6(i) = 1 report " natural_vector(zero to fifteen) error in the left generic value" severity  error;
      if (S7(i) /= 1) then
        k := 1;
      end if;
      assert S7(i) = 1 report " positive_vector(zero to fifteen) error in the left generic value" severity  error;
      if (S48(i) /= (true,'1','s',note,3,3.0,3 ns,1,1,"shishir","0011")) then
        k := 1;
      end if;
      assert S48(i) = (true,'1','s',note,3,3.0,3 ns,1,1,"shishir","0011") report " array_rec_std(zero to seven) error in the left generic value" severity  error;
    end loop;
    assert NOT( k=0 )
      report "***PASSED TEST: c01s01b01x01p05n02i00744"
      severity NOTE;
    assert ( k=0 )
      report "***FAILED TEST: c01s01b01x01p05n02i00744 - Generic can be used to specify the size of ports."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c01s01b01x01p05n02i00744arch;
