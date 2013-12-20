
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
-- $Id: tc753.vhd,v 1.2 2001-10-26 16:29:59 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c01s01b01x01p05n02i00753pkg is
  subtype hi_to_low_range    is integer range 0 to 7;
  type boolean_vector       is array (natural range <>) of boolean;
  type severity_level_vector    is array (natural range <>) of severity_level;
  type integer_vector       is array (natural range <>) of integer;
  type real_vector       is array (natural range <>) of real;
  type time_vector       is array (natural range <>) of time;
  type natural_vector       is array (natural range <>) of natural;
  type positive_vector    is array (natural range <>) of positive;
  subtype boolean_vector_st    is boolean_vector(0 to 15);
  subtype severity_level_vector_st    is  severity_level_vector(0 to 15);
  subtype integer_vector_st    is integer_vector(0 to 15);
  subtype real_vector_st    is real_vector(0 to 15);
  subtype time_vector_st    is time_vector(0 to 15);
  subtype natural_vector_st    is natural_vector(0 to 15);
  subtype positive_vector_st    is positive_vector(0 to 15);
  type boolean_cons_vector    is array (15 downto 0) of boolean;
  type severity_level_cons_vector    is array (15 downto 0) of severity_level;
  type integer_cons_vector    is array (15 downto 0) of integer;
  type real_cons_vector    is array (15 downto 0) of real;
  type time_cons_vector    is array (15 downto 0) of time;
  type natural_cons_vector    is array (15 downto 0) of natural;
  type positive_cons_vector    is array (15 downto 0) of positive;
  type boolean_cons_vectorofvector    is array (0 to 15) of boolean_cons_vector;
  type severity_level_cons_vectorofvector    is array (0 to 15) of severity_level_cons_vector;
  type integer_cons_vectorofvector    is array (0 to 15) of integer_cons_vector ;
  type real_cons_vectorofvector    is array (0 to 15) of real_cons_vector;
  type time_cons_vectorofvector    is array (0 to 15) of time_cons_vector;
  type natural_cons_vectorofvector    is array (0 to 15) of natural_cons_vector;
  type positive_cons_vectorofvector    is array (0 to 15) of positive_cons_vector;
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
                               j:string(1 to 7);
                               k:bit_vector(0 to 3);
                             end record;
  type record_array_st is record
                            a:boolean_vector_st;
                            b:severity_level_vector_st;
                            c:integer_vector_st;
                            d:real_vector_st;
                            e:time_vector_st;
                            f:natural_vector_st;
                            g:positive_vector_st;
                          end record;
  type record_cons_array is record
                              a:boolean_cons_vector;
                              b:severity_level_cons_vector;
                              c:integer_cons_vector;
                              d:real_cons_vector;
                              e:time_cons_vector;
                              f:natural_cons_vector;
                              g:positive_cons_vector;
                            end record;
  type record_cons_arrayofarray is record
                                     a:boolean_cons_vectorofvector;
                                     b:severity_level_cons_vectorofvector;
                                     c:integer_cons_vectorofvector;
                                     d:real_cons_vectorofvector;
                                     e:time_cons_vectorofvector;
                                     f:natural_cons_vectorofvector;
                                     g:positive_cons_vectorofvector;
                                   end record;
  type record_array_new is record
                             a:boolean_vector(0 to 15);
                             b:severity_level_vector(0 to 15);
                             c:integer_vector(0 to 15);
                             d:real_vector(0 to 15);
                             e:time_vector(0 to 15);
                             f:natural_vector(0 to 15);
                             g:positive_vector(0 to 15);
                           end record;
  type record_of_records is record
                              a: record_std_package;
                              c: record_cons_array;
                              g: record_cons_arrayofarray;
                              i: record_array_st;
                              j: record_array_new;
                            end record;
  subtype boolean_vector_range is boolean_vector(hi_to_low_range);
  subtype severity_level_vector_range is severity_level_vector(hi_to_low_range);
  subtype integer_vector_range is integer_vector(hi_to_low_range);
  subtype real_vector_range is real_vector(hi_to_low_range);
  subtype time_vector_range is time_vector(hi_to_low_range);
  subtype natural_vector_range is natural_vector(hi_to_low_range);
  subtype positive_vector_range is positive_vector(hi_to_low_range);
  type array_rec_std is array (integer range <>) of record_std_package;
  type array_rec_cons is array (integer range <>) of record_cons_array;
  type array_rec_rec is array (integer range <>) of record_of_records;
  subtype array_rec_std_st is array_rec_std (hi_to_low_range);
  subtype array_rec_cons_st is array_rec_cons (hi_to_low_range);
  subtype array_rec_rec_st is array_rec_rec (hi_to_low_range);
  
  type record_of_arr_of_record is record
                                    a: array_rec_std(0 to 7);
                                    b: array_rec_cons(0 to 7);
                                    c: array_rec_rec(0 to 7);
                                  end record;

  type current is range -2147483647 to +2147483647
    units
      nA;
      uA = 1000 nA;
      mA = 1000 uA;
      A  = 1000 mA;
    end units;
  
  type current_vector is array (natural range <>) of current;
  
  subtype current_vector_range is current_vector(hi_to_low_range);
  
  type resistance is range -2147483647 to +2147483647
    units
      uOhm;
      mOhm = 1000 uOhm;
      Ohm = 1000 mOhm;
      KOhm  = 1000 Ohm;
    end units;
  
  type resistance_vector is array (natural range <>) of resistance;
  
  subtype resistance_vector_range is resistance_vector(hi_to_low_range);
  
  type byte is array(0 to 7) of bit;
  
  subtype word is bit_vector(0 to 15);                                 --constrained array
  
  constant size :integer := 7;
  
  type primary_memory is array(0 to size) of word;                     --array of an array
  
  type primary_memory_module is                                       --record with field
    record                                                               --as an array
      enable:bit;
      memory_number:primary_memory;
    end record;
  type whole_memory is array(0 to size) of primary_memory_module;     --array of a complex record
  subtype delay is integer range 1 to 10;
  
end c01s01b01x01p05n02i00753pkg;


use work.c01s01b01x01p05n02i00753pkg.all;
ENTITY c01s01b01x01p05n02i00753ent IS
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
    C1 : boolean := true;
    C2 : bit := '1';
    C3 : character := 's';
    C4 : severity_level := note;
    C5 : integer := 3;
    C6 : real := 3.0;
    C7 : time := 3 ns;
    C8 : natural := 1;
    C9 : positive := 1;
    C10 : string := "shishir";
    C11 : bit_vector := B"0011"
    );
  port(
    S1 : boolean_vector(zero to fifteen) := (zero to fifteen => C1);
    S2 : severity_level_vector(zero to fifteen):= (zero to fifteen => C4);
    S3 : integer_vector(zero to fifteen):= (zero to fifteen => C5);
    S4 : real_vector(zero to fifteen):= (zero to fifteen => C6);
    S5 : time_vector (zero to fifteen):= (zero to fifteen => C7);
    S6 : natural_vector(zero to fifteen):= (zero to fifteen => C8);
    S7 : positive_vector(zero to fifteen):= (zero to fifteen => C9);
    S8 : boolean_cons_vector:= (zero to fifteen => C1);
    S9 : severity_level_cons_vector := (zero to fifteen => C4);
    S10 : integer_cons_vector:= (zero to fifteen => C5);
    S11 : real_cons_vector:= (zero to fifteen => C6);
    S12 : time_cons_vector := (zero to fifteen => C7);
    S13 : natural_cons_vector := (zero to fifteen => C8);
    S14 : positive_cons_vector := (zero to fifteen => C9);
    S15 : boolean_cons_vectorofvector:= (zero to fifteen =>(others=> C1));
    S16 : severity_level_cons_vectorofvector := (zero to fifteen =>(others=> C4));
    S17 : integer_cons_vectorofvector := (zero to fifteen =>(others=> C5));
    S18 : real_cons_vectorofvector := (zero to fifteen =>(others=> C6));
    S19 : time_cons_vectorofvector := (zero to fifteen =>(others=> C7));
    S20 : natural_cons_vectorofvector := (zero to fifteen =>(others=> C8));
    S21 : positive_cons_vectorofvector := (zero to fifteen =>(others=> C9));
    S22 : record_std_package := (C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11);
    S25 : boolean_vector_st := (zero to fifteen => C1);
    S26 : severity_level_vector_st:= (zero to fifteen => C4);
    S27 : integer_vector_st:= (zero to fifteen => C5);
    S28 : real_vector_st:= (zero to fifteen => C6);
    S29 : time_vector_st:= (zero to fifteen => C7);
    S30 : natural_vector_st:= (zero to fifteen => C8);
    S31 : positive_vector_st:= (zero to fifteen => C9)
    );
END c01s01b01x01p05n02i00753ent;

ARCHITECTURE c01s01b01x01p05n02i00753arch OF c01s01b01x01p05n02i00753ent IS

BEGIN
  assert (S1(0) = C1) report " error in initializing S1" severity error;
  assert (S2(0) = C4) report " error in initializing S2" severity error;
  assert (S3(0) = C5) report " error in initializing S3" severity error;
  assert (S4(0) = C6) report " error in initializing S4" severity error;
  assert (S5(0) = C7) report " error in initializing S5" severity error;
  assert (S6(0) = C8) report " error in initializing S6" severity error;
  assert (S7(0) = C9) report " error in initializing S7" severity error;
  assert (S8(0) = C1) report " error in initializing S8" severity error;
  assert (S9(0) = C4) report " error in initializing S9" severity error;
  assert (S10(0) = C5) report " error in initializing S10" severity error;
  assert (S11(0) = C6) report " error in initializing S11" severity error;
  assert (S12(0) = C7) report " error in initializing S12" severity error;
  assert (S13(0) = C8) report " error in initializing S13" severity error;
  assert (S14(0) = C9) report " error in initializing S14" severity error;
  assert (S15(0)(0) = C1) report " error in initializing S15" severity error;
  assert (S16(0)(0) = C4) report " error in initializing S16" severity error;
  assert (S17(0)(0) = C5) report " error in initializing S17" severity error;
  assert (S18(0)(0) = C6) report " error in initializing S18" severity error;
  assert (S19(0)(0) = C7) report " error in initializing S19" severity error;
  assert (S20(0)(0) = C8) report " error in initializing S20" severity error;
  assert (S21(0)(0) = C9) report " error in initializing S21" severity error;
  assert (S22.a = C1) report " error in initializing S21" severity error;
  assert (S22.b = C2) report " error in initializing S21" severity error;
  assert (S22.c = C3) report " error in initializing S21" severity error;
  assert (S22.d = C4) report " error in initializing S21" severity error;
  assert (S22.e = C5) report " error in initializing S21" severity error;
  assert (S22.f = C6) report " error in initializing S21" severity error;
  assert (S22.g = C7) report " error in initializing S21" severity error;
  assert (S22.h = C8) report " error in initializing S21" severity error;
  assert (S22.i = C9) report " error in initializing S21" severity error;
  assert (S22.j = C10) report " error in initializing S21" severity error;
  assert (S22.k = C11) report " error in initializing S21" severity error;
  assert (S25(0) = C1) report " error in initializing S25" severity error;
  assert (S26(0) = C4) report " error in initializing S26" severity error;
  assert (S27(0) = C5) report " error in initializing S27" severity error;
  assert (S28(0) = C6) report " error in initializing S28" severity error;
  assert (S29(0) = C7) report " error in initializing S29" severity error;
  assert (S30(0) = C8) report " error in initializing S30" severity error;
  assert (S31(0) = C9) report " error in initializing S31" severity error;

  TESTING: PROCESS
  BEGIN

    assert NOT(    (S1(0) = C1)    and 
                   (S2(0) = C4)    and 
                   (S3(0) = C5)    and 
                   (S4(0) = C6)    and 
                   (S5(0) = C7)    and 
                   (S6(0) = C8)    and 
                   (S7(0) = C9)    and 
                   (S8(0) = C1)    and 
                   (S9(0) = C4)    and 
                   (S10(0) = C5)    and 
                   (S11(0) = C6)    and 
                   (S12(0) = C7)    and 
                   (S13(0) = C8)    and 
                   (S14(0) = C9)    and 
                   (S15(0)(0) = C1) and 
                   (S16(0)(0) = C4) and 
                   (S17(0)(0) = C5) and 
                   (S18(0)(0) = C6) and 
                   (S19(0)(0) = C7) and 
                   (S20(0)(0) = C8) and 
                   (S21(0)(0) = C9) and 
                   (S22.a = C1)    and 
                   (S22.b = C2)    and 
                   (S22.c = C3)    and 
                   (S22.d = C4)    and 
                   (S22.e = C5)    and 
                   (S22.f = C6)    and 
                   (S22.g = C7)    and 
                   (S22.h = C8)    and 
                   (S22.i = C9)    and 
                   (S22.j = C10)    and 
                   (S22.k = C11)    and 
                   (S25(0) = C1)    and 
                   (S26(0) = C4)    and 
                   (S27(0) = C5)    and 
                   (S28(0) = C6)    and 
                   (S29(0) = C7)    and 
                   (S30(0) = C8)    and 
                   (S31(0) = C9)    ) 
      report "***PASSED TEST: c01s01b01x01p05n02i00753"
      severity NOTE;
    assert (    (S1(0) = C1)    and 
                (S2(0) = C4)    and 
                (S3(0) = C5)    and 
                (S4(0) = C6)    and 
                (S5(0) = C7)    and 
                (S6(0) = C8)    and 
                (S7(0) = C9)    and 
                (S8(0) = C1)    and 
                (S9(0) = C4)    and 
                (S10(0) = C5)    and 
                (S11(0) = C6)    and 
                (S12(0) = C7)    and 
                (S13(0) = C8)    and 
                (S14(0) = C9)    and 
                (S15(0)(0) = C1) and 
                (S16(0)(0) = C4) and 
                (S17(0)(0) = C5) and 
                (S18(0)(0) = C6) and 
                (S19(0)(0) = C7) and 
                (S20(0)(0) = C8) and 
                (S21(0)(0) = C9) and 
                (S22.a = C1)    and 
                (S22.b = C2)    and 
                (S22.c = C3)    and 
                (S22.d = C4)    and 
                (S22.e = C5)    and 
                (S22.f = C6)    and 
                (S22.g = C7)    and 
                (S22.h = C8)    and 
                (S22.i = C9)    and 
                (S22.j = C10)    and 
                (S22.k = C11)    and 
                (S25(0) = C1)    and 
                (S26(0) = C4)    and 
                (S27(0) = C5)    and 
                (S28(0) = C6)    and 
                (S29(0) = C7)    and 
                (S30(0) = C8)    and 
                (S31(0) = C9)    ) 
      report "***FAILED TEST: c01s01b01x01p05n02i00753 - Generic can be used to specify the size of ports."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c01s01b01x01p05n02i00753arch;
