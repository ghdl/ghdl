
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
-- $Id: tc757.vhd,v 1.2 2001-10-26 16:30:00 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c01s01b01x01p05n02i00757ent IS
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
END c01s01b01x01p05n02i00757ent;

ARCHITECTURE c01s01b01x01p05n02i00757arch OF c01s01b01x01p05n02i00757ent IS
  subtype hi_to_low_range is integer range zero to seven;

  type boolean_vector is array (natural range <>) of boolean;
  type severity_level_vector is array (natural range <>) of severity_level;
  type integer_vector is array (natural range <>) of integer;
  type real_vector is array (natural range <>) of real;
  type time_vector is array (natural range <>) of time;
  type natural_vector is array (natural range <>) of natural;
  type positive_vector is array (natural range <>) of positive;

  subtype boolean_vector_st is boolean_vector(zero to fifteen);
  subtype severity_level_vector_st is  severity_level_vector(zero to fifteen);
  subtype integer_vector_st is  integer_vector(zero to fifteen);
  subtype real_vector_st is  real_vector(zero to fifteen);
  subtype time_vector_st is  time_vector(zero to fifteen);
  subtype natural_vector_st is  natural_vector(zero to fifteen);
  subtype positive_vector_st is  positive_vector(zero to fifteen);

  type boolean_cons_vector is array (fifteen downto zero) of boolean;
  type severity_level_cons_vector is array (fifteen downto zero) of severity_level;
  type integer_cons_vector is array (fifteen downto zero) of integer;
  type real_cons_vector is array (fifteen downto zero) of real;
  type time_cons_vector is array (fifteen downto zero) of time;
  type natural_cons_vector is array (fifteen downto zero) of natural;
  type positive_cons_vector is array (fifteen downto zero) of positive;
  
  type boolean_cons_vectorofvector is array (zero to fifteen) of boolean_cons_vector;
  type severity_level_cons_vectorofvector is array (zero to fifteen) of severity_level_cons_vector;
  type integer_cons_vectorofvector is array (zero to fifteen) of integer_cons_vector ;
  type real_cons_vectorofvector is array (zero to fifteen) of real_cons_vector;
  type time_cons_vectorofvector is array (zero to fifteen) of time_cons_vector;
  type natural_cons_vectorofvector is array (zero to fifteen) of natural_cons_vector;
  type positive_cons_vectorofvector is array (zero to fifteen) of positive_cons_vector; 
  subtype column is integer range one to two;
  subtype row is integer range one to eight;
  type s2boolean_cons_vector is array (row,column) of boolean;
  type s2bit_cons_vector is array (row,column) of bit;
  type s2char_cons_vector is array (row,column) of character;
  type s2severity_level_cons_vector is array (row,column) of severity_level;
  type s2integer_cons_vector is array (row,column) of integer;
  type s2real_cons_vector is array (row,column) of real;
  type s2time_cons_vector is array (row,column) of time;
  type s2natural_cons_vector is array (row,column) of natural;
  type s2positive_cons_vector is array (row,column) of positive;
  
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
                               j:string(one to seven);
                               k:bit_vector(zero to three);
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
  
  type record_2cons_array is record
                               a:s2boolean_cons_vector;
                               b:s2bit_cons_vector;
                               c:s2char_cons_vector;
                               d:s2severity_level_cons_vector;
                               e:s2integer_cons_vector;
                               f:s2real_cons_vector;
                               g:s2time_cons_vector;
                               h:s2natural_cons_vector;
                               i:s2positive_cons_vector;
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
                             a:boolean_vector(zero to fifteen);
                             b:severity_level_vector(zero to fifteen);
                             c:integer_vector(zero to fifteen);
                             d:real_vector(zero to fifteen);
                             e:time_vector(zero to fifteen);
                             f:natural_vector(zero to fifteen);
                             g:positive_vector(zero to fifteen);
                           end record;
  
  type record_of_records is record
                              a: record_std_package;
                              c: record_cons_array;
                              e: record_2cons_array;
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
  type array_rec_2cons is array (integer range <>) of record_2cons_array;
  type array_rec_rec is array (integer range <>) of record_of_records;
  
  subtype array_rec_std_st is array_rec_std (hi_to_low_range);
  subtype array_rec_cons_st is array_rec_cons (hi_to_low_range);
  subtype array_rec_2cons_st is  array_rec_2cons (hi_to_low_range);
  subtype array_rec_rec_st is array_rec_rec (hi_to_low_range);
  
  type record_of_arr_of_record is record
                                    a: array_rec_std(zero to seven);
                                    b: array_rec_cons(zero to seven);
                                    c: array_rec_2cons(zero to seven);
                                    d: array_rec_rec(zero to seven);
                                  end record;
  
  type four_value is ('Z','0','1','X');                                 --enumerated type
  type four_value_vector is array (natural range <>) of four_value;
  subtype four_value_vector_range is four_value_vector(hi_to_low_range);
  
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
--   function resolution14(i:in four_value_vector) return four_value;        --bus resolution
--   subtype four_value_state is resolution14 four_value;                    --function type
  type four_value_map is array(four_value) of boolean;
  subtype binary is four_value range '0' to '1';
  type byte is array(zero to seven) of bit;
  subtype word is bit_vector(zero to fifteen);                                 --constrained array
  constant size :integer := seven;
  type primary_memory is array(zero to size) of word;                     --array of an array
  type primary_memory_module is                                       --record with field
    record                                                               --as an array
      enable:binary;
      memory_number:primary_memory;
    end record;
  type whole_memory is array(0 to size) of primary_memory_module;     --array of a complex record
  subtype delay is integer range one to 10;
  
  
  constant     C12 : boolean_vector := (C1,false);
  constant     C13 : severity_level_vector := (C4,error);
  constant     C14 : integer_vector := (one,two,three,four);
  constant     C15 : real_vector := (1.0,2.0,C6,4.0);
  constant     C16 : time_vector := (1 ns, 2 ns,C7, 4 ns);
  constant     C17 : natural_vector := (one,2,3,4);
  constant     C18 : positive_vector := (one,2,3,4);
  constant C19 : boolean_cons_vector := (others => C1);
  constant C20 : severity_level_cons_vector := (others => C4);
  constant C21 : integer_cons_vector := (others => C5);
  constant C22 : real_cons_vector := (others => C6);
  constant C23 : time_cons_vector :=  (others => C7);
  constant C24 : natural_cons_vector :=  (others => C8);
  constant C25 : positive_cons_vector :=  (others => C9);
  constant C26 : boolean_cons_vectorofvector := (others => (others => C1));
  constant C27 : severity_level_cons_vectorofvector :=  (others => (others => C4));
  constant C28 : integer_cons_vectorofvector := (others => (others => C5));
  constant C29 : real_cons_vectorofvector := (others => (others => C6));
  constant C30 : time_cons_vectorofvector := (others => (others => C7));
  constant C31 : natural_cons_vectorofvector := (others => (others => C8));
  constant C32 : positive_cons_vectorofvector := (others => (others => C9));
  
BEGIN
  assert (hi_to_low_range'left = 0) report "generic for left bound of hi_to_low_range not working" severity failure;
  assert (hi_to_low_range'right = 7) report "generic for right bound of hi_to_low_range not working" severity failure;
  assert (row'left = 1) report "generic constrained for left bound of row not working" severity failure;
  assert (row'right = 8) report "generic constrained for right bound of row not working" severity failure;
  assert (column'left = 1) report "generic constrained for left bound of column not working" severity failure;
  assert (column'right = 2) report "generic constrained for right bound of column not working" severity failure;
  assert (boolean_cons_vector'left = 15) report "generic constrained for left bound of array not working" severity failure;
  assert (severity_level_cons_vector'left = 15) report "generic constrained for left bound of array not working" severity failure;
  assert (integer_cons_vector'left = 15) report "generic constrained for left bound of array not working" severity failure;
  assert (real_cons_vector'left = 15) report "generic constrained for left bound of array not working" severity failure;
  assert (time_cons_vector'left = 15) report "generic constrained for left bound of array not working" severity failure;
  assert (natural_cons_vector'left = 15) report "generic constrained for left bound of array not working" severity failure;
  assert (positive_cons_vector'left = 15) report "generic constrained for left bound of array not working" severity failure;
  assert (boolean_cons_vector'right = 0) report "generic constrained for right bound of array not working" severity failure;
  assert (severity_level_cons_vector'right = 0) report "generic constrained for right bound of array not working" severity failure;
  assert (integer_cons_vector'right = 0) report "generic constrained for right bound of array not working" severity failure;
  assert (real_cons_vector'right = 0) report "generic constrained for right bound of array not working" severity failure;
  assert (time_cons_vector'right = 0) report "generic constrained for right bound of array not working" severity failure;
  assert (natural_cons_vector'right = 0) report "generic constrained for right bound of array not working" severity failure;
  assert (positive_cons_vector'right = 0) report "generic constrained for right bound of array not working" severity failure;
  assert  (boolean_cons_vectorofvector'left = 0) report "generic constrained for left bound of array not working" severity failure;
  assert  (severity_level_cons_vectorofvector'left = 0) report "generic constrained for left bound of array not working" severity failure;
  assert  (integer_cons_vectorofvector'left = 0) report "generic constrained for left bound of array not working" severity failure;
  assert  (real_cons_vectorofvector'left = 0) report "generic constrained for left bound of array not working" severity failure;
  assert  (time_cons_vectorofvector'left = 0) report "generic constrained for left bound of array not working" severity failure;
  assert  (natural_cons_vectorofvector'left = 0) report "generic constrained for left bound of array not working" severity failure;
  assert  (positive_cons_vectorofvector'left = 0) report "generic constrained for left bound of array not working" severity failure;
  assert  (boolean_cons_vectorofvector'right = 15) report "generic constrained for right bound of array not working" severity failure;
  assert  (severity_level_cons_vectorofvector'right = 15) report "generic constrained for right bound of array not working" severity failure;
  assert  (integer_cons_vectorofvector'right = 15) report "generic constrained for right bound of array not working" severity failure;
  assert  (real_cons_vectorofvector'right = 15) report "generic constrained for right bound of array not working" severity failure;
  assert  (time_cons_vectorofvector'right = 15) report "generic constrained for right bound of array not working" severity failure;
  assert  (natural_cons_vectorofvector'right = 15) report "generic constrained for right bound of array not working" severity failure;
  assert  (positive_cons_vectorofvector'right = 15) report "generic constrained for right bound of array not working" severity failure;

  TESTING: PROCESS
  BEGIN

    assert    NOT(    (hi_to_low_range'left = 0) and 
                      (hi_to_low_range'right = 7) and 
                      (row'left = 1) and 
                      (row'right = 8) and 
                      (column'left = 1) and 
                      (column'right = 2) and 
                      (boolean_cons_vector'left = 15) and 
                      (severity_level_cons_vector'left = 15)   and 
                      (integer_cons_vector'left = 15) and 
                      (real_cons_vector'left = 15)    and 
                      (time_cons_vector'left = 15)    and 
                      (natural_cons_vector'left = 15) and 
                      (positive_cons_vector'left = 15) and 
                      (boolean_cons_vector'right = 0) and 
                      (severity_level_cons_vector'right = 0)   and 
                      (integer_cons_vector'right = 0) and 
                      (real_cons_vector'right = 0)    and 
                      (time_cons_vector'right = 0)    and 
                      (natural_cons_vector'right = 0) and 
                      (positive_cons_vector'right = 0) and 
                      (boolean_cons_vectorofvector'left = 0) and 
                      (severity_level_cons_vectorofvector'left = 0) and 
                      (integer_cons_vectorofvector'left = 0) and 
                      (real_cons_vectorofvector'left = 0)    and 
                      (time_cons_vectorofvector'left = 0)    and 
                      (natural_cons_vectorofvector'left = 0) and 
                      (positive_cons_vectorofvector'left = 0) and 
                      (boolean_cons_vectorofvector'right = 15) and 
                      (severity_level_cons_vectorofvector'right = 15) and 
                      (integer_cons_vectorofvector'right = 15) and 
                      (real_cons_vectorofvector'right = 15)    and 
                      (time_cons_vectorofvector'right = 15)    and 
                      (natural_cons_vectorofvector'right = 15) and 
                      (positive_cons_vectorofvector'right = 15)    ) 
      report "***PASSED TEST: c01s01b01x01p05n02i00757"
      severity NOTE;
    assert    (    (hi_to_low_range'left = 0) and 
                   (hi_to_low_range'right = 7) and 
                   (row'left = 1) and 
                   (row'right = 8) and 
                   (column'left = 1) and 
                   (column'right = 2) and 
                   (boolean_cons_vector'left = 15) and 
                   (severity_level_cons_vector'left = 15)   and 
                   (integer_cons_vector'left = 15) and 
                   (real_cons_vector'left = 15)    and 
                   (time_cons_vector'left = 15)    and 
                   (natural_cons_vector'left = 15) and 
                   (positive_cons_vector'left = 15) and 
                   (boolean_cons_vector'right = 0) and 
                   (severity_level_cons_vector'right = 0)   and 
                   (integer_cons_vector'right = 0) and 
                   (real_cons_vector'right = 0)    and 
                   (time_cons_vector'right = 0)    and 
                   (natural_cons_vector'right = 0) and 
                   (positive_cons_vector'right = 0) and 
                   (boolean_cons_vectorofvector'left = 0) and 
                   (severity_level_cons_vectorofvector'left = 0) and 
                   (integer_cons_vectorofvector'left = 0) and 
                   (real_cons_vectorofvector'left = 0)    and 
                   (time_cons_vectorofvector'left = 0)    and 
                   (natural_cons_vectorofvector'left = 0) and 
                   (positive_cons_vectorofvector'left = 0) and 
                   (boolean_cons_vectorofvector'right = 15) and 
                   (severity_level_cons_vectorofvector'right = 15) and 
                   (integer_cons_vectorofvector'right = 15) and 
                   (real_cons_vectorofvector'right = 15)    and 
                   (time_cons_vectorofvector'right = 15)    and 
                   (natural_cons_vectorofvector'right = 15) and 
                   (positive_cons_vectorofvector'right = 15)    ) 
      report "***FAILED TEST: c01s01b01x01p05n02i00757 - Generic can be used to specify the size of ports."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c01s01b01x01p05n02i00757arch;
