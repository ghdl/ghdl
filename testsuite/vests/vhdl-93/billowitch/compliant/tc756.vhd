
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
-- $Id: tc756.vhd,v 1.2 2001-10-26 16:30:00 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c01s01b01x01p05n02i00756ent IS
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
END c01s01b01x01p05n02i00756ent;

ARCHITECTURE c01s01b01x01p05n02i00756arch OF c01s01b01x01p05n02i00756ent IS
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
                                    a: array_rec_std(zero to seven);
                                    b: array_rec_cons(zero to seven);
                                    c: array_rec_rec(zero to seven);
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
  
  type byte is array(zero to seven) of bit;
  
  subtype word is bit_vector(zero to fifteen);                                 --constrained array
  
  constant size :integer := seven;
  
  type primary_memory is array(zero to size) of word;                     --array of an array
  
  type primary_memory_module is                                       --record with field
    record                                                               --as an array
      enable:bit;
      memory_number:primary_memory;
    end record;
  type whole_memory is array(0 to size) of primary_memory_module;     --array of a complex record
  subtype delay is integer range one to 10;
  
  constant C12    : boolean_vector := (C1,false);
  constant C13    : severity_level_vector := (C4,error);
  constant C14    : integer_vector := (one,two,three,four);
  constant C15    : real_vector := (1.0,2.0,C6,4.0);
  constant C16    : time_vector := (1 ns, 2 ns,C7, 4 ns);
  constant C17    : natural_vector := (one,2,3,4);
  constant C18    : positive_vector := (one,2,3,4);
  constant C19    : boolean_cons_vector := (others => C1);
  constant C20    : severity_level_cons_vector := (others => C4);
  constant C21    : integer_cons_vector := (others => C5);
  constant C22    : real_cons_vector := (others => C6);
  constant C23    : time_cons_vector :=  (others => C7);
  constant C24    : natural_cons_vector :=  (others => C8);
  constant C25    : positive_cons_vector :=  (others => C9);
  constant C26    : boolean_cons_vectorofvector := (others => (others => C1));
  constant C27    : severity_level_cons_vectorofvector :=  (others => (others => C4));
  constant C28    : integer_cons_vectorofvector := (others => (others => C5));
  constant C29    : real_cons_vectorofvector := (others => (others => C6));
  constant C30    : time_cons_vectorofvector := (others => (others => C7));
  constant C31    : natural_cons_vectorofvector := (others => (others => C8));
  constant C32    : positive_cons_vectorofvector := (others => (others => C9));
  constant C50    : record_std_package := (C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11);
  constant C51    : record_cons_array := (C19,C20,C21,C22,C23,C24,C25);
  constant C53    : record_cons_arrayofarray := (C26,C27,C28,C29,C30,C31,C32);
  constant C70    : boolean_vector_st :=(others => C1);
  constant C71    : severity_level_vector_st:= (others => C4);
  constant C72    : integer_vector_st:=(others => C5);
  constant C73    : real_vector_st:=(others => C6);
  constant C74    : time_vector_st:=(others => C7);
  constant C75    : natural_vector_st:=(others => C8);
  constant C76    : positive_vector_st:=(others => C9);
  constant C77    : record_array_st := (C70,C71,C72,C73,C74,C75,C76);
  constant C54a    : record_array_st := (C70,C71,C72,C73,C74,C75,C76);
  constant C54b   : record_array_new:= (C70,C71,C72,C73,C74,C75,C76);
  constant C55    : record_of_records := (C50,C51,C53,C77,C54b);
  constant C60    : byte := (others => '0');
  constant C61    : word := (others =>'0' );
  constant C64    : primary_memory := (others => C61);
  constant C65    : primary_memory_module := ('1',C64);
  constant C66    : whole_memory := (others => C65);
  constant C67    : current := 1 A;
  constant C68    : resistance := 1 Ohm;
  constant C69    : delay := 2;
  constant C78   : boolean_vector_range := (others => C1);
  constant C79   : severity_level_vector_range := (others => C4) ;
  constant C80   : integer_vector_range :=(others => C5) ;
  constant C81   : real_vector_range :=(others => C6);
  constant C82   : time_vector_range :=(others => C7);
  constant C83   : natural_vector_range :=(others => C8);
  constant C84   : positive_vector_range :=(others => C9);
  constant C85   : array_rec_std(0 to 7) :=(others => C50)  ;
  constant C86   : array_rec_cons (0 to 7) :=(others => C51);
  constant C88   : array_rec_rec(0 to 7) :=(others => C55);
  constant C102   : record_of_arr_of_record:= (C85,C86,C88);
  
BEGIN
  TESTING: PROCESS
    variable V1    : boolean_vector(zero to fifteen);
    variable V2    : severity_level_vector(zero to fifteen);
    variable V3    : integer_vector(zero to fifteen);
    variable V4    : real_vector(zero to fifteen);
    variable V5    : time_vector (zero to fifteen);
    variable V6    : natural_vector(zero to fifteen);
    variable V7    : positive_vector(zero to fifteen);
    variable V8    : boolean_cons_vector;
    variable V9    : severity_level_cons_vector ;
    variable V10    : integer_cons_vector;
    variable V11    : real_cons_vector;
    variable V12    : time_cons_vector ;
    variable V13    : natural_cons_vector ;
    variable V14    : positive_cons_vector ;
    variable V15    : boolean_cons_vectorofvector;
    variable V16    : severity_level_cons_vectorofvector;
    variable V17    : integer_cons_vectorofvector;
    variable V18    : real_cons_vectorofvector;
    variable V19    : time_cons_vectorofvector;
    variable V20    : natural_cons_vectorofvector;
    variable V21    : positive_cons_vectorofvector;
    variable V22    : record_std_package;
    variable V23    : record_cons_array;
    variable V24    : record_cons_arrayofarray ;
    variable V25    : boolean_vector_st;
    variable V26    : severity_level_vector_st;
    variable V27    : integer_vector_st;
    variable V28    : real_vector_st;
    variable V29    : time_vector_st;
    variable V30    : natural_vector_st;
    variable V31    : positive_vector_st;
    variable V32    : record_array_st;
    variable V33    : record_array_st;
    variable V34   : record_array_new;
    variable V35    : record_of_records;
    variable V36    : byte;
    variable V37    : word;
    variable V38    : current_vector(zero to three);
    variable V39    : resistance_vector(zero to three);
    variable V40    : delay;
    variable V41   : boolean_vector_range;
    variable V42   : severity_level_vector_range ;
    variable V43   : integer_vector_range ;
    variable V44   : real_vector_range ;
    variable V45   : time_vector_range ;
    variable V46   : natural_vector_range ;
    variable V47   : positive_vector_range ;
    variable V48   : array_rec_std(zero to seven);
    variable V49   : array_rec_cons(zero to seven);
    variable V50   : array_rec_rec(zero to seven);
    variable V51   : record_of_arr_of_record;

  BEGIN
    assert (V1'left = 0) report " boolean_vector(zero to fifteen) error in the left generic value" severity  error;
    assert (V2'left = 0) report " severity_level_vector(zero to fifteen) error in the left generic value" severity  error;
    assert (V3'left = 0) report " integer_vector(zero to fifteen) error in the left generic value" severity  error;
    assert (V4'left = 0) report " real_vector(zero to fifteen) error in the left generic value" severity  error;
    assert (V5'left = 0) report " time_vector (zero to fifteen) error in the left generic value" severity  error;
    assert (V6'left = 0) report " natural_vector(zero to fifteen) error in the left generic value" severity  error;
    assert (V7'left = 0) report " positive_vector(zero to fifteen) error in the left generic value" severity  error;
    assert (V8'left = 15) report " boolean_cons_vector error in the left generic value" severity  error;
    assert (V9'left = 15) report " severity_level_cons_vector  error in the left generic value" severity  error;
    assert (V10'left = 15) report " integer_cons_vector error in the left generic value" severity  error;
    assert (V11'left = 15) report " real_cons_vector error in the left generic value" severity  error;
    assert (V12'left = 15) report " time_cons_vector  error in the left generic value" severity  error;
    assert (V13'left = 15) report " natural_cons_vector  error in the left generic value" severity  error;
    assert (V14'left = 15) report " positive_cons_vector  error in the left generic value" severity  error;
    assert (V15'left = 0) report " boolean_cons_vectorofvector error in the left generic value" severity  error;
    assert (V16'left = 0) report " severity_level_cons_vectorofvector error in the left generic value" severity  error;
    assert (V17'left = 0) report " integer_cons_vectorofvector error in the left generic value" severity  error;
    assert (V18'left = 0) report " real_cons_vectorofvector error in the left generic value" severity  error;
    assert (V19'left = 0) report " time_cons_vectorofvector error in the left generic value" severity  error;
    assert (V20'left = 0) report " natural_cons_vectorofvector error in the left generic value" severity  error;
    assert (V21'left = 0) report " positive_cons_vectorofvector error in the left generic value" severity  error;
    assert (V22.j'left = 1) report " record_std_package error in the left generic value" severity  error;
    assert (V22.k'left = 0) report " record_std_package error in the left generic value" severity  error;
    assert (V23.a'left = 15) report " record_cons_array error in the left generic value" severity  error;
    assert (V23.b'left = 15) report " record_cons_array error in the left generic value" severity  error;
    assert (V23.c'left = 15) report " record_cons_array error in the left generic value" severity  error;
    assert (V23.d'left = 15) report " record_cons_array error in the left generic value" severity  error;
    assert (V23.e'left = 15) report " record_cons_array error in the left generic value" severity  error;
    assert (V23.f'left = 15) report " record_cons_array error in the left generic value" severity  error;
    assert (V23.g'left = 15) report " record_cons_array error in the left generic value" severity  error;
    assert (V24.a'left = 0) report " record_cons_arrayofarray  error in the left generic value" severity  error;
    assert (V24.b'left = 0) report " record_cons_arrayofarray  error in the left generic value" severity  error;
    assert (V24.c'left = 0) report " record_cons_arrayofarray  error in the left generic value" severity  error;
    assert (V24.d'left = 0) report " record_cons_arrayofarray  error in the left generic value" severity  error;
    assert (V24.e'left = 0) report " record_cons_arrayofarray  error in the left generic value" severity  error;
    assert (V24.f'left = 0) report " record_cons_arrayofarray  error in the left generic value" severity  error;
    assert (V24.g'left = 0) report " record_cons_arrayofarray  error in the left generic value" severity  error;
    assert (V25'left = 0) report " boolean_vector_st error in the left generic value" severity  error;
    assert (V26'left = 0) report "  severity_level_vector_st error in the left generic value" severity  error;
    assert (V27'left = 0) report "  integer_vector_st error in the left generic value" severity  error;
    assert (V28'left = 0) report "  real_vector_st error in the left generic value" severity  error;
    assert (V29'left = 0) report "  time_vector_st error in the left generic value" severity  error;
    assert (V30'left = 0) report "  natural_vector_st error in the left generic value" severity  error;
    assert (V31'left = 0) report " positive_vector_st error in the left generic value" severity  error;
    assert (V32.a'left = 0) report " record_array_st error in the left generic value" severity  error;
    assert (V32.b'left = 0) report " record_array_st error in the left generic value" severity  error;
    assert (V32.c'left = 0) report " record_array_st error in the left generic value" severity  error;
    assert (V32.d'left = 0) report " record_array_st error in the left generic value" severity  error;
    assert (V32.e'left = 0) report " record_array_st error in the left generic value" severity  error;
    assert (V32.f'left = 0) report " record_array_st error in the left generic value" severity  error;
    assert (V32.g'left = 0) report " record_array_st error in the left generic value" severity  error;
    assert (V34.a'left = 0) report " record_array_new error in the left generic value" severity  error;
    assert (V34.b'left = 0) report " record_array_new error in the left generic value" severity  error;
    assert (V34.c'left = 0) report " record_array_new error in the left generic value" severity  error;
    assert (V34.d'left = 0) report " record_array_new error in the left generic value" severity  error;
    assert (V34.e'left = 0) report " record_array_new error in the left generic value" severity  error;
    assert (V34.f'left = 0) report " record_array_new error in the left generic value" severity  error;
    assert (V34.g'left = 0) report " record_array_new error in the left generic value" severity  error;
    assert (V36'left = 0) report " byte error in the left generic value" severity  error;
    assert (V37'left = 0) report " word error in the left generic value" severity  error;
    assert (V38'left = 0) report " current_vector(zero to three) error in the left generic value" severity  error;
    assert (V39'left = 0) report " resistance_vector(zero to three) error in the left generic value" severity  error;
--assert (V40'left = 1) report " delay error in the left generic value" severity  error;
    assert (V41'left = 0) report " boolean_vector_range error in the left generic value" severity  error;
    assert (V42'left = 0) report " severity_level_vector_range  error in the left generic value" severity  error;
    assert (V43'left = 0) report " integer_vector_range  error in the left generic value" severity  error;
    assert (V44'left = 0) report " real_vector_range  error in the left generic value" severity  error;
    assert (V45'left = 0) report " time_vector_range  error in the left generic value" severity  error;
    assert (V46'left = 0) report " natural_vector_range  error in the left generic value" severity  error;
    assert (V47'left = 0) report " positive_vector_range  error in the left generic value" severity  error;
    assert (V48'left = 0) report " array_rec_std(zero to seven) error in the left generic value" severity  error;
    assert (V49'left = 0) report " array_rec_cons(zero to seven) error in the left generic value" severity  error;
    assert (V50'left = 0) report " array_rec_rec(zero to seven) error in the left generic value" severity  error;
    assert (V51.a'left = 0) report " record_of_arr_of_record error in the left generic value" severity  error;
    assert (V51.b'left = 0) report " record_of_arr_of_record error in the left generic value" severity  error;
    assert (V51.c'left = 0) report " record_of_arr_of_record error in the left generic value" severity  error;
    
    assert (V1'right = 15) report " boolean_vector(zero to fifteen) error in the right generic value" severity  error;
    assert (V2'right = 15) report " severity_level_vector(zero to fifteen) error in the right generic value" severity  error;
    assert (V3'right = 15) report " integer_vector(zero to fifteen) error in the right generic value" severity  error;
    assert (V4'right = 15) report " real_vector(zero to fifteen) error in the right generic value" severity  error;
    assert (V5'right = 15) report " time_vector (zero to fifteen) error in the right generic value" severity  error;
    assert (V6'right = 15) report " natural_vector(zero to fifteen) error in the right generic value" severity  error;
    assert (V7'right = 15) report " positive_vector(zero to fifteen) error in the right generic value" severity  error;
    assert (V8'right = 0) report " boolean_cons_vector error in the right generic value" severity  error;
    assert (V9'right = 0) report " severity_level_cons_vector  error in the right generic value" severity  error;
    assert (V10'right = 0) report " integer_cons_vector error in the right generic value" severity  error;
    assert (V11'right = 0) report " real_cons_vector error in the right generic value" severity  error;
    assert (V12'right = 0) report " time_cons_vector  error in the right generic value" severity  error;
    assert (V13'right = 0) report " natural_cons_vector  error in the right generic value" severity  error;
    assert (V14'right = 0) report " positive_cons_vector  error in the right generic value" severity  error;
    assert (V15'right = 15) report " boolean_cons_vectorofvector error in the right generic value" severity  error;
    assert (V16'right = 15) report " severity_level_cons_vectorofvector error in the right generic value" severity  error;
    assert (V17'right = 15) report " integer_cons_vectorofvector error in the right generic value" severity  error;
    assert (V18'right = 15) report " real_cons_vectorofvector error in the right generic value" severity  error;
    assert (V19'right = 15) report " time_cons_vectorofvector error in the right generic value" severity  error;
    assert (V20'right = 15) report " natural_cons_vectorofvector error in the right generic value" severity  error;
    assert (V21'right = 15) report " positive_cons_vectorofvector error in the right generic value" severity  error;
    assert (V22.j'right = 7) report " record_std_package error in the right generic value" severity  error;
    assert (V22.k'right = 3) report " record_std_package error in the right generic value" severity  error;
    assert (V23.a'right = 0) report " record_cons_array error in the right generic value" severity  error;
    assert (V23.b'right = 0) report " record_cons_array error in the right generic value" severity  error;
    assert (V23.c'right = 0) report " record_cons_array error in the right generic value" severity  error;
    assert (V23.d'right = 0) report " record_cons_array error in the right generic value" severity  error;
    assert (V23.e'right = 0) report " record_cons_array error in the right generic value" severity  error;
    assert (V23.f'right = 0) report " record_cons_array error in the right generic value" severity  error;
    assert (V23.g'right = 0) report " record_cons_array error in the right generic value" severity  error;
    assert (V24.a'right = 15) report " record_cons_arrayofarray  error in the right generic value" severity  error;
    assert (V24.b'right = 15) report " record_cons_arrayofarray  error in the right generic value" severity  error;
    assert (V24.c'right = 15) report " record_cons_arrayofarray  error in the right generic value" severity  error;
    assert (V24.d'right = 15) report " record_cons_arrayofarray  error in the right generic value" severity  error;
    assert (V24.e'right = 15) report " record_cons_arrayofarray  error in the right generic value" severity  error;
    assert (V24.f'right = 15) report " record_cons_arrayofarray  error in the right generic value" severity  error;
    assert (V24.g'right = 15) report " record_cons_arrayofarray  error in the right generic value" severity  error;
    assert (V25'right = 15) report " boolean_vector_st error in the right generic value" severity  error;
    assert (V26'right = 15) report "  severity_level_vector_st error in the right generic value" severity  error;
    assert (V27'right = 15) report "  integer_vector_st error in the right generic value" severity  error;
    assert (V28'right = 15) report "  real_vector_st error in the right generic value" severity  error;
    assert (V29'right = 15) report "  time_vector_st error in the right generic value" severity  error;
    assert (V30'right = 15) report "  natural_vector_st error in the right generic value" severity  error;
    assert (V31'right = 15) report " positive_vector_st error in the right generic value" severity  error;
    assert (V32.a'right = 15) report " record_array_st error in the right generic value" severity  error;
    assert (V32.b'right = 15) report " record_array_st error in the right generic value" severity  error;
    assert (V32.c'right = 15) report " record_array_st error in the right generic value" severity  error;
    assert (V32.d'right = 15) report " record_array_st error in the right generic value" severity  error;
    assert (V32.e'right = 15) report " record_array_st error in the right generic value" severity  error;
    assert (V32.f'right = 15) report " record_array_st error in the right generic value" severity  error;
    assert (V32.g'right = 15) report " record_array_st error in the right generic value" severity  error;
    assert (V34.a'right = 15) report " record_array_new error in the right generic value" severity  error;
    assert (V34.b'right = 15) report " record_array_new error in the right generic value" severity  error;
    assert (V34.c'right = 15) report " record_array_new error in the right generic value" severity  error;
    assert (V34.d'right = 15) report " record_array_new error in the right generic value" severity  error;
    assert (V34.e'right = 15) report " record_array_new error in the right generic value" severity  error;
    assert (V34.f'right = 15) report " record_array_new error in the right generic value" severity  error;
    assert (V34.g'right = 15) report " record_array_new error in the right generic value" severity  error;
    assert (V36'right = 7) report " byte error in the right generic value" severity  error;
    assert (V37'right = 15) report " word error in the right generic value" severity  error;
    assert (V38'right = 3) report " current_vector(zero to three) error in the right generic value" severity  error;
    assert (V39'right = 3) report " resistance_vector(zero to three) error in the right generic value" severity  error;
--assert (V40'right = 1) report " delay error in the right generic value" severity  error;
    assert (V41'right = 7) report " boolean_vector_range error in the right generic value" severity  error;
    assert (V42'right = 7) report " severity_level_vector_range  error in the right generic value" severity  error;
    assert (V43'right = 7) report " integer_vector_range  error in the right generic value" severity  error;
    assert (V44'right = 7) report " real_vector_range  error in the right generic value" severity  error;
    assert (V45'right = 7) report " time_vector_range  error in the right generic value" severity  error;
    assert (V46'right = 7) report " natural_vector_range  error in the right generic value" severity  error;
    assert (V47'right = 7) report " positive_vector_range  error in the right generic value" severity  error;
    assert (V48'right = 7) report " array_rec_std(zero to seven) error in the right generic value" severity  error;
    assert (V49'right = 7) report " array_rec_cons(zero to seven) error in the right generic value" severity  error;
    assert (V50'right = 7) report " array_rec_rec(zero to seven) error in the right generic value" severity  error;
    assert (V51.a'right = 7) report " record_of_arr_of_record error in the right generic value" severity  error;
    assert (V51.b'right = 7) report " record_of_arr_of_record error in the right generic value" severity  error;
    assert (V51.c'right = 7) report " record_of_arr_of_record error in the right generic value" severity  error;
    assert (V1'length = 16) report " boolean_vector(zero to fifteen) error in the length generic value" severity  error;
    assert (V2'length = 16) report " severity_level_vector(zero to fifteen) error in the length generic value" severity  error;
    assert (V3'length = 16) report " integer_vector(zero to fifteen) error in the length generic value" severity  error;
    assert (V4'length = 16) report " real_vector(zero to fifteen) error in the length generic value" severity  error;
    assert (V5'length = 16) report " time_vector (zero to fifteen) error in the length generic value" severity  error;
    assert (V6'length = 16) report " natural_vector(zero to fifteen) error in the length generic value" severity  error;
    assert (V7'length = 16) report " positive_vector(zero to fifteen) error in the length generic value" severity  error;
    assert (V8'length = 16) report " boolean_cons_vector error in the length generic value" severity  error;
    assert (V9'length = 16) report " severity_level_cons_vector  error in the length generic value" severity  error;
    assert (V10'length = 16) report " integer_cons_vector error in the length generic value" severity  error;
    assert (V11'length = 16) report " real_cons_vector error in the length generic value" severity  error;
    assert (V12'length = 16) report " time_cons_vector  error in the length generic value" severity  error;
    assert (V13'length = 16) report " natural_cons_vector  error in the length generic value" severity  error;
    assert (V14'length = 16) report " positive_cons_vector  error in the length generic value" severity  error;
    assert (V15'length = 16) report " boolean_cons_vectorofvector error in the length generic value" severity  error;
    assert (V16'length = 16) report " severity_level_cons_vectorofvector error in the length generic value" severity  error;
    assert (V17'length = 16) report " integer_cons_vectorofvector error in the length generic value" severity  error;
    assert (V18'length = 16) report " real_cons_vectorofvector error in the length generic value" severity  error;
    assert (V19'length = 16) report " time_cons_vectorofvector error in the length generic value" severity  error;
    assert (V20'length = 16) report " natural_cons_vectorofvector error in the length generic value" severity  error;
    assert (V21'length = 16) report " positive_cons_vectorofvector error in the length generic value" severity  error;
    assert (V22.j'length = 7) report " record_std_package error in the length generic value" severity  error;
    assert (V22.k'length = 4) report " record_std_package error in the length generic value" severity  error;
    assert (V23.a'length = 16) report " record_cons_array error in the length generic value" severity  error;
    assert (V23.b'length = 16) report " record_cons_array error in the length generic value" severity  error;
    assert (V23.c'length = 16) report " record_cons_array error in the length generic value" severity  error;
    assert (V23.d'length = 16) report " record_cons_array error in the length generic value" severity  error;
    assert (V23.e'length = 16) report " record_cons_array error in the length generic value" severity  error;
    assert (V23.f'length = 16) report " record_cons_array error in the length generic value" severity  error;
    assert (V23.g'length = 16) report " record_cons_array error in the length generic value" severity  error;
    assert (V24.a'length = 16) report " record_cons_arrayofarray  error in the length generic value" severity  error;
    assert (V24.b'length = 16) report " record_cons_arrayofarray  error in the length generic value" severity  error;
    assert (V24.c'length = 16) report " record_cons_arrayofarray  error in the length generic value" severity  error;
    assert (V24.d'length = 16) report " record_cons_arrayofarray  error in the length generic value" severity  error;
    assert (V24.e'length = 16) report " record_cons_arrayofarray  error in the length generic value" severity  error;
    assert (V24.f'length = 16) report " record_cons_arrayofarray  error in the length generic value" severity  error;
    assert (V24.g'length = 16) report " record_cons_arrayofarray  error in the length generic value" severity  error;
    assert (V25'length = 16) report " boolean_vector_st error in the length generic value" severity  error;
    assert (V26'length = 16) report "  severity_level_vector_st error in the length generic value" severity  error;
    assert (V27'length = 16) report "  integer_vector_st error in the length generic value" severity  error;
    assert (V28'length = 16) report "  real_vector_st error in the length generic value" severity  error;
    assert (V29'length = 16) report "  time_vector_st error in the length generic value" severity  error;
    assert (V30'length = 16) report "  natural_vector_st error in the length generic value" severity  error;
    assert (V31'length = 16) report " positive_vector_st error in the length generic value" severity  error;
    assert (V32.a'length = 16) report " record_array_st error in the length generic value" severity  error;
    assert (V32.b'length = 16) report " record_array_st error in the length generic value" severity  error;
    assert (V32.c'length = 16) report " record_array_st error in the length generic value" severity  error;
    assert (V32.d'length = 16) report " record_array_st error in the length generic value" severity  error;
    assert (V32.e'length = 16) report " record_array_st error in the length generic value" severity  error;
    assert (V32.f'length = 16) report " record_array_st error in the length generic value" severity  error;
    assert (V32.g'length = 16) report " record_array_st error in the length generic value" severity  error;
    assert (V34.a'length = 16) report " record_array_new error in the length generic value" severity  error;
    assert (V34.b'length = 16) report " record_array_new error in the length generic value" severity  error;
    assert (V34.c'length = 16) report " record_array_new error in the length generic value" severity  error;
    assert (V34.d'length = 16) report " record_array_new error in the length generic value" severity  error;
    assert (V34.e'length = 16) report " record_array_new error in the length generic value" severity  error;
    assert (V34.f'length = 16) report " record_array_new error in the length generic value" severity  error;
    assert (V34.g'length = 16) report " record_array_new error in the length generic value" severity  error;
    assert (V36'length = 8) report " byte error in the length generic value" severity  error;
    assert (V37'length = 16) report " word error in the length generic value" severity  error;
    assert (V38'length = 4) report " current_vector(zero to three) error in the length generic value" severity  error;
    assert (V39'length = 4) report " resistance_vector(zero to three) error in the length generic value" severity  error;
--assert (V40'length = 1) report " delay error in the length generic value" severity  error;
    assert (V41'length = 8) report " boolean_vector_range error in the length generic value" severity  error;
    assert (V42'length = 8) report " severity_level_vector_range  error in the length generic value" severity  error;
    assert (V43'length = 8) report " integer_vector_range  error in the length generic value" severity  error;
    assert (V44'length = 8) report " real_vector_range  error in the length generic value" severity  error;
    assert (V45'length = 8) report " time_vector_range  error in the length generic value" severity  error;
    assert (V46'length = 8) report " natural_vector_range  error in the length generic value" severity  error;
    assert (V48'length = 8) report " positive_vector_range  error in the length generic value" severity  error;
    assert (V48'length = 8) report " array_rec_std(zero to seven) error in the length generic value" severity  error;
    assert (V49'length = 8) report " array_rec_cons(zero to seven) error in the length generic value" severity  error;
    assert (V50'length = 8) report " array_rec_rec(zero to seven) error in the length generic value" severity  error;
    assert (V51.a'length = 8) report " record_of_arr_of_record error in the length generic value" severity  error;
    assert (V51.b'length = 8) report " record_of_arr_of_record error in the length generic value" severity  error;
    assert (V51.c'length = 8) report " record_of_arr_of_record error in the length generic value" severity  error;


    assert   NOT(    (V1'left = 0)    and 
                     (V2'left = 0)    and 
                     (V3'left = 0)    and 
                     (V4'left = 0)    and 
                     (V5'left = 0)    and 
                     (V6'left = 0)    and 
                     (V7'left = 0)    and 
                     (V8'left = 15) and 
                     (V9'left = 15) and 
                     (V10'left = 15) and 
                     (V11'left = 15) and 
                     (V12'left = 15) and 
                     (V13'left = 15) and 
                     (V14'left = 15) and 
                     (V15'left = 0) and 
                     (V16'left = 0) and 
                     (V17'left = 0) and 
                     (V18'left = 0) and 
                     (V19'left = 0) and 
                     (V20'left = 0) and 
                     (V21'left = 0) and 
                     (V22.j'left = 1) and 
                     (V22.k'left = 0) and 
                     (V23.a'left = 15) and 
                     (V23.b'left = 15) and 
                     (V23.c'left = 15) and 
                     (V23.d'left = 15) and 
                     (V23.e'left = 15) and 
                     (V23.f'left = 15) and 
                     (V23.g'left = 15) and 
                     (V24.a'left = 0) and 
                     (V24.b'left = 0) and 
                     (V24.c'left = 0) and 
                     (V24.d'left = 0) and 
                     (V24.e'left = 0) and 
                     (V24.f'left = 0) and 
                     (V24.g'left = 0) and 
                     (V25'left = 0) and 
                     (V26'left = 0) and 
                     (V27'left = 0) and 
                     (V28'left = 0) and 
                     (V29'left = 0) and 
                     (V30'left = 0) and 
                     (V31'left = 0) and 
                     (V32.a'left = 0) and 
                     (V32.b'left = 0) and 
                     (V32.c'left = 0) and 
                     (V32.d'left = 0) and 
                     (V32.e'left = 0) and 
                     (V32.f'left = 0) and 
                     (V32.g'left = 0) and 
                     (V34.a'left = 0) and 
                     (V34.b'left = 0) and 
                     (V34.c'left = 0) and 
                     (V34.d'left = 0) and 
                     (V34.e'left = 0) and 
                     (V34.f'left = 0) and 
                     (V34.g'left = 0) and 
                     (V36'left = 0) and 
                     (V37'left = 0) and 
                     (V38'left = 0) and 
                     (V39'left = 0) and 
--          (V40'left = 1) and 
                     (V42'left = 0) and 
                     (V43'left = 0) and 
                     (V44'left = 0) and 
                     (V45'left = 0) and 
                     (V46'left = 0) and 
                     (V47'left = 0) and 
                     (V48'left = 0) and 
                     (V49'left = 0) and 
                     (V50'left = 0) and 
                     (V51.a'left = 0) and 
                     (V51.b'left = 0) and 
                     (V51.c'left = 0) and 
                     (V1'right = 15) and 
                     (V2'right = 15) and 
                     (V3'right = 15) and 
                     (V4'right = 15) and 
                     (V5'right = 15) and 
                     (V6'right = 15) and 
                     (V7'right = 15) and 
                     (V8'right = 0) and 
                     (V9'right = 0) and 
                     (V10'right = 0)and 
                     (V11'right = 0) and 
                     (V12'right = 0) and 
                     (V13'right = 0) and 
                     (V14'right = 0) and 
                     (V15'right = 15) and 
                     (V16'right = 15) and 
                     (V17'right = 15) and 
                     (V18'right = 15) and 
                     (V19'right = 15) and 
                     (V20'right = 15) and 
                     (V21'right = 15) and 
                     (V22.j'right = 7) and 
                     (V22.k'right = 3) and 
                     (V23.a'right = 0) and 
                     (V23.b'right = 0) and 
                     (V23.c'right = 0) and 
                     (V23.d'right = 0) and 
                     (V23.e'right = 0) and 
                     (V23.f'right = 0) and 
                     (V23.g'right = 0) and 
                     (V24.a'right = 15) and 
                     (V24.b'right = 15) and 
                     (V24.c'right = 15) and 
                     (V24.d'right = 15) and 
                     (V24.e'right = 15) and 
                     (V24.f'right = 15) and 
                     (V24.g'right = 15) and 
                     (V25'right = 15) and 
                     (V26'right = 15) and 
                     (V27'right = 15) and 
                     (V28'right = 15) and 
                     (V29'right = 15) and 
                     (V30'right = 15) and 
                     (V31'right = 15) and 
                     (V32.a'right = 15) and 
                     (V32.b'right = 15) and 
                     (V32.c'right = 15) and 
                     (V32.d'right = 15) and 
                     (V32.e'right = 15) and 
                     (V32.f'right = 15) and 
                     (V32.g'right = 15) and 
                     (V34.a'right = 15) and 
                     (V34.b'right = 15) and 
                     (V34.c'right = 15) and 
                     (V34.d'right = 15) and 
                     (V34.e'right = 15) and 
                     (V34.f'right = 15) and 
                     (V34.g'right = 15) and 
                     (V36'right = 7) and 
                     (V37'right = 15) and  
                     (V38'right = 3) and 
                     (V39'right = 3) and 
--          (V40'right = 1) and 
                     (V41'right = 7) and 
                     (V42'right = 7) and 
                     (V43'right = 7) and 
                     (V44'right = 7) and 
                     (V45'right = 7) and 
                     (V46'right = 7) and 
                     (V47'right = 7) and 
                     (V48'right = 7) and 
                     (V49'right = 7) and 
                     (V50'right = 7) and 
                     (V51.a'right = 7) and 
                     (V51.b'right = 7) and 
                     (V51.c'right = 7) and 
                     (V1'length = 16) and 
                     (V2'length = 16) and 
                     (V3'length = 16) and 
                     (V4'length = 16) and 
                     (V5'length = 16) and 
                     (V6'length = 16) and 
                     (V7'length = 16) and 
                     (V8'length = 16) and 
                     (V9'length = 16) and 
                     (V10'length = 16) and 
                     (V11'length = 16) and 
                     (V12'length = 16) and 
                     (V13'length = 16) and 
                     (V14'length = 16) and 
                     (V15'length = 16) and 
                     (V16'length = 16) and 
                     (V17'length = 16) and 
                     (V18'length = 16) and 
                     (V19'length = 16) and 
                     (V20'length = 16) and 
                     (V21'length = 16) and 
                     (V22.j'length = 7)and 
                     (V22.k'length = 4) and 
                     (V23.a'length = 16) and 
                     (V23.b'length = 16) and 
                     (V23.c'length = 16) and 
                     (V23.d'length = 16) and 
                     (V23.e'length = 16) and 
                     (V23.f'length = 16) and 
                     (V23.g'length = 16) and 
                     (V24.a'length = 16) and 
                     (V24.b'length = 16) and 
                     (V24.c'length = 16) and 
                     (V24.d'length = 16) and 
                     (V24.e'length = 16) and 
                     (V24.f'length = 16) and 
                     (V24.g'length = 16) and 
                     (V25'length = 16) and 
                     (V26'length = 16) and 
                     (V27'length = 16) and 
                     (V28'length = 16) and 
                     (V29'length = 16) and 
                     (V30'length = 16) and 
                     (V31'length = 16) and 
                     (V32.a'length = 16) and 
                     (V32.b'length = 16) and 
                     (V32.c'length = 16) and 
                     (V32.d'length = 16) and 
                     (V32.e'length = 16) and 
                     (V32.f'length = 16) and 
                     (V32.g'length = 16) and 
                     (V34.a'length = 16) and 
                     (V34.b'length = 16) and 
                     (V34.c'length = 16) and 
                     (V34.d'length = 16) and 
                     (V34.e'length = 16) and 
                     (V34.f'length = 16) and 
                     (V34.g'length = 16) and 
                     (V36'length = 8) and 
                     (V37'length = 16) and 
                     (V38'length = 4) and 
                     (V39'length = 4) and 
--          (V40'length = 1) and 
                     (V41'length = 8) and 
                     (V42'length = 8) and 
                     (V43'length = 8) and 
                     (V44'length = 8) and 
                     (V45'length = 8) and 
                     (V46'length = 8) and 
                     (V48'length = 8) and 
                     (V48'length = 8) and 
                     (V49'length = 8) and 
                     (V50'length = 8) and 
                     (V51.a'length = 8) and 
                     (V51.b'length = 8) and 
                     (V51.c'length = 8) ) 
      report "***PASSED TEST: c01s01b01x01p05n02i00756"
      severity NOTE;
    assert   ((V1'left = 0)    and 
              (V2'left = 0)    and 
              (V3'left = 0)    and 
              (V4'left = 0)    and 
              (V5'left = 0)    and 
              (V6'left = 0)    and 
              (V7'left = 0)    and 
              (V8'left = 15) and 
              (V9'left = 15) and 
              (V10'left = 15) and 
              (V11'left = 15) and 
              (V12'left = 15) and 
              (V13'left = 15) and 
              (V14'left = 15) and 
              (V15'left = 0) and 
              (V16'left = 0) and 
              (V17'left = 0) and 
              (V18'left = 0) and 
              (V19'left = 0) and 
              (V20'left = 0) and 
              (V21'left = 0) and 
              (V22.j'left = 1) and 
              (V22.k'left = 0) and 
              (V23.a'left = 15) and 
              (V23.b'left = 15) and 
              (V23.c'left = 15) and 
              (V23.d'left = 15) and 
              (V23.e'left = 15) and 
              (V23.f'left = 15) and 
              (V23.g'left = 15) and 
              (V24.a'left = 0) and 
              (V24.b'left = 0) and 
              (V24.c'left = 0) and 
              (V24.d'left = 0) and 
              (V24.e'left = 0) and 
              (V24.f'left = 0) and 
              (V24.g'left = 0) and 
              (V25'left = 0) and 
              (V26'left = 0) and 
              (V27'left = 0) and 
              (V28'left = 0) and 
              (V29'left = 0) and 
              (V30'left = 0) and 
              (V31'left = 0) and 
              (V32.a'left = 0) and 
              (V32.b'left = 0) and 
              (V32.c'left = 0) and 
              (V32.d'left = 0) and 
              (V32.e'left = 0) and 
              (V32.f'left = 0) and 
              (V32.g'left = 0) and 
              (V34.a'left = 0) and 
              (V34.b'left = 0) and 
              (V34.c'left = 0) and 
              (V34.d'left = 0) and 
              (V34.e'left = 0) and 
              (V34.f'left = 0) and 
              (V34.g'left = 0) and 
              (V36'left = 0) and 
              (V37'left = 0) and 
              (V38'left = 0) and 
              (V39'left = 0) and 
--          (V40'left = 1) and 
              (V42'left = 0) and 
              (V43'left = 0) and 
              (V44'left = 0) and 
              (V45'left = 0) and 
              (V46'left = 0) and 
              (V47'left = 0) and 
              (V48'left = 0) and 
              (V49'left = 0) and 
              (V50'left = 0) and 
              (V51.a'left = 0) and 
              (V51.b'left = 0) and 
              (V51.c'left = 0) and 
              (V1'right = 15) and 
              (V2'right = 15) and 
              (V3'right = 15) and 
              (V4'right = 15) and 
              (V5'right = 15) and 
              (V6'right = 15) and 
              (V7'right = 15) and 
              (V8'right = 0) and 
              (V9'right = 0) and 
              (V10'right = 0)and 
              (V11'right = 0) and 
              (V12'right = 0) and 
              (V13'right = 0) and 
              (V14'right = 0) and 
              (V15'right = 15) and 
              (V16'right = 15) and 
              (V17'right = 15) and 
              (V18'right = 15) and 
              (V19'right = 15) and 
              (V20'right = 15) and 
              (V21'right = 15) and 
              (V22.j'right = 7) and 
              (V22.k'right = 3) and 
              (V23.a'right = 0) and 
              (V23.b'right = 0) and 
              (V23.c'right = 0) and 
              (V23.d'right = 0) and 
              (V23.e'right = 0) and 
              (V23.f'right = 0) and 
              (V23.g'right = 0) and 
              (V24.a'right = 15) and 
              (V24.b'right = 15) and 
              (V24.c'right = 15) and 
              (V24.d'right = 15) and 
              (V24.e'right = 15) and 
              (V24.f'right = 15) and 
              (V24.g'right = 15) and 
              (V25'right = 15) and 
              (V26'right = 15) and 
              (V27'right = 15) and 
              (V28'right = 15) and 
              (V29'right = 15) and 
              (V30'right = 15) and 
              (V31'right = 15) and 
              (V32.a'right = 15) and 
              (V32.b'right = 15) and 
              (V32.c'right = 15) and 
              (V32.d'right = 15) and 
              (V32.e'right = 15) and 
              (V32.f'right = 15) and 
              (V32.g'right = 15) and 
              (V34.a'right = 15) and 
              (V34.b'right = 15) and 
              (V34.c'right = 15) and 
              (V34.d'right = 15) and 
              (V34.e'right = 15) and 
              (V34.f'right = 15) and 
              (V34.g'right = 15) and 
              (V36'right = 7) and 
              (V37'right = 15) and  
              (V38'right = 3) and 
              (V39'right = 3) and 
--          (V40'right = 1) and 
              (V41'right = 7) and 
              (V42'right = 7) and 
              (V43'right = 7) and 
              (V44'right = 7) and 
              (V45'right = 7) and 
              (V46'right = 7) and 
              (V47'right = 7) and 
              (V48'right = 7) and 
              (V49'right = 7) and 
              (V50'right = 7) and 
              (V51.a'right = 7) and 
              (V51.b'right = 7) and 
              (V51.c'right = 7) and 
              (V1'length = 16) and 
              (V2'length = 16) and 
              (V3'length = 16) and 
              (V4'length = 16) and 
              (V5'length = 16) and 
              (V6'length = 16) and 
              (V7'length = 16) and 
              (V8'length = 16) and 
              (V9'length = 16) and 
              (V10'length = 16) and 
              (V11'length = 16) and 
              (V12'length = 16) and 
              (V13'length = 16) and 
              (V14'length = 16) and 
              (V15'length = 16) and 
              (V16'length = 16) and 
              (V17'length = 16) and 
              (V18'length = 16) and 
              (V19'length = 16) and 
              (V20'length = 16) and 
              (V21'length = 16) and 
              (V22.j'length = 7)and 
              (V22.k'length = 4) and 
              (V23.a'length = 16) and 
              (V23.b'length = 16) and 
              (V23.c'length = 16) and 
              (V23.d'length = 16) and 
              (V23.e'length = 16) and 
              (V23.f'length = 16) and 
              (V23.g'length = 16) and 
              (V24.a'length = 16) and 
              (V24.b'length = 16) and 
              (V24.c'length = 16) and 
              (V24.d'length = 16) and 
              (V24.e'length = 16) and 
              (V24.f'length = 16) and 
              (V24.g'length = 16) and 
              (V25'length = 16) and 
              (V26'length = 16) and 
              (V27'length = 16) and 
              (V28'length = 16) and 
              (V29'length = 16) and 
              (V30'length = 16) and 
              (V31'length = 16) and 
              (V32.a'length = 16) and 
              (V32.b'length = 16) and 
              (V32.c'length = 16) and 
              (V32.d'length = 16) and 
              (V32.e'length = 16) and 
              (V32.f'length = 16) and 
              (V32.g'length = 16) and 
              (V34.a'length = 16) and 
              (V34.b'length = 16) and 
              (V34.c'length = 16) and 
              (V34.d'length = 16) and 
              (V34.e'length = 16) and 
              (V34.f'length = 16) and 
              (V34.g'length = 16) and 
              (V36'length = 8) and 
              (V37'length = 16) and 
              (V38'length = 4) and 
              (V39'length = 4) and 
--          (V40'length = 1) and 
              (V41'length = 8) and 
              (V42'length = 8) and 
              (V43'length = 8) and 
              (V44'length = 8) and 
              (V45'length = 8) and 
              (V46'length = 8) and 
              (V48'length = 8) and 
              (V48'length = 8) and 
              (V49'length = 8) and 
              (V50'length = 8) and 
              (V51.a'length = 8) and 
              (V51.b'length = 8) and 
              (V51.c'length = 8) ) 
      report "***FAILED TEST: c01s01b01x01p05n02i00756 - Generic can be used to specify the size of ports."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c01s01b01x01p05n02i00756arch;
