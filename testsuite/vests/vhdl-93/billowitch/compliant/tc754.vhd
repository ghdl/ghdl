
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
-- $Id: tc754.vhd,v 1.2 2001-10-26 16:30:00 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c01s01b01x01p05n02i00754pkg is
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
  subtype integer_vector_st    is  integer_vector(0 to 15);
  subtype real_vector_st    is  real_vector(0 to 15);
  subtype time_vector_st    is  time_vector(0 to 15);
  subtype natural_vector_st    is  natural_vector(0 to 15);
  subtype positive_vector_st    is  positive_vector(0 to 15);
  type boolean_cons_vector    is array (15 downto 0) of boolean;
  type severity_level_cons_vector    is array (15 downto 0) of severity_level;
  type integer_cons_vector    is array (15 downto 0) of integer;
  type real_cons_vector    is array (15 downto 0) of real;
  type time_cons_vector    is array (15 downto 0) of time;
  type natural_cons_vector    is array (15 downto 0) of natural;
  type positive_cons_vector    is array (15 downto 0) of positive;
  type boolean_cons_vectorofvector       is array (0 to 15) of boolean_cons_vector;
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
  subtype boolean_vector_range    is boolean_vector(hi_to_low_range);
  subtype severity_level_vector_range    is severity_level_vector(hi_to_low_range);
  subtype integer_vector_range    is integer_vector(hi_to_low_range);
  subtype real_vector_range       is real_vector(hi_to_low_range);
  subtype time_vector_range       is time_vector(hi_to_low_range);
  subtype natural_vector_range    is natural_vector(hi_to_low_range);
  subtype positive_vector_range    is positive_vector(hi_to_low_range);
  type array_rec_std       is array (integer range <>) of record_std_package;
  type array_rec_cons       is array (integer range <>) of record_cons_array;
  type array_rec_rec       is array (integer range <>) of record_of_records;
  subtype array_rec_std_st    is array_rec_std (hi_to_low_range);
  subtype array_rec_cons_st    is array_rec_cons (hi_to_low_range);
  subtype array_rec_rec_st    is array_rec_rec (hi_to_low_range);
  
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
  
  constant C1    : boolean := true;
  constant C2    : bit := '1';
  constant C3    : character := 's';
  constant C4    : severity_level := note;
  constant C5    : integer := 3;
  constant C6    : real := 3.0;
  constant C7    : time := 3 ns;
  constant C8    : natural := 1;
  constant C9    : positive := 1;
  constant C10    : string := "shishir";
  constant C11    : bit_vector := B"0011";
  constant C12    : boolean_vector := (C1,false);
  constant C13    : severity_level_vector := (C4,error);
  constant C14    : integer_vector := (1,2,3,4);
  constant C15    : real_vector := (1.0,2.0,C6,4.0);
  constant C16    : time_vector := (1 ns, 2 ns,C7, 4 ns);
  constant C17    : natural_vector := (1,2,3,4);
  constant C18    : positive_vector := (1,2,3,4);
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

end c01s01b01x01p05n02i00754pkg;


use work.c01s01b01x01p05n02i00754pkg.all;
ENTITY c01s01b01x01p05n02i00754ent IS
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
    S1    : boolean_vector(zero to fifteen);
    S2    : severity_level_vector(zero to fifteen);
    S3    : integer_vector(zero to fifteen);
    S4    : real_vector(zero to fifteen);
    S5    : time_vector (zero to fifteen);
    S6    : natural_vector(zero to fifteen);
    S7    : positive_vector(zero to fifteen);
    S8    : boolean_cons_vector;
    S9    : severity_level_cons_vector ;
    S10    : integer_cons_vector;
    S11    : real_cons_vector;
    S12    : time_cons_vector ;
    S13    : natural_cons_vector ;
    S14    : positive_cons_vector ;
    S15    : boolean_cons_vectorofvector;
    S16    : severity_level_cons_vectorofvector;
    S17    : integer_cons_vectorofvector;
    S18    : real_cons_vectorofvector;
    S19    : time_cons_vectorofvector;
    S20    : natural_cons_vectorofvector;
    S21    : positive_cons_vectorofvector;
    S22    : record_std_package;
    S23    : record_cons_array;
    S24    : record_cons_arrayofarray ;
    S25    : boolean_vector_st;
    S26    : severity_level_vector_st;
    S27    : integer_vector_st;
    S28    : real_vector_st;
    S29    : time_vector_st;
    S30    : natural_vector_st;
    S31    : positive_vector_st;
    S32    : record_array_st;
    S33    : record_array_st;
    S34   : record_array_new;
    S35    : record_of_records;
    S36    : byte;
    S37    : word;
    S38    : current_vector(zero to three);
    S39    : resistance_vector(zero to three);
    S40    : delay;
    S41   : boolean_vector_range;
    S42   : severity_level_vector_range ;
    S43   : integer_vector_range ;
    S44   : real_vector_range ;
    S45   : time_vector_range ;
    S46   : natural_vector_range ;
    S47   : positive_vector_range ;
    S48   : array_rec_std(zero to seven);
    S49   : array_rec_cons(zero to seven);
    S50   : array_rec_rec(zero to seven);
    S51   : record_of_arr_of_record
    );
END c01s01b01x01p05n02i00754ent;

ARCHITECTURE c01s01b01x01p05n02i00754arch OF c01s01b01x01p05n02i00754ent IS

BEGIN
  assert (S1'left = 0) report " boolean_vector(zero to fifteen) error in the left generic value" severity  error;
  assert (S2'left = 0) report " severity_level_vector(zero to fifteen) error in the left generic value" severity  error;
  assert (S3'left = 0) report " integer_vector(zero to fifteen) error in the left generic value" severity  error;
  assert (S4'left = 0) report " real_vector(zero to fifteen) error in the left generic value" severity  error;
  assert (S5'left = 0) report " time_vector (zero to fifteen) error in the left generic value" severity  error;
  assert (S6'left = 0) report " natural_vector(zero to fifteen) error in the left generic value" severity  error;
  assert (S7'left = 0) report " positive_vector(zero to fifteen) error in the left generic value" severity  error;
  assert (S8'left = 15) report " boolean_cons_vector error in the left generic value" severity  error;
  assert (S9'left = 15) report " severity_level_cons_vector  error in the left generic value" severity  error;
  assert (S10'left = 15) report " integer_cons_vector error in the left generic value" severity  error;
  assert (S11'left = 15) report " real_cons_vector error in the left generic value" severity  error;
  assert (S12'left = 15) report " time_cons_vector  error in the left generic value" severity  error;
  assert (S13'left = 15) report " natural_cons_vector  error in the left generic value" severity  error;
  assert (S14'left = 15) report " positive_cons_vector  error in the left generic value" severity  error;
  assert (S15'left = 0) report " boolean_cons_vectorofvector error in the left generic value" severity  error;
  assert (S16'left = 0) report " severity_level_cons_vectorofvector error in the left generic value" severity  error;
  assert (S17'left = 0) report " integer_cons_vectorofvector error in the left generic value" severity  error;
  assert (S18'left = 0) report " real_cons_vectorofvector error in the left generic value" severity  error;
  assert (S19'left = 0) report " time_cons_vectorofvector error in the left generic value" severity  error;
  assert (S20'left = 0) report " natural_cons_vectorofvector error in the left generic value" severity  error;
  assert (S21'left = 0) report " positive_cons_vectorofvector error in the left generic value" severity  error;
  assert (S22.j'left = 1) report " record_std_package error in the left generic value" severity  error;
  assert (S22.k'left = 0) report " record_std_package error in the left generic value" severity  error;
  assert (S23.a'left = 15) report " record_cons_array error in the left generic value" severity  error;
  assert (S23.b'left = 15) report " record_cons_array error in the left generic value" severity  error;
  assert (S23.c'left = 15) report " record_cons_array error in the left generic value" severity  error;
  assert (S23.d'left = 15) report " record_cons_array error in the left generic value" severity  error;
  assert (S23.e'left = 15) report " record_cons_array error in the left generic value" severity  error;
  assert (S23.f'left = 15) report " record_cons_array error in the left generic value" severity  error;
  assert (S23.g'left = 15) report " record_cons_array error in the left generic value" severity  error;
  assert (S24.a'left = 0) report " record_cons_arrayofarray  error in the left generic value" severity  error;
  assert (S24.b'left = 0) report " record_cons_arrayofarray  error in the left generic value" severity  error;
  assert (S24.c'left = 0) report " record_cons_arrayofarray  error in the left generic value" severity  error;
  assert (S24.d'left = 0) report " record_cons_arrayofarray  error in the left generic value" severity  error;
  assert (S24.e'left = 0) report " record_cons_arrayofarray  error in the left generic value" severity  error;
  assert (S24.f'left = 0) report " record_cons_arrayofarray  error in the left generic value" severity  error;
  assert (S24.g'left = 0) report " record_cons_arrayofarray  error in the left generic value" severity  error;
  assert (S25'left = 0) report " boolean_vector_st error in the left generic value" severity  error;
  assert (S26'left = 0) report "  severity_level_vector_st error in the left generic value" severity  error;
  assert (S27'left = 0) report "  integer_vector_st error in the left generic value" severity  error;
  assert (S28'left = 0) report "  real_vector_st error in the left generic value" severity  error;
  assert (S29'left = 0) report "  time_vector_st error in the left generic value" severity  error;
  assert (S30'left = 0) report "  natural_vector_st error in the left generic value" severity  error;
  assert (S31'left = 0) report " positive_vector_st error in the left generic value" severity  error;
  assert (S32.a'left = 0) report " record_array_st error in the left generic value" severity  error;
  assert (S32.b'left = 0) report " record_array_st error in the left generic value" severity  error;
  assert (S32.c'left = 0) report " record_array_st error in the left generic value" severity  error;
  assert (S32.d'left = 0) report " record_array_st error in the left generic value" severity  error;
  assert (S32.e'left = 0) report " record_array_st error in the left generic value" severity  error;
  assert (S32.f'left = 0) report " record_array_st error in the left generic value" severity  error;
  assert (S32.g'left = 0) report " record_array_st error in the left generic value" severity  error;
  assert (S34.a'left = 0) report " record_array_new error in the left generic value" severity  error;
  assert (S34.b'left = 0) report " record_array_new error in the left generic value" severity  error;
  assert (S34.c'left = 0) report " record_array_new error in the left generic value" severity  error;
  assert (S34.d'left = 0) report " record_array_new error in the left generic value" severity  error;
  assert (S34.e'left = 0) report " record_array_new error in the left generic value" severity  error;
  assert (S34.f'left = 0) report " record_array_new error in the left generic value" severity  error;
  assert (S34.g'left = 0) report " record_array_new error in the left generic value" severity  error;
  assert (S36'left = 0) report " byte error in the left generic value" severity  error;
  assert (S37'left = 0) report " word error in the left generic value" severity  error;
  assert (S38'left = 0) report " current_vector(zero to three) error in the left generic value" severity  error;
  assert (S39'left = 0) report " resistance_vector(zero to three) error in the left generic value" severity  error;
--assert (S40'left = 1) report " delay error in the left generic value" severity  error;
  assert (S41'left = 0) report " boolean_vector_range error in the left generic value" severity  error;
  assert (S42'left = 0) report " severity_level_vector_range  error in the left generic value" severity  error;
  assert (S43'left = 0) report " integer_vector_range  error in the left generic value" severity  error;
  assert (S44'left = 0) report " real_vector_range  error in the left generic value" severity  error;
  assert (S45'left = 0) report " time_vector_range  error in the left generic value" severity  error;
  assert (S46'left = 0) report " natural_vector_range  error in the left generic value" severity  error;
  assert (S47'left = 0) report " positive_vector_range  error in the left generic value" severity  error;
  assert (S48'left = 0) report " array_rec_std(zero to seven) error in the left generic value" severity  error;
  assert (S49'left = 0) report " array_rec_cons(zero to seven) error in the left generic value" severity  error;
  assert (S50'left = 0) report " array_rec_rec(zero to seven) error in the left generic value" severity  error;
  assert (S51.a'left = 0) report " record_of_arr_of_record error in the left generic value" severity  error;
  assert (S51.b'left = 0) report " record_of_arr_of_record error in the left generic value" severity  error;
  assert (S51.c'left = 0) report " record_of_arr_of_record error in the left generic value" severity  error;
  
  assert (S1'right = 15) report " boolean_vector(zero to fifteen) error in the right generic value" severity  error;
  assert (S2'right = 15) report " severity_level_vector(zero to fifteen) error in the right generic value" severity  error;
  assert (S3'right = 15) report " integer_vector(zero to fifteen) error in the right generic value" severity  error;
  assert (S4'right = 15) report " real_vector(zero to fifteen) error in the right generic value" severity  error;
  assert (S5'right = 15) report " time_vector (zero to fifteen) error in the right generic value" severity  error;
  assert (S6'right = 15) report " natural_vector(zero to fifteen) error in the right generic value" severity  error;
  assert (S7'right = 15) report " positive_vector(zero to fifteen) error in the right generic value" severity  error;
  assert (S8'right = 0) report " boolean_cons_vector error in the right generic value" severity  error;
  assert (S9'right = 0) report " severity_level_cons_vector  error in the right generic value" severity  error;
  assert (S10'right = 0) report " integer_cons_vector error in the right generic value" severity  error;
  assert (S11'right = 0) report " real_cons_vector error in the right generic value" severity  error;
  assert (S12'right = 0) report " time_cons_vector  error in the right generic value" severity  error;
  assert (S13'right = 0) report " natural_cons_vector  error in the right generic value" severity  error;
  assert (S14'right = 0) report " positive_cons_vector  error in the right generic value" severity  error;
  assert (S15'right = 15) report " boolean_cons_vectorofvector error in the right generic value" severity  error;
  assert (S16'right = 15) report " severity_level_cons_vectorofvector error in the right generic value" severity  error;
  assert (S17'right = 15) report " integer_cons_vectorofvector error in the right generic value" severity  error;
  assert (S18'right = 15) report " real_cons_vectorofvector error in the right generic value" severity  error;
  assert (S19'right = 15) report " time_cons_vectorofvector error in the right generic value" severity  error;
  assert (S20'right = 15) report " natural_cons_vectorofvector error in the right generic value" severity  error;
  assert (S21'right = 15) report " positive_cons_vectorofvector error in the right generic value" severity  error;
  assert (S22.j'right = 7) report " record_std_package error in the right generic value" severity  error;
  assert (S22.k'right = 3) report " record_std_package error in the right generic value" severity  error;
  assert (S23.a'right = 0) report " record_cons_array error in the right generic value" severity  error;
  assert (S23.b'right = 0) report " record_cons_array error in the right generic value" severity  error;
  assert (S23.c'right = 0) report " record_cons_array error in the right generic value" severity  error;
  assert (S23.d'right = 0) report " record_cons_array error in the right generic value" severity  error;
  assert (S23.e'right = 0) report " record_cons_array error in the right generic value" severity  error;
  assert (S23.f'right = 0) report " record_cons_array error in the right generic value" severity  error;
  assert (S23.g'right = 0) report " record_cons_array error in the right generic value" severity  error;
  assert (S24.a'right = 15) report " record_cons_arrayofarray  error in the right generic value" severity  error;
  assert (S24.b'right = 15) report " record_cons_arrayofarray  error in the right generic value" severity  error;
  assert (S24.c'right = 15) report " record_cons_arrayofarray  error in the right generic value" severity  error;
  assert (S24.d'right = 15) report " record_cons_arrayofarray  error in the right generic value" severity  error;
  assert (S24.e'right = 15) report " record_cons_arrayofarray  error in the right generic value" severity  error;
  assert (S24.f'right = 15) report " record_cons_arrayofarray  error in the right generic value" severity  error;
  assert (S24.g'right = 15) report " record_cons_arrayofarray  error in the right generic value" severity  error;
  assert (S25'right = 15) report " boolean_vector_st error in the right generic value" severity  error;
  assert (S26'right = 15) report "  severity_level_vector_st error in the right generic value" severity  error;
  assert (S27'right = 15) report "  integer_vector_st error in the right generic value" severity  error;
  assert (S28'right = 15) report "  real_vector_st error in the right generic value" severity  error;
  assert (S29'right = 15) report "  time_vector_st error in the right generic value" severity  error;
  assert (S30'right = 15) report "  natural_vector_st error in the right generic value" severity  error;
  assert (S31'right = 15) report " positive_vector_st error in the right generic value" severity  error;
  assert (S32.a'right = 15) report " record_array_st error in the right generic value" severity  error;
  assert (S32.b'right = 15) report " record_array_st error in the right generic value" severity  error;
  assert (S32.c'right = 15) report " record_array_st error in the right generic value" severity  error;
  assert (S32.d'right = 15) report " record_array_st error in the right generic value" severity  error;
  assert (S32.e'right = 15) report " record_array_st error in the right generic value" severity  error;
  assert (S32.f'right = 15) report " record_array_st error in the right generic value" severity  error;
  assert (S32.g'right = 15) report " record_array_st error in the right generic value" severity  error;
  assert (S34.a'right = 15) report " record_array_new error in the right generic value" severity  error;
  assert (S34.b'right = 15) report " record_array_new error in the right generic value" severity  error;
  assert (S34.c'right = 15) report " record_array_new error in the right generic value" severity  error;
  assert (S34.d'right = 15) report " record_array_new error in the right generic value" severity  error;
  assert (S34.e'right = 15) report " record_array_new error in the right generic value" severity  error;
  assert (S34.f'right = 15) report " record_array_new error in the right generic value" severity  error;
  assert (S34.g'right = 15) report " record_array_new error in the right generic value" severity  error;
  assert (S36'right = 7) report " byte error in the right generic value" severity  error;
  assert (S37'right = 15) report " word error in the right generic value" severity  error;
  assert (S38'right = 3) report " current_vector(zero to three) error in the right generic value" severity  error;
  assert (S39'right = 3) report " resistance_vector(zero to three) error in the right generic value" severity  error;
--assert (S40'right = 1) report " delay error in the right generic value" severity  error;
  assert (S41'right = 7) report " boolean_vector_range error in the right generic value" severity  error;
  assert (S42'right = 7) report " severity_level_vector_range  error in the right generic value" severity  error;
  assert (S43'right = 7) report " integer_vector_range  error in the right generic value" severity  error;
  assert (S44'right = 7) report " real_vector_range  error in the right generic value" severity  error;
  assert (S45'right = 7) report " time_vector_range  error in the right generic value" severity  error;
  assert (S46'right = 7) report " natural_vector_range  error in the right generic value" severity  error;
  assert (S47'right = 7) report " positive_vector_range  error in the right generic value" severity  error;
  assert (S48'right = 7) report " array_rec_std(zero to seven) error in the right generic value" severity  error;
  assert (S49'right = 7) report " array_rec_cons(zero to seven) error in the right generic value" severity  error;
  assert (S50'right = 7) report " array_rec_rec(zero to seven) error in the right generic value" severity  error;
  assert (S51.a'right = 7) report " record_of_arr_of_record error in the right generic value" severity  error;
  assert (S51.b'right = 7) report " record_of_arr_of_record error in the right generic value" severity  error;
  assert (S51.c'right = 7) report " record_of_arr_of_record error in the right generic value" severity  error;
  assert (S1'length = 16) report " boolean_vector(zero to fifteen) error in the length generic value" severity  error;
  assert (S2'length = 16) report " severity_level_vector(zero to fifteen) error in the length generic value" severity  error;
  assert (S3'length = 16) report " integer_vector(zero to fifteen) error in the length generic value" severity  error;
  assert (S4'length = 16) report " real_vector(zero to fifteen) error in the length generic value" severity  error;
  assert (S5'length = 16) report " time_vector (zero to fifteen) error in the length generic value" severity  error;
  assert (S6'length = 16) report " natural_vector(zero to fifteen) error in the length generic value" severity  error;
  assert (S7'length = 16) report " positive_vector(zero to fifteen) error in the length generic value" severity  error;
  assert (S8'length = 16) report " boolean_cons_vector error in the length generic value" severity  error;
  assert (S9'length = 16) report " severity_level_cons_vector  error in the length generic value" severity  error;
  assert (S10'length = 16) report " integer_cons_vector error in the length generic value" severity  error;
  assert (S11'length = 16) report " real_cons_vector error in the length generic value" severity  error;
  assert (S12'length = 16) report " time_cons_vector  error in the length generic value" severity  error;
  assert (S13'length = 16) report " natural_cons_vector  error in the length generic value" severity  error;
  assert (S14'length = 16) report " positive_cons_vector  error in the length generic value" severity  error;
  assert (S15'length = 16) report " boolean_cons_vectorofvector error in the length generic value" severity  error;
  assert (S16'length = 16) report " severity_level_cons_vectorofvector error in the length generic value" severity  error;
  assert (S17'length = 16) report " integer_cons_vectorofvector error in the length generic value" severity  error;
  assert (S18'length = 16) report " real_cons_vectorofvector error in the length generic value" severity  error;
  assert (S19'length = 16) report " time_cons_vectorofvector error in the length generic value" severity  error;
  assert (S20'length = 16) report " natural_cons_vectorofvector error in the length generic value" severity  error;
  assert (S21'length = 16) report " positive_cons_vectorofvector error in the length generic value" severity  error;
  assert (S22.j'length = 7) report " record_std_package error in the length generic value" severity  error;
  assert (S22.k'length = 4) report " record_std_package error in the length generic value" severity  error;
  assert (S23.a'length = 16) report " record_cons_array error in the length generic value" severity  error;
  assert (S23.b'length = 16) report " record_cons_array error in the length generic value" severity  error;
  assert (S23.c'length = 16) report " record_cons_array error in the length generic value" severity  error;
  assert (S23.d'length = 16) report " record_cons_array error in the length generic value" severity  error;
  assert (S23.e'length = 16) report " record_cons_array error in the length generic value" severity  error;
  assert (S23.f'length = 16) report " record_cons_array error in the length generic value" severity  error;
  assert (S23.g'length = 16) report " record_cons_array error in the length generic value" severity  error;
  assert (S24.a'length = 16) report " record_cons_arrayofarray  error in the length generic value" severity  error;
  assert (S24.b'length = 16) report " record_cons_arrayofarray  error in the length generic value" severity  error;
  assert (S24.c'length = 16) report " record_cons_arrayofarray  error in the length generic value" severity  error;
  assert (S24.d'length = 16) report " record_cons_arrayofarray  error in the length generic value" severity  error;
  assert (S24.e'length = 16) report " record_cons_arrayofarray  error in the length generic value" severity  error;
  assert (S24.f'length = 16) report " record_cons_arrayofarray  error in the length generic value" severity  error;
  assert (S24.g'length = 16) report " record_cons_arrayofarray  error in the length generic value" severity  error;
  assert (S25'length = 16) report " boolean_vector_st error in the length generic value" severity  error;
  assert (S26'length = 16) report "  severity_level_vector_st error in the length generic value" severity  error;
  assert (S27'length = 16) report "  integer_vector_st error in the length generic value" severity  error;
  assert (S28'length = 16) report "  real_vector_st error in the length generic value" severity  error;
  assert (S29'length = 16) report "  time_vector_st error in the length generic value" severity  error;
  assert (S30'length = 16) report "  natural_vector_st error in the length generic value" severity  error;
  assert (S31'length = 16) report " positive_vector_st error in the length generic value" severity  error;
  assert (S32.a'length = 16) report " record_array_st error in the length generic value" severity  error;
  assert (S32.b'length = 16) report " record_array_st error in the length generic value" severity  error;
  assert (S32.c'length = 16) report " record_array_st error in the length generic value" severity  error;
  assert (S32.d'length = 16) report " record_array_st error in the length generic value" severity  error;
  assert (S32.e'length = 16) report " record_array_st error in the length generic value" severity  error;
  assert (S32.f'length = 16) report " record_array_st error in the length generic value" severity  error;
  assert (S32.g'length = 16) report " record_array_st error in the length generic value" severity  error;
  assert (S34.a'length = 16) report " record_array_new error in the length generic value" severity  error;
  assert (S34.b'length = 16) report " record_array_new error in the length generic value" severity  error;
  assert (S34.c'length = 16) report " record_array_new error in the length generic value" severity  error;
  assert (S34.d'length = 16) report " record_array_new error in the length generic value" severity  error;
  assert (S34.e'length = 16) report " record_array_new error in the length generic value" severity  error;
  assert (S34.f'length = 16) report " record_array_new error in the length generic value" severity  error;
  assert (S34.g'length = 16) report " record_array_new error in the length generic value" severity  error;
  assert (S36'length = 8) report " byte error in the length generic value" severity  error;
  assert (S37'length = 16) report " word error in the length generic value" severity  error;
  assert (S38'length = 4) report " current_vector(zero to three) error in the length generic value" severity  error;
  assert (S39'length = 4) report " resistance_vector(zero to three) error in the length generic value" severity  error;
--assert (S40'length = 1) report " delay error in the length generic value" severity  error;
  assert (S41'length = 8) report " boolean_vector_range error in the length generic value" severity  error;
  assert (S42'length = 8) report " severity_level_vector_range  error in the length generic value" severity  error;
  assert (S43'length = 8) report " integer_vector_range  error in the length generic value" severity  error;
  assert (S44'length = 8) report " real_vector_range  error in the length generic value" severity  error;
  assert (S45'length = 8) report " time_vector_range  error in the length generic value" severity  error;
  assert (S46'length = 8) report " natural_vector_range  error in the length generic value" severity  error;
  assert (S48'length = 8) report " positive_vector_range  error in the length generic value" severity  error;
  assert (S48'length = 8) report " array_rec_std(zero to seven) error in the length generic value" severity  error;
  assert (S49'length = 8) report " array_rec_cons(zero to seven) error in the length generic value" severity  error;
  assert (S50'length = 8) report " array_rec_rec(zero to seven) error in the length generic value" severity  error;
  assert (S51.a'length = 8) report " record_of_arr_of_record error in the length generic value" severity  error;
  assert (S51.b'length = 8) report " record_of_arr_of_record error in the length generic value" severity  error;
  assert (S51.c'length = 8) report " record_of_arr_of_record error in the length generic value" severity  error;

  TESTING: PROCESS
  BEGIN

    assert   NOT(    (S1'left = 0)    and 
                     (S2'left = 0)    and 
                     (S3'left = 0)    and 
                     (S4'left = 0)    and 
                     (S5'left = 0)    and 
                     (S6'left = 0)    and 
                     (S7'left = 0)    and 
                     (S8'left = 15) and 
                     (S9'left = 15) and 
                     (S10'left = 15) and 
                     (S11'left = 15) and 
                     (S12'left = 15) and 
                     (S13'left = 15) and 
                     (S14'left = 15) and 
                     (S15'left = 0) and 
                     (S16'left = 0) and 
                     (S17'left = 0) and 
                     (S18'left = 0) and 
                     (S19'left = 0) and 
                     (S20'left = 0) and 
                     (S21'left = 0) and 
                     (S22.j'left = 1) and 
                     (S22.k'left = 0) and 
                     (S23.a'left = 15) and 
                     (S23.b'left = 15) and 
                     (S23.c'left = 15) and 
                     (S23.d'left = 15) and 
                     (S23.e'left = 15) and 
                     (S23.f'left = 15) and 
                     (S23.g'left = 15) and 
                     (S24.a'left = 0) and 
                     (S24.b'left = 0) and 
                     (S24.c'left = 0) and 
                     (S24.d'left = 0) and 
                     (S24.e'left = 0) and 
                     (S24.f'left = 0) and 
                     (S24.g'left = 0) and 
                     (S25'left = 0) and 
                     (S26'left = 0) and 
                     (S27'left = 0) and 
                     (S28'left = 0) and 
                     (S29'left = 0) and 
                     (S30'left = 0) and 
                     (S31'left = 0) and 
                     (S32.a'left = 0) and 
                     (S32.b'left = 0) and 
                     (S32.c'left = 0) and 
                     (S32.d'left = 0) and 
                     (S32.e'left = 0) and 
                     (S32.f'left = 0) and 
                     (S32.g'left = 0) and 
                     (S34.a'left = 0) and 
                     (S34.b'left = 0) and 
                     (S34.c'left = 0) and 
                     (S34.d'left = 0) and 
                     (S34.e'left = 0) and 
                     (S34.f'left = 0) and 
                     (S34.g'left = 0) and 
                     (S36'left = 0) and 
                     (S37'left = 0) and 
                     (S38'left = 0) and 
                     (S39'left = 0) and 
--          (S40'left = 1) and 
                     (S42'left = 0) and 
                     (S43'left = 0) and 
                     (S44'left = 0) and 
                     (S45'left = 0) and 
                     (S46'left = 0) and 
                     (S47'left = 0) and 
                     (S48'left = 0) and 
                     (S49'left = 0) and 
                     (S50'left = 0) and 
                     (S51.a'left = 0) and 
                     (S51.b'left = 0) and 
                     (S51.c'left = 0) and 
                     (S1'right = 15) and 
                     (S2'right = 15) and 
                     (S3'right = 15) and 
                     (S4'right = 15) and 
                     (S5'right = 15) and 
                     (S6'right = 15) and 
                     (S7'right = 15) and 
                     (S8'right = 0) and 
                     (S9'right = 0) and 
                     (S10'right = 0)and 
                     (S11'right = 0) and 
                     (S12'right = 0) and 
                     (S13'right = 0) and 
                     (S14'right = 0) and 
                     (S15'right = 15) and 
                     (S16'right = 15) and 
                     (S17'right = 15) and 
                     (S18'right = 15) and 
                     (S19'right = 15) and 
                     (S20'right = 15) and 
                     (S21'right = 15) and 
                     (S22.j'right = 7) and 
                     (S22.k'right = 3) and 
                     (S23.a'right = 0) and 
                     (S23.b'right = 0) and 
                     (S23.c'right = 0) and 
                     (S23.d'right = 0) and 
                     (S23.e'right = 0) and 
                     (S23.f'right = 0) and 
                     (S23.g'right = 0) and 
                     (S24.a'right = 15) and 
                     (S24.b'right = 15) and 
                     (S24.c'right = 15) and 
                     (S24.d'right = 15) and 
                     (S24.e'right = 15) and 
                     (S24.f'right = 15) and 
                     (S24.g'right = 15) and 
                     (S25'right = 15) and 
                     (S26'right = 15) and 
                     (S27'right = 15) and 
                     (S28'right = 15) and 
                     (S29'right = 15) and 
                     (S30'right = 15) and 
                     (S31'right = 15) and 
                     (S32.a'right = 15) and 
                     (S32.b'right = 15) and 
                     (S32.c'right = 15) and 
                     (S32.d'right = 15) and 
                     (S32.e'right = 15) and 
                     (S32.f'right = 15) and 
                     (S32.g'right = 15) and 
                     (S34.a'right = 15) and 
                     (S34.b'right = 15) and 
                     (S34.c'right = 15) and 
                     (S34.d'right = 15) and 
                     (S34.e'right = 15) and 
                     (S34.f'right = 15) and 
                     (S34.g'right = 15) and 
                     (S36'right = 7) and 
                     (S37'right = 15) and  
                     (S38'right = 3) and 
                     (S39'right = 3) and 
--          (S40'right = 1) and 
                     (S41'right = 7) and 
                     (S42'right = 7) and 
                     (S43'right = 7) and 
                     (S44'right = 7) and 
                     (S45'right = 7) and 
                     (S46'right = 7) and 
                     (S47'right = 7) and 
                     (S48'right = 7) and 
                     (S49'right = 7) and 
                     (S50'right = 7) and 
                     (S51.a'right = 7) and 
                     (S51.b'right = 7) and 
                     (S51.c'right = 7) and 
                     (S1'length = 16) and 
                     (S2'length = 16) and 
                     (S3'length = 16) and 
                     (S4'length = 16) and 
                     (S5'length = 16) and 
                     (S6'length = 16) and 
                     (S7'length = 16) and 
                     (S8'length = 16) and 
                     (S9'length = 16) and 
                     (S10'length = 16) and 
                     (S11'length = 16) and 
                     (S12'length = 16) and 
                     (S13'length = 16) and 
                     (S14'length = 16) and 
                     (S15'length = 16) and 
                     (S16'length = 16) and 
                     (S17'length = 16) and 
                     (S18'length = 16) and 
                     (S19'length = 16) and 
                     (S20'length = 16) and 
                     (S21'length = 16) and 
                     (S22.j'length = 7)and 
                     (S22.k'length = 4) and 
                     (S23.a'length = 16) and 
                     (S23.b'length = 16) and 
                     (S23.c'length = 16) and 
                     (S23.d'length = 16) and 
                     (S23.e'length = 16) and 
                     (S23.f'length = 16) and 
                     (S23.g'length = 16) and 
                     (S24.a'length = 16) and 
                     (S24.b'length = 16) and 
                     (S24.c'length = 16) and 
                     (S24.d'length = 16) and 
                     (S24.e'length = 16) and 
                     (S24.f'length = 16) and 
                     (S24.g'length = 16) and 
                     (S25'length = 16) and 
                     (S26'length = 16) and 
                     (S27'length = 16) and 
                     (S28'length = 16) and 
                     (S29'length = 16) and 
                     (S30'length = 16) and 
                     (S31'length = 16) and 
                     (S32.a'length = 16) and 
                     (S32.b'length = 16) and 
                     (S32.c'length = 16) and 
                     (S32.d'length = 16) and 
                     (S32.e'length = 16) and 
                     (S32.f'length = 16) and 
                     (S32.g'length = 16) and 
                     (S34.a'length = 16) and 
                     (S34.b'length = 16) and 
                     (S34.c'length = 16) and 
                     (S34.d'length = 16) and 
                     (S34.e'length = 16) and 
                     (S34.f'length = 16) and 
                     (S34.g'length = 16) and 
                     (S36'length = 8) and 
                     (S37'length = 16) and 
                     (S38'length = 4) and 
                     (S39'length = 4) and 
--          (S40'length = 1) and 
                     (S41'length = 8) and 
                     (S42'length = 8) and 
                     (S43'length = 8) and 
                     (S44'length = 8) and 
                     (S45'length = 8) and 
                     (S46'length = 8) and 
                     (S48'length = 8) and 
                     (S48'length = 8) and 
                     (S49'length = 8) and 
                     (S50'length = 8) and 
                     (S51.a'length = 8) and 
                     (S51.b'length = 8) and 
                     (S51.c'length = 8) ) 
      report "***PASSED TEST: c01s01b01x01p05n02i00754"
      severity NOTE;
    assert   ((S1'left = 0)    and 
              (S2'left = 0)    and 
              (S3'left = 0)    and 
              (S4'left = 0)    and 
              (S5'left = 0)    and 
              (S6'left = 0)    and 
              (S7'left = 0)    and 
              (S8'left = 15) and 
              (S9'left = 15) and 
              (S10'left = 15) and 
              (S11'left = 15) and 
              (S12'left = 15) and 
              (S13'left = 15) and 
              (S14'left = 15) and 
              (S15'left = 0) and 
              (S16'left = 0) and 
              (S17'left = 0) and 
              (S18'left = 0) and 
              (S19'left = 0) and 
              (S20'left = 0) and 
              (S21'left = 0) and 
              (S22.j'left = 1) and 
              (S22.k'left = 0) and 
              (S23.a'left = 15) and 
              (S23.b'left = 15) and 
              (S23.c'left = 15) and 
              (S23.d'left = 15) and 
              (S23.e'left = 15) and 
              (S23.f'left = 15) and 
              (S23.g'left = 15) and 
              (S24.a'left = 0) and 
              (S24.b'left = 0) and 
              (S24.c'left = 0) and 
              (S24.d'left = 0) and 
              (S24.e'left = 0) and 
              (S24.f'left = 0) and 
              (S24.g'left = 0) and 
              (S25'left = 0) and 
              (S26'left = 0) and 
              (S27'left = 0) and 
              (S28'left = 0) and 
              (S29'left = 0) and 
              (S30'left = 0) and 
              (S31'left = 0) and 
              (S32.a'left = 0) and 
              (S32.b'left = 0) and 
              (S32.c'left = 0) and 
              (S32.d'left = 0) and 
              (S32.e'left = 0) and 
              (S32.f'left = 0) and 
              (S32.g'left = 0) and 
              (S34.a'left = 0) and 
              (S34.b'left = 0) and 
              (S34.c'left = 0) and 
              (S34.d'left = 0) and 
              (S34.e'left = 0) and 
              (S34.f'left = 0) and 
              (S34.g'left = 0) and 
              (S36'left = 0) and 
              (S37'left = 0) and 
              (S38'left = 0) and 
              (S39'left = 0) and 
--          (S40'left = 1) and 
              (S42'left = 0) and 
              (S43'left = 0) and 
              (S44'left = 0) and 
              (S45'left = 0) and 
              (S46'left = 0) and 
              (S47'left = 0) and 
              (S48'left = 0) and 
              (S49'left = 0) and 
              (S50'left = 0) and 
              (S51.a'left = 0) and 
              (S51.b'left = 0) and 
              (S51.c'left = 0) and 
              (S1'right = 15) and 
              (S2'right = 15) and 
              (S3'right = 15) and 
              (S4'right = 15) and 
              (S5'right = 15) and 
              (S6'right = 15) and 
              (S7'right = 15) and 
              (S8'right = 0) and 
              (S9'right = 0) and 
              (S10'right = 0)and 
              (S11'right = 0) and 
              (S12'right = 0) and 
              (S13'right = 0) and 
              (S14'right = 0) and 
              (S15'right = 15) and 
              (S16'right = 15) and 
              (S17'right = 15) and 
              (S18'right = 15) and 
              (S19'right = 15) and 
              (S20'right = 15) and 
              (S21'right = 15) and 
              (S22.j'right = 7) and 
              (S22.k'right = 3) and 
              (S23.a'right = 0) and 
              (S23.b'right = 0) and 
              (S23.c'right = 0) and 
              (S23.d'right = 0) and 
              (S23.e'right = 0) and 
              (S23.f'right = 0) and 
              (S23.g'right = 0) and 
              (S24.a'right = 15) and 
              (S24.b'right = 15) and 
              (S24.c'right = 15) and 
              (S24.d'right = 15) and 
              (S24.e'right = 15) and 
              (S24.f'right = 15) and 
              (S24.g'right = 15) and 
              (S25'right = 15) and 
              (S26'right = 15) and 
              (S27'right = 15) and 
              (S28'right = 15) and 
              (S29'right = 15) and 
              (S30'right = 15) and 
              (S31'right = 15) and 
              (S32.a'right = 15) and 
              (S32.b'right = 15) and 
              (S32.c'right = 15) and 
              (S32.d'right = 15) and 
              (S32.e'right = 15) and 
              (S32.f'right = 15) and 
              (S32.g'right = 15) and 
              (S34.a'right = 15) and 
              (S34.b'right = 15) and 
              (S34.c'right = 15) and 
              (S34.d'right = 15) and 
              (S34.e'right = 15) and 
              (S34.f'right = 15) and 
              (S34.g'right = 15) and 
              (S36'right = 7) and 
              (S37'right = 15) and  
              (S38'right = 3) and 
              (S39'right = 3) and 
--          (S40'right = 1) and 
              (S41'right = 7) and 
              (S42'right = 7) and 
              (S43'right = 7) and 
              (S44'right = 7) and 
              (S45'right = 7) and 
              (S46'right = 7) and 
              (S47'right = 7) and 
              (S48'right = 7) and 
              (S49'right = 7) and 
              (S50'right = 7) and 
              (S51.a'right = 7) and 
              (S51.b'right = 7) and 
              (S51.c'right = 7) and 
              (S1'length = 16) and 
              (S2'length = 16) and 
              (S3'length = 16) and 
              (S4'length = 16) and 
              (S5'length = 16) and 
              (S6'length = 16) and 
              (S7'length = 16) and 
              (S8'length = 16) and 
              (S9'length = 16) and 
              (S10'length = 16) and 
              (S11'length = 16) and 
              (S12'length = 16) and 
              (S13'length = 16) and 
              (S14'length = 16) and 
              (S15'length = 16) and 
              (S16'length = 16) and 
              (S17'length = 16) and 
              (S18'length = 16) and 
              (S19'length = 16) and 
              (S20'length = 16) and 
              (S21'length = 16) and 
              (S22.j'length = 7)and 
              (S22.k'length = 4) and 
              (S23.a'length = 16) and 
              (S23.b'length = 16) and 
              (S23.c'length = 16) and 
              (S23.d'length = 16) and 
              (S23.e'length = 16) and 
              (S23.f'length = 16) and 
              (S23.g'length = 16) and 
              (S24.a'length = 16) and 
              (S24.b'length = 16) and 
              (S24.c'length = 16) and 
              (S24.d'length = 16) and 
              (S24.e'length = 16) and 
              (S24.f'length = 16) and 
              (S24.g'length = 16) and 
              (S25'length = 16) and 
              (S26'length = 16) and 
              (S27'length = 16) and 
              (S28'length = 16) and 
              (S29'length = 16) and 
              (S30'length = 16) and 
              (S31'length = 16) and 
              (S32.a'length = 16) and 
              (S32.b'length = 16) and 
              (S32.c'length = 16) and 
              (S32.d'length = 16) and 
              (S32.e'length = 16) and 
              (S32.f'length = 16) and 
              (S32.g'length = 16) and 
              (S34.a'length = 16) and 
              (S34.b'length = 16) and 
              (S34.c'length = 16) and 
              (S34.d'length = 16) and 
              (S34.e'length = 16) and 
              (S34.f'length = 16) and 
              (S34.g'length = 16) and 
              (S36'length = 8) and 
              (S37'length = 16) and 
              (S38'length = 4) and 
              (S39'length = 4) and 
--          (S40'length = 1) and 
              (S41'length = 8) and 
              (S42'length = 8) and 
              (S43'length = 8) and 
              (S44'length = 8) and 
              (S45'length = 8) and 
              (S46'length = 8) and 
              (S48'length = 8) and 
              (S48'length = 8) and 
              (S49'length = 8) and 
              (S50'length = 8) and 
              (S51.a'length = 8) and 
              (S51.b'length = 8) and 
              (S51.c'length = 8) ) 
      report "***FAILED TEST: c01s01b01x01p05n02i00754 - Generic can be used to specify the size of ports."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c01s01b01x01p05n02i00754arch;
