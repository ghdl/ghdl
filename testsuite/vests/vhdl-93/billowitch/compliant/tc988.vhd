
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
-- $Id: tc988.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c06s03b00x00p06n01i00988pkg is
------------------------------------USING ONLY WHITE MATTER---------------------------------
--------------------------------------------------------------------------------------------
---ACCESS TYPE FROM STANDARD PACKAGE

  type boolean_ptr is access boolean ;                                  --simple boolean type   
  type bit_ptr is access bit ;                                          --simple bit type
  type char_ptr is access character;                                    --simple character type
  type severity_level_ptr is access severity_level;                     --simple severity type
  type integer_ptr is access integer;                                   --simple integer type   
  type real_ptr is access real;                                         --simple real type
  type time_ptr is access time;                                         --simple time type
  type natural_ptr is access natural;                                   --simple natural type
  type positive_ptr is access positive;                                 --simple positive type
  type string_ptr is access string;                                     --simple string type
  type bit_vector_ptr is access bit_vector;                             --simple bit_vector type

--------------------------------------------------------------------------------------------

--UNCONSTRAINED ARRAY OF TYPES FROM STANDARD PACKAGE
--Index type is natural
  type boolean_vector is array (natural range <>) of boolean;
  type severity_level_vector is array (natural range <>) of severity_level;
  type integer_vector is array (natural range <>) of integer;
  type real_vector is array (natural range <>) of real;
  type time_vector is array (natural range <>) of time;
  type natural_vector is array (natural range <>) of natural;
  type positive_vector is array (natural range <>) of positive;

---------------------------------------------------------------------------------------------
--CONSTRAINED ARRAY OF TYPES FROM STANDARD PACKAGE
--Index type is natural
  subtype boolean_vector_st is boolean_vector(0 to 15);
  subtype severity_level_vector_st is  severity_level_vector(0 to 15);
  subtype integer_vector_st is  integer_vector(0 to 15);
  subtype real_vector_st is  real_vector(0 to 15);
  subtype time_vector_st is  time_vector(0 to 15);
  subtype natural_vector_st is  natural_vector(0 to 15);
  subtype positive_vector_st is  positive_vector(0 to 15);
  
---------------------------------------------------------------------------------------------
--CONSTRAINED ARRAY OF TYPES FROM STANDARD PACKAGE
--Index type is natural
  type boolean_cons_vector is array (15 downto 0) of boolean;
  type severity_level_cons_vector is array (15 downto 0) of severity_level;
  type integer_cons_vector is array (15 downto 0) of integer;
  type real_cons_vector is array (15 downto 0) of real;
  type time_cons_vector is array (15 downto 0) of time;
  type natural_cons_vector is array (15 downto 0) of natural;
  type positive_cons_vector is array (15 downto 0) of positive;
  
---------------------------------------------------------------------------------------------
  
--CONSTRAINED ARRAY OF ARRAY OF TYPES FROM STANDARD PACKAGE
--Index type is natural
  type boolean_cons_vectorofvector is array (0 to 15) of boolean_cons_vector;
  type severity_level_cons_vectorofvector is array (0 to 15) of severity_level_cons_vector;
  type integer_cons_vectorofvector is array (0 to 15) of integer_cons_vector ;
  type real_cons_vectorofvector is array (0 to 15) of real_cons_vector;
  type time_cons_vectorofvector is array (0 to 15) of time_cons_vector;
  type natural_cons_vectorofvector is array (0 to 15) of natural_cons_vector;
  type positive_cons_vectorofvector is array (0 to 15) of positive_cons_vector;
  
---------------------------------------------------------------------------------------------

--UNCONSTRAINED 2-DIMENSIONAL ARRAY OF TYPES FROM STANDARD PACKAGE
--Index type is natural
  type s2boolean_vector is array (natural range <>,natural range <>) of boolean;
  type s2bit_vector is array (natural range<>,natural range <>) of bit;
  type s2char_vector is array (natural range<>,natural range <>) of character;
  type s2severity_level_vector is array (natural range <>,natural range <>) of severity_level;
  type s2integer_vector is array (natural range <>,natural range <>) of integer;
  type s2real_vector is array (natural range <>,natural range <>) of real;
  type s2time_vector is array (natural range <>,natural range <>) of time;
  type s2natural_vector is array (natural range <>,natural range <>) of natural;
  type s2positive_vector is array (natural range <>,natural range <>) of positive;
  
----------------------------------------------------------------------------------------------
  
--CONSTRAINED 2-DIMENSIONAL ARRAY OF TYPES FROM STANDARD PACKAGE
--Index type is natural
  type column is range 1 to 64;
  type row is range 1 to 1024;
  type s2boolean_cons_vector is array (row,column) of boolean;
  type s2bit_cons_vector is array (row,column) of bit;
  type s2char_cons_vector is array (row,column) of character;
  type s2severity_level_cons_vector is array (row,column) of severity_level;
  type s2integer_cons_vector is array (row,column) of integer;
  type s2real_cons_vector is array (row,column) of real;
  type s2time_cons_vector is array (row,column) of time;
  type s2natural_cons_vector is array (row,column) of natural;
  type s2positive_cons_vector is array (row,column) of positive;
  
-----------------------------------------------------------------------------------------------
--RECORD WITH FIELDS FROM STANDARD PACKAGE
  
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
  
  
-----------------------------------------------------------------------------------------------
--RECORD WITH FIELDS AS UNCONSTRAINT ARRAYS
  
  type record_array_st is record
                            a:boolean_vector_st;
                            b:severity_level_vector_st;
                            c:integer_vector_st;
                            d:real_vector_st;
                            e:time_vector_st;
                            f:natural_vector_st;
                            g:positive_vector_st;
                          end record;
  
-----------------------------------------------------------------------------------------------
  
--RECORD WITH FIELDS AS CONSTRAINT ARRAYS
  
  type record_cons_array is record
                              a:boolean_cons_vector;
                              b:severity_level_cons_vector;
                              c:integer_cons_vector;
                              d:real_cons_vector;
                              e:time_cons_vector;
                              f:natural_cons_vector;
                              g:positive_cons_vector;
                            end record;
  
-----------------------------------------------------------------------------------------------
  
--RECORD WITH FIELDS AS 2-DIMENSIONAL CONSTRAINED ARRAYS
  
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

-----------------------------------------------------------------------------------------------
--RECORD WITH FIELDS AS 2-DIMENSIONAL CONSTRAINED ARRAYS OF ARRAY
  type record_cons_arrayofarray is record
                                     a:boolean_cons_vectorofvector;
                                     b:severity_level_cons_vectorofvector;
                                     c:integer_cons_vectorofvector;
                                     d:real_cons_vectorofvector;
                                     e:time_cons_vectorofvector;
                                     f:natural_cons_vectorofvector;
                                     g:positive_cons_vectorofvector;
                                   end record;
  
-----------------------------------------------------------------------------------------------
  type record_of_ptr is record
                          a:boolean_ptr ;                                   --simple boolean type
                          b:bit_ptr;                                        --simple bit type
                          c:char_ptr;                                       --simple character type
                          e:severity_level_ptr;                        --simple severity type
                          f:integer_ptr;                                    --simple integer type
                          g:  real_ptr ;                                    --simple real type
                          h:time_ptr;                                       --simple time type
                          i: natural_ptr;                                   --simple natural type

                          j:positive_ptr;                                   --simple positive type
                          k: string_ptr;                                    --simple string type
                          l: bit_vector_ptr;                                --simple bit_vector type
                        end record;
  
  
-----------------------------------------------------------------------------------------------
  type record_of_records is record
                              a: record_std_package;
                              c: record_cons_array;
                              e: record_2cons_array;
                              g: record_cons_arrayofarray;
                              h: record_of_ptr;
                              i: record_array_st;
                            end record;
  
-----------------------------------------------------------------------------------------------
--ACCESS TYPES FOR ABOVE
-----------------------------------------------------------------------------------------------
  
  type boolean_vector_ptr is access boolean_vector;
  type severity_level_vector_ptr is access severity_level_vector;
  type integer_vector_ptr is access integer_vector;
  type real_vector_ptr is access real_vector;
  type time_vector_ptr is access time_vector;
  type natural_vector_ptr is access natural_vector;
  type positive_vector_ptr is access positive_vector;
-----------------------------------------------------------------------------------------------
  type boolean_vector_st_ptr is access boolean_vector_st;--(0 to 15);
  type severity_level_vector_st_ptr is access  severity_level_vector_st;--(0 to 15);
  type integer_vector_st_ptr is access  integer_vector_st;--(0 to 15);
  type real_vector_st_ptr is access  real_vector_st;--(0 to 15);
  type time_vector_st_ptr is access  time_vector_st;--(0 to 15);
  type natural_vector_st_ptr is access  natural_vector_st;--(0 to 15);
  type positive_vector_st_ptr is access  positive_vector_st;--(0 to 15);
-----------------------------------------------------------------------------------------------
  type boolean_cons_vector_ptr is access boolean_cons_vector;
  type severity_level_cons_vector_ptr is access severity_level_cons_vector;
  type integer_cons_vector_ptr is access  integer_cons_vector;
  type real_cons_vector_ptr is access real_cons_vector;
  type time_cons_vector_ptr is access time_cons_vector;
  type natural_cons_vector_ptr is access natural_cons_vector;
  type positive_cons_vector_ptr is access  positive_cons_vector;
-----------------------------------------------------------------------------------------------
  type boolean_cons_vectorofvector_ptr is access boolean_cons_vectorofvector;
  type sev_lvl_cons_vecofvec_ptr is access severity_level_cons_vectorofvector;
  type integer_cons_vectorofvector_ptr is access integer_cons_vectorofvector;
  type real_cons_vectorofvector_ptr is access  real_cons_vectorofvector;
  type time_cons_vectorofvector_ptr is access time_cons_vectorofvector;
  type natural_cons_vectorofvector_ptr is access natural_cons_vectorofvector;
  type posi_cons_vecofvec_ptr is access positive_cons_vectorofvector;
-----------------------------------------------------------------------------------------------
  type s2boolean_vector_ptr is access s2boolean_vector;
  type s2bit_vector_ptr is access  s2bit_vector;
  type s2char_vector_ptr is access s2char_vector;
  type s2severity_level_vector_ptr is access s2severity_level_vector;
  type s2integer_vector_ptr is access s2integer_vector;
  type s2real_vector_ptr is access s2real_vector;
  type s2time_vector_ptr is access  s2time_vector;
  type s2positive_vector_ptr is access s2positive_vector;
-----------------------------------------------------------------------------------------------
  type s2boolean_cons_vector_ptr is access s2boolean_cons_vector;
  type s2bit_cons_vector_ptr is access s2bit_cons_vector;
  type s2char_cons_vector_ptr is access  s2char_cons_vector;
  type s2sev_lvl_cons_vec_ptr is access s2severity_level_cons_vector;
  type s2integer_cons_vector_ptr is access  s2integer_cons_vector;
  type s2real_cons_vector_ptr is access  s2real_cons_vector;
  type s2time_cons_vector_ptr is access  s2time_cons_vector;
  type s2natural_cons_vector_ptr is access s2natural_cons_vector;
  type s2positive_cons_vector_ptr is access s2positive_cons_vector;
----------------------------------------------------------------------------------------------
  type record_std_package_ptr is access record_std_package;
  type record_cons_array_ptr is access  record_cons_array;
  type record_2cons_array_ptr is access  record_2cons_array;
  type record_cons_arrayofarray_ptr is access  record_cons_arrayofarray;
  type record_of_ptr_ptr is access record_of_ptr;
  type record_of_records_ptr is access record_of_records;
  type record_array_st_ptr is access record_array_st;
  
-----------------------------------------------------------------------------------------------
-------------------------USING PARTIAL GRAY & PARTIAL WHITE MATTER-----------------------------
  
  
  
  type four_value is ('Z','0','1','X');                                 --enumerated type
  type four_value_map is array(four_value) of boolean;
  subtype binary is four_value range '0' to '1';
  type four_value_vector is array (natural range <>) of four_value;     --unconstraint array of
  type byte is array(0 to 7) of bit;
  subtype word is bit_vector(0 to  15);                                 --constrained array
  function resolution(i:in four_value_vector) return four_value;        --bus resolution
  subtype four_value_state is resolution four_value;                    --function type
  type state_vector is array (natural range <>) of four_value_state;    --unconstraint array of
  constant size :integer := 63;
  type primary_memory is array(0 to size) of word;                      --array of an array
  type primary_memory_module is                                         --record with field
    record                                                                --as an array
      enable:binary;
      memory_number:primary_memory;
    end record;
  type whole_memory is array(0 to size) of primary_memory_module;       --array of a complex record
  type current is range -2147483647 to +2147483647
    units
      nA;
      uA = 1000 nA;
      mA = 1000 uA;
      A  = 1000 mA;
    end units;
  type resistance is range -2147483647 to +2147483647
    units
      uOhm;
      mOhm = 1000 uOhm;
      Ohm = 1000 mOhm;
      KOhm  = 1000 Ohm;
    end units;
  subtype delay is integer range 1 to 10;
  
  type four_value_ptr is access four_value;
  type four_value_map_ptr is access four_value_map;
  type binary_ptr is access binary;
  type four_value_vector_ptr is access four_value_vector;               --ennumerated type
  type byte_ptr is access byte;
  type word_ptr is access word;
  type four_value_state_ptr is access four_value_state;
  type state_vector_ptr is access state_vector;                         --type returned by resolu.
  type primary_memory_ptr is access primary_memory;
  type primary_memory_module_ptr is access primary_memory_module;
  type whole_memory_ptr is access whole_memory;
  type current_ptr is access current;
  type resistance_ptr is access resistance;
  type delay_ptr is access delay;
-------------------------------------------------------------------------------------------
  constant C1 : boolean := true;
  constant C2 : bit := '1';
  constant C3 : character := 's';
  constant C4 : severity_level := note;
  constant C5 : integer := 3;
  constant C6 : real := 3.0;
  constant C7 : time := 3 ns;
  constant C8 : natural := 1;
  constant C9 : positive := 1;
  constant C10 : string := "shishir";
  constant C11 : bit_vector := B"0011";
  constant C12 : boolean_vector := (true,false);
  constant C13 : severity_level_vector := (note,error);
  constant C14 : integer_vector := (1,2,3,4);
  constant C15 : real_vector := (1.0,2.0,3.0,4.0);
  constant C16 : time_vector := (1 ns, 2 ns, 3 ns, 4 ns);
  constant C17 : natural_vector := (1,2,3,4);
  constant C18 : positive_vector := (1,2,3,4);
  constant C19 : boolean_cons_vector := (others => C1);
  constant C20 : severity_level_cons_vector := (others => C4);
  constant C21 : integer_cons_vector := (others => C5);
  constant C22 : real_cons_vector := (others => C6);
  constant C23 : time_cons_vector :=  (others => C7);
  constant C24 : natural_cons_vector :=  (others => C8);
  constant C25 : positive_cons_vector :=  (others => C9);
  
  constant C70 : boolean_vector_st :=(others => C1);
  constant C71 :  severity_level_vector_st:= (others => C4);
  constant C72 :  integer_vector_st:=(others => C5);
  constant C73 :  real_vector_st:=(others => C6);
  constant C74 :  time_vector_st:=(others => C7);
  constant C75 :  natural_vector_st:=(others => C8);
  constant C76 : positive_vector_st:=(others => C9);
  
  
  constant C26 : boolean_cons_vectorofvector := (others => (others => C1));
  constant C27 : severity_level_cons_vectorofvector :=  (others => (others => C4));
  constant C28 : integer_cons_vectorofvector := (others => (others => C5));
  constant C29 : real_cons_vectorofvector := (others => (others => C6));
  constant C30 : time_cons_vectorofvector := (others => (others => C7));
  constant C31 : natural_cons_vectorofvector := (others => (others => C8));
  constant C32 : positive_cons_vectorofvector := (others => (others => C9));
--constant C33 : s2boolean_vector := ((true,true),(false,false));
--constant C34 : s2bit_vector := ((B"0011"),(B"1100"));
--constant C35 : s2char_vector := (('s','h'),('i','s'));
--constant C36 : s2severity_level_vector := ((note,error),(error,note));
--constant C37 : s2integer_vector := ((1,2,3,4),(4,3,2,1));
--constant C38 : s2real_vector := ((1.0,2.0,3.0,4.0),(4.0,3.0,2.0,1.0));
--constant C39 : s2time_vector := ((1 ns, 2 ns, 3 ns, 4 ns),(1 ns, 2 ns, 3 ns, 4 ns));
--constant C40 : s2positive_vector := ((1,2,3,4),(4,3,2,1));
  constant C41 : s2boolean_cons_vector := (others =>(others => C1));
  constant C42 : s2bit_cons_vector := (others => (others => C2));
  constant C43 : s2char_cons_vector := (others =>(others => C3));
  constant C44 : s2severity_level_cons_vector := (others => (others => C4));
  constant C45 : s2integer_cons_vector := (others => (others => C5));
  constant C46 : s2real_cons_vector := (others =>(others => C6));
  constant C47 : s2time_cons_vector := (others =>(others => C7));
  constant C48 : s2natural_cons_vector := (others =>(others => C8));
  constant C49 : s2positive_cons_vector := (others => (others => C9));
  constant C50 : record_std_package := (C1,C2,C3,C4,C5,C6,C7,C8,C9);
  constant C51 : record_cons_array := (C19,C20,C21,C22,C23,C24,C25);
  constant C52 : record_2cons_array := (C41,C42,C43,C44,C45,C46,C47,C48,C49);
  constant C53 : record_cons_arrayofarray := (C26,C27,C28,C29,C30,C31,C32);
--constant C54 : record_of_ptr := (NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);
--constant C54a : record_array_st := (C70,C71,C72,C73,C74,C75,C76);
--constant C55 : record_of_records := (C50,C51,C52,C53,C54,C54a);
  constant C56 : four_value := 'Z';
  constant C57 : four_value_map := (true,true,true,true);
  constant C58 : binary := '0';
  constant C59 : four_value_vector := ('1','0','1','0');
  constant C60 : byte := (others => '0');
  constant C61 : word := (others =>'0' );
  constant C62 : four_value_state := 'Z';
  constant C63 : state_vector := ('Z','Z','Z','Z');
  constant C64 : primary_memory := (others => C61);
  constant C65 : primary_memory_module := ('1',C64);
  constant C66 : whole_memory := (others => C65);
  constant C67 : current := 1 A;
  constant C68 : resistance := 1 Ohm;
  constant C69 : delay := 2;
  
end c06s03b00x00p06n01i00988pkg;

package body c06s03b00x00p06n01i00988pkg is
  function resolution(i:in four_value_vector) return four_value is
    variable temp :four_value := 'Z';
  begin
    return temp;
  end;
end c06s03b00x00p06n01i00988pkg;

use work.c06s03b00x00p06n01i00988pkg.all;

ENTITY c06s03b00x00p06n01i00988ent IS
END c06s03b00x00p06n01i00988ent;

ARCHITECTURE c06s03b00x00p06n01i00988arch OF c06s03b00x00p06n01i00988ent IS

BEGIN
  TESTING: PROCESS
    variable var1 : boolean_ptr := new boolean;
    variable var2 : bit_ptr := new bit;
    variable var3 : char_ptr := new character;
    variable var4 : severity_level_ptr := new severity_level;
    variable var5 : integer_ptr := new integer;
    variable var6 : real_ptr := new real;
    variable var7 : time_ptr := new time;
    variable var8 : natural_ptr := new natural;
    variable var9 : positive_ptr := new positive;
    variable var10 : string_ptr := new string(1 to 7);
    variable var11 : bit_vector_ptr := new bit_vector(0 to 3);
    variable var12 : boolean_vector_ptr := new boolean_vector(0 to 1);
    variable var13 : severity_level_vector_ptr := new severity_level_vector(0 to 1);
    variable var14 : integer_vector_ptr := new integer_vector(0 to 3);
    variable var15 : real_vector_ptr := new real_vector(0 to 3);
    variable var16 : time_vector_ptr := new time_vector(0 to 3);
    variable var17 : natural_vector_ptr := new natural_vector(0 to 3);
    variable var18 : positive_vector_ptr := new positive_vector( 0 to 3);
    variable var19 : boolean_cons_vector_ptr := new boolean_cons_vector;
    variable var20 : severity_level_cons_vector_ptr := new severity_level_cons_vector;
    variable var21 : integer_cons_vector_ptr := new integer_cons_vector;
    variable var22 : real_cons_vector_ptr := new real_cons_vector;
    variable var23 : time_cons_vector_ptr := new time_cons_vector;
    variable var24 : natural_cons_vector_ptr := new natural_cons_vector;
    variable var25 : positive_cons_vector_ptr := new positive_cons_vector;
    variable var26 : boolean_cons_vectorofvector_ptr := new boolean_cons_vectorofvector;
    variable var27 : sev_lvl_cons_vecofvec_ptr := new severity_level_cons_vectorofvector;
    variable var28 : integer_cons_vectorofvector_ptr := new integer_cons_vectorofvector;
    variable var29 : real_cons_vectorofvector_ptr := new real_cons_vectorofvector;
    variable var30 : time_cons_vectorofvector_ptr := new time_cons_vectorofvector;
    variable var31 : natural_cons_vectorofvector_ptr := new natural_cons_vectorofvector;
    variable var32 : posi_cons_vecofvec_ptr := new positive_cons_vectorofvector;
--variable var33 : s2boolean_vector_ptr := new s2boolean_vector;
--variable var34 : s2bit_vector_ptr := new s2bit_vector;
--variable var35 : s2char_vector_ptr := new s2char_vector;
--variable var36 : s2severity_level_vector_ptr := new s2severity_level_vector;
--variable var37 : s2integer_vector_ptr := new s2integer_vector;
--variable var38 : s2real_vector_ptr := new s2real_vector;
--variable var39 : s2time_vector_ptr := new s2time_vector;
--variable var40 : s2positive_vector_ptr := new s2positive_vector;
    variable var41 : s2boolean_cons_vector_ptr := new s2boolean_cons_vector;
    variable var42 : s2bit_cons_vector_ptr := new s2bit_cons_vector;
    variable var43 : s2char_cons_vector_ptr := new s2char_cons_vector;
    variable var44 : s2sev_lvl_cons_vec_ptr := new s2severity_level_cons_vector;
    variable var45 : s2integer_cons_vector_ptr := new s2integer_cons_vector;
    variable var46 : s2real_cons_vector_ptr := new s2real_cons_vector;
    variable var47 : s2time_cons_vector_ptr := new s2time_cons_vector;
    variable var48 : s2natural_cons_vector_ptr := new s2natural_cons_vector;
    variable var49 : s2positive_cons_vector_ptr := new s2positive_cons_vector;
    variable var50 : record_std_package_ptr := new record_std_package;
    variable var51 : record_cons_array_ptr := new record_cons_array;
    variable var52 : record_2cons_array_ptr := new record_2cons_array;
    variable var53 : record_cons_arrayofarray_ptr := new record_cons_arrayofarray;
    variable var54 : record_of_ptr_ptr := new record_of_ptr;
    variable var55 : record_of_records_ptr := new record_of_records;
    variable var56 : four_value_ptr := new four_value;
    variable var57 : four_value_map_ptr := new four_value_map;
    variable var58 : binary_ptr := new binary;
    variable var59 : four_value_vector_ptr := new four_value_vector(0 to 3);
    variable var60 : byte_ptr := new byte;
    variable var61 : word_ptr := new word;
    variable var62 : four_value_state_ptr := new four_value_state;
    variable var63 : state_vector_ptr := new state_vector(0 to 3);
    variable var64 : primary_memory_ptr := new primary_memory;
    variable var65 : primary_memory_module_ptr := new primary_memory_module;
    variable var66 : whole_memory_ptr := new whole_memory;
    variable var67 : current_ptr := new current;
    variable var68 : resistance_ptr := new resistance;
    variable var69 : delay_ptr := new delay;
    variable var70 : boolean_vector_st_ptr := new boolean_vector_st;
    variable var71 : severity_level_vector_st_ptr := new severity_level_vector_st;
    variable var72 : integer_vector_st_ptr := new integer_vector_st;
    variable var73 : real_vector_st_ptr := new real_vector_st;
    variable var74 : time_vector_st_ptr := new time_vector_st;
    variable var75 : natural_vector_st_ptr := new natural_vector_st;
    variable var76 : positive_vector_st_ptr := new positive_vector_st;
    variable var54a : record_array_st_ptr := new record_array_st;
    
    variable vari1 : boolean := C1;
    variable vari2 : bit := C2;
    variable vari3 : character := C3;
    variable vari4 : severity_level := C4;
    variable vari5 : integer := C5;
    variable vari6 : real := C6;
    variable vari7 : time := C7;
    variable vari8 : natural := C8;
    variable vari9 : positive := C9;
    variable vari10 : string(1 to 7) := C10;
    variable vari11 : bit_vector(0 to 3):= C11;
    variable vari12 : boolean_vector(0 to 1):= C12;
    variable vari13 : severity_level_vector(0 to 1) := C13;
    variable vari14 : integer_vector(0 to 3) := C14;
    variable vari15 : real_vector(0 to 3):= C15;
    variable vari16 : time_vector(0 to 3):= C16;
    variable vari17 : natural_vector(0 to 3):= C17;
    variable vari18 : positive_vector(0 to 3):= C18;
    variable vari19 : boolean_cons_vector := C19;
    variable vari20 : severity_level_cons_vector := C20;
    variable vari21 : integer_cons_vector := C21;
    variable vari22 : real_cons_vector := C22;
    variable vari23 : time_cons_vector := C23;
    variable vari24 : natural_cons_vector := C24;
    variable vari25 : positive_cons_vector := C25;
    variable vari26 : boolean_cons_vectorofvector := C26;
    variable vari27 : severity_level_cons_vectorofvector := C27;
    variable vari28 : integer_cons_vectorofvector := C28;
    variable vari29 : real_cons_vectorofvector := C29;
    variable vari30 : time_cons_vectorofvector := C30;
    variable vari31 : natural_cons_vectorofvector := C31;
    variable vari32 : positive_cons_vectorofvector := C32;
--variable vari33 : s2boolean_vector := C33;
--variable vari34 : s2bit_vector := C34;
--variable vari35 : s2char_vector := C35;
--variable vari36 : s2severity_level_vector := C36;
--variable vari37 : s2integer_vector := C37;
--variable vari38 : s2real_vector := C38;
--variable vari39 : s2time_vector := C39;
--variable vari40 : s2positive_vector := C40;
    variable vari41 : s2boolean_cons_vector := C41;
    variable vari42 : s2bit_cons_vector := C42;
    variable vari43 : s2char_cons_vector := C43;
    variable vari44 : s2severity_level_cons_vector := C44;
    variable vari45 : s2integer_cons_vector := C45;
    variable vari46 : s2real_cons_vector := C46;
    variable vari47 : s2time_cons_vector := C47;
    variable vari48 : s2natural_cons_vector := C48;
    variable vari49 : s2positive_cons_vector := C49;
    variable vari50 : record_std_package := C50;
    variable vari51 : record_cons_array := C51;
    variable vari52 : record_2cons_array := C52;
    variable vari53 : record_cons_arrayofarray := C53;
--variable vari54 : record_of_ptr := C54;
--variable vari55 : record_of_records := C55;
    variable vari56 : four_value := C56;
    variable vari57 : four_value_map := C57;
    variable vari58 : binary := C58;
    variable vari59 : four_value_vector(0 to 3):= C59;
    variable vari60 : byte := C60;
    variable vari61 : word := C61;
    variable vari62 : four_value_state := C62;
    variable vari63 : state_vector(0 to 3):= C63;
    variable vari64 : primary_memory := C64;
    variable vari65 : primary_memory_module := C65;
    variable vari66 : whole_memory := C66;
    variable vari67 : current := C67;
    variable vari68 : resistance := C68;
    variable vari69 : delay := C69;
    variable vari70 : boolean_vector_st := C70;
    variable vari71 : severity_level_vector_st := C71;
    variable vari72 : integer_vector_st := C72;
    variable vari73 : real_vector_st := C73;
    variable vari74 : time_vector_st := C74;
    variable vari75 : natural_vector_st := C75;
    variable vari76 : positive_vector_st := C76;
--variable vari54a : record_array_st := C54a;

  BEGIN

    var1.all := vari1;
    var2.all := vari2;
    var3.all := vari3;
    var4.all := vari4;
    var5.all := vari5;
    var6.all := vari6;
    var7.all := vari7;
    var8.all := vari8;
    var9.all := vari9;
    var10.all := vari10;
    var11.all := vari11;
    var12.all := vari12;
    var13.all := vari13;
    var14.all := vari14;
    var15.all := vari15;
    var16.all := vari16;
    var17.all := vari17;
    var18.all := vari18;
    var19.all := vari19;
    var20.all := vari20;
    var21.all := vari21;
    var22.all := vari22;
    var23.all := vari23;
    var24.all := vari24;
    var25.all := vari25;
    var26.all := vari26;
    var27.all := vari27;
    var28.all := vari28;
    var29.all := vari29;
    var30.all := vari30;
    var31.all := vari31;
    var32.all := vari32;
--var33.all := vari33;
--var34.all := vari34;
--var35.all := vari35;
--var36.all := vari36;
--var37.all := vari37;
--var38.all := vari38;
--var39.all := vari39;
--var40.all := vari40;
    var41.all := vari41;
    var42.all := vari42;
    var43.all := vari43;
    var44.all := vari44;
    var45.all := vari45;
    var46.all := vari46;
    var47.all := vari47;
    var48.all := vari48;
    var49.all := vari49;
    var50.all := vari50;
    var51.all := vari51;
    var52.all := vari52;
    var53.all := vari53;
--var54.all := vari54;
--var55.all := vari55;
    var56.all := vari56;
    var57.all := vari57;
    var58.all := vari58;
    var59.all := vari59;
    var60.all := vari60;
    var61.all := vari61;
    var62.all := vari62;
    var63.all := vari63;
    var64.all := vari64;
    var65.all := vari65;
    var66.all := vari66;
    var67.all := vari67;
    var68.all := vari68;
    var69.all := vari69;
    var70.all := vari70;
    var71.all := vari71;
    var72.all := vari72;
    var73.all := vari73;
    var74.all := vari74;
    var75.all := vari75;
    var76.all := vari76;
--var54a.all := vari54a;
    
    
    
    
    ASSERT var1.all = C1 REPORT "Improper Assignment of var1" SEVERITY FAILURE;
    ASSERT var2.all = C2 REPORT "Improper Assignment of var2" SEVERITY FAILURE;
    ASSERT var3.all = C3 REPORT "Improper Assignment of var3" SEVERITY FAILURE;
    ASSERT var4.all = C4 REPORT "Improper Assignment of var4" SEVERITY FAILURE;
    ASSERT var5.all = C5 REPORT "Improper Assignment of var5" SEVERITY FAILURE;
    ASSERT var6.all = C6 REPORT "Improper Assignment of var6" SEVERITY FAILURE;
    ASSERT var7.all = C7 REPORT "Improper Assignment of var7" SEVERITY FAILURE;
    ASSERT var8.all = C8 REPORT "Improper Assignment of var8" SEVERITY FAILURE;
    ASSERT var9.all = C9 REPORT "Improper Assignment of var9" SEVERITY FAILURE;
    ASSERT var10.all = C10 REPORT "Improper Assignment of var10" SEVERITY FAILURE;
    ASSERT var11.all = C11 REPORT "Improper Assignment of var11" SEVERITY FAILURE;
    ASSERT var12.all = C12 REPORT "Improper Assignment of var12" SEVERITY FAILURE;
    ASSERT var13.all = C13 REPORT "Improper Assignment of var13" SEVERITY FAILURE;
    ASSERT var14.all = C14 REPORT "Improper Assignment of var14" SEVERITY FAILURE;
    ASSERT var15.all = C15 REPORT "Improper Assignment of var15" SEVERITY FAILURE;
    ASSERT var16.all = C16 REPORT "Improper Assignment of var16" SEVERITY FAILURE;
    ASSERT var17.all = C17 REPORT "Improper Assignment of var17" SEVERITY FAILURE;
    ASSERT var18.all = C18 REPORT "Improper Assignment of var18" SEVERITY FAILURE;
    ASSERT var19.all = C19 REPORT "Improper Assignment of var19" SEVERITY FAILURE;
    ASSERT var20.all = C20 REPORT "Improper Assignment of var20" SEVERITY FAILURE;
    ASSERT var21.all = C21 REPORT "Improper Assignment of var21" SEVERITY FAILURE;
    ASSERT var22.all = C22 REPORT "Improper Assignment of var22" SEVERITY FAILURE;
    ASSERT var23.all = C23 REPORT "Improper Assignment of var23" SEVERITY FAILURE;
    ASSERT var24.all = C24 REPORT "Improper Assignment of var24" SEVERITY FAILURE;
    ASSERT var25.all = C25 REPORT "Improper Assignment of var25" SEVERITY FAILURE;
    ASSERT var26.all = C26 REPORT "Improper Assignment of var26" SEVERITY FAILURE;
    ASSERT var27.all = C27 REPORT "Improper Assignment of var27" SEVERITY FAILURE;
    ASSERT var28.all = C28 REPORT "Improper Assignment of var28" SEVERITY FAILURE;
    ASSERT var29.all = C29 REPORT "Improper Assignment of var29" SEVERITY FAILURE;
    ASSERT var30.all = C30 REPORT "Improper Assignment of var30" SEVERITY FAILURE;
    ASSERT var31.all = C31 REPORT "Improper Assignment of var31" SEVERITY FAILURE;
    ASSERT var32.all = C32 REPORT "Improper Assignment of var32" SEVERITY FAILURE;
--ASSERT var33.all = C33 REPORT "Improper Assignment of var33" SEVERITY FAILURE;
--ASSERT var34.all = C34 REPORT "Improper Assignment of var34" SEVERITY FAILURE;
--ASSERT var35.all = C35 REPORT "Improper Assignment of var35" SEVERITY FAILURE;
--ASSERT var36.all = C36 REPORT "Improper Assignment of var36" SEVERITY FAILURE;
--ASSERT var37.all = C37 REPORT "Improper Assignment of var37" SEVERITY FAILURE;
--ASSERT var38.all = C38 REPORT "Improper Assignment of var38" SEVERITY FAILURE;
--ASSERT var39.all = C39 REPORT "Improper Assignment of var39" SEVERITY FAILURE;
--ASSERT var40.all = C40 REPORT "Improper Assignment of var40" SEVERITY FAILURE;
    ASSERT var41.all = C41 REPORT "Improper Assignment of var41" SEVERITY FAILURE;
    ASSERT var42.all = C42 REPORT "Improper Assignment of var42" SEVERITY FAILURE;
    ASSERT var43.all = C43 REPORT "Improper Assignment of var43" SEVERITY FAILURE;
    ASSERT var44.all = C44 REPORT "Improper Assignment of var44" SEVERITY FAILURE;
    ASSERT var45.all = C45 REPORT "Improper Assignment of var45" SEVERITY FAILURE;
    ASSERT var46.all = C46 REPORT "Improper Assignment of var46" SEVERITY FAILURE;
    ASSERT var47.all = C47 REPORT "Improper Assignment of var47" SEVERITY FAILURE;
    ASSERT var48.all = C48 REPORT "Improper Assignment of var48" SEVERITY FAILURE;
    ASSERT var49.all = C49 REPORT "Improper Assignment of var49" SEVERITY FAILURE;
    ASSERT var50.all = C50 REPORT "Improper Assignment of var50" SEVERITY FAILURE;
    ASSERT var51.all = C51 REPORT "Improper Assignment of var51" SEVERITY FAILURE;
    ASSERT var52.all = C52 REPORT "Improper Assignment of var52" SEVERITY FAILURE;
    ASSERT var53.all = C53 REPORT "Improper Assignment of var53" SEVERITY FAILURE;
--ASSERT var54.all = C54 REPORT "Improper Assignment of var54" SEVERITY FAILURE;
--ASSERT var54a.all = C54a REPORT "Improper Assignment of var54a" SEVERITY FAILURE;
--ASSERT var55.all = C55 REPORT "Improper Assignment of var55" SEVERITY FAILURE;
    ASSERT var56.all = C56 REPORT "Improper Assignment of var56" SEVERITY FAILURE;
    ASSERT var57.all = C57 REPORT "Improper Assignment of var57" SEVERITY FAILURE;
    ASSERT var58.all = C58 REPORT "Improper Assignment of var58" SEVERITY FAILURE;
    ASSERT var59.all = C59 REPORT "Improper Assignment of var59" SEVERITY FAILURE;
    ASSERT var60.all = C60 REPORT "Improper Assignment of var60" SEVERITY FAILURE;
    ASSERT var61.all = C61 REPORT "Improper Assignment of var61" SEVERITY FAILURE;
    ASSERT var62.all = C62 REPORT "Improper Assignment of var62" SEVERITY FAILURE;
    ASSERT var63.all = C63 REPORT "Improper Assignment of var63" SEVERITY FAILURE;
    ASSERT var64.all = C64 REPORT "Improper Assignment of var64" SEVERITY FAILURE;
    ASSERT var65.all = C65 REPORT "Improper Assignment of var65" SEVERITY FAILURE;
    ASSERT var66.all = C66 REPORT "Improper Assignment of var66" SEVERITY FAILURE;
    ASSERT var67.all = C67 REPORT "Improper Assignment of var67" SEVERITY FAILURE;
    ASSERT var68.all = C68 REPORT "Improper Assignment of var68" SEVERITY FAILURE;
    ASSERT var69.all = C69 REPORT "Improper Assignment of var69" SEVERITY FAILURE;
    ASSERT var70.all = C70 REPORT "Improper Assignment of var70" SEVERITY FAILURE;
    ASSERT var71.all = C71 REPORT "Improper Assignment of var71" SEVERITY FAILURE;
    ASSERT var72.all = C72 REPORT "Improper Assignment of var72" SEVERITY FAILURE;
    ASSERT var73.all = C73 REPORT "Improper Assignment of var73" SEVERITY FAILURE;
    ASSERT var74.all = C74 REPORT "Improper Assignment of var74" SEVERITY FAILURE;
    ASSERT var75.all = C75 REPORT "Improper Assignment of var75" SEVERITY FAILURE;
    ASSERT var76.all = C76 REPORT "Improper Assignment of var76" SEVERITY FAILURE;

    assert NOT(   var1.all   = C1    and 
                  var2.all   = C2    and 
                  var3.all   = C3    and 
                  var4.all   = C4    and 
                  var5.all   = C5    and 
                  var6.all   = C6    and 
                  var7.all   = C7    and 
                  var8.all   = C8    and 
                  var9.all   = C9    and 
                  var10.all   = C10    and 
                  var11.all   = C11    and 
                  var12.all   = C12    and 
                  var13.all   = C13    and 
                  var14.all   = C14    and 
                  var15.all   = C15    and 
                  var16.all   = C16    and 
                  var17.all   = C17    and 
                  var18.all   = C18    and 
                  var19.all   = C19    and 
                  var20.all   = C20    and 
                  var21.all   = C21    and 
                  var22.all   = C22    and 
                  var23.all   = C23    and 
                  var24.all   = C24    and 
                  var25.all   = C25    and 
                  var26.all   = C26    and 
                  var27.all   = C27    and 
                  var28.all   = C28    and 
                  var29.all   = C29    and 
                  var30.all   = C30    and 
                  var31.all   = C31    and 
                  var32.all   = C32    and 
--      var33.all   = C33    and 
--      var34.all   = C34    and 
--      var35.all   = C35    and 
--      var36.all   = C36    and 
--      var37.all   = C37    and 
--      var38.all   = C38    and 
--      var39.all   = C39    and 
--      var40.all   = C40    and 
                  var41.all   = C41    and 
                  var42.all   = C42    and 
                  var43.all   = C43    and 
                  var44.all   = C44    and 
                  var45.all   = C45    and 
                  var46.all   = C46    and 
                  var47.all   = C47    and 
                  var48.all   = C48    and 
                  var49.all   = C49    and 
                  var50.all   = C50    and 
                  var51.all   = C51    and 
                  var52.all   = C52    and 
                  var53.all   = C53    and 
--      var54.all   = C54    and 
--      var54a.all   = C54a    and 
--      var55.all   = C55    and 
                  var56.all   = C56    and 
                  var57.all   = C57    and 
                  var58.all   = C58    and 
                  var59.all   = C59    and 
                  var60.all   = C60    and 
                  var61.all   = C61    and 
                  var62.all   = C62    and 
                  var63.all   = C63    and 
                  var64.all   = C64    and 
                  var65.all   = C65    and 
                  var66.all   = C66    and 
                  var67.all   = C67    and 
                  var68.all   = C68    and 
                  var69.all   = C69    and 
                  var70.all   = C70    and 
                  var71.all   = C71    and 
                  var72.all   = C72    and 
                  var73.all   = C73    and 
                  var74.all   = C74    and 
                  var75.all   = C75    and 
                  var76.all   = C76    ) 
      report "***PASSED TEST: c06s03b00x00p06n01i00988" 
      severity NOTE;
    assert (   var1.all   = C1    and 
               var2.all   = C2    and 
               var3.all   = C3    and 
               var4.all   = C4    and 
               var5.all   = C5    and 
               var6.all   = C6    and 
               var7.all   = C7    and 
               var8.all   = C8    and 
               var9.all   = C9    and 
               var10.all   = C10    and 
               var11.all   = C11    and 
               var12.all   = C12    and 
               var13.all   = C13    and 
               var14.all   = C14    and 
               var15.all   = C15    and 
               var16.all   = C16    and 
               var17.all   = C17    and 
               var18.all   = C18    and 
               var19.all   = C19    and 
               var20.all   = C20    and 
               var21.all   = C21    and 
               var22.all   = C22    and 
               var23.all   = C23    and 
               var24.all   = C24    and 
               var25.all   = C25    and 
               var26.all   = C26    and 
               var27.all   = C27    and 
               var28.all   = C28    and 
               var29.all   = C29    and 
               var30.all   = C30    and 
               var31.all   = C31    and 
               var32.all   = C32    and 
--      var33.all   = C33    and 
--      var34.all   = C34    and 
--      var35.all   = C35    and 
--      var36.all   = C36    and 
--      var37.all   = C37    and 
--      var38.all   = C38    and 
--      var39.all   = C39    and 
--      var40.all   = C40    and 
               var41.all   = C41    and 
               var42.all   = C42    and 
               var43.all   = C43    and 
               var44.all   = C44    and 
               var45.all   = C45    and 
               var46.all   = C46    and 
               var47.all   = C47    and 
               var48.all   = C48    and 
               var49.all   = C49    and 
               var50.all   = C50    and 
               var51.all   = C51    and 
               var52.all   = C52    and 
               var53.all   = C53    and 
--      var54.all   = C54    and 
--      var54a.all   = C54a    and 
--      var55.all   = C55    and 
               var56.all   = C56    and 
               var57.all   = C57    and 
               var58.all   = C58    and 
               var59.all   = C59    and 
               var60.all   = C60    and 
               var61.all   = C61    and 
               var62.all   = C62    and 
               var63.all   = C63    and 
               var64.all   = C64    and 
               var65.all   = C65    and 
               var66.all   = C66    and 
               var67.all   = C67    and 
               var68.all   = C68    and 
               var69.all   = C69    and 
               var70.all   = C70    and 
               var71.all   = C71    and 
               var72.all   = C72    and 
               var73.all   = C73    and 
               var74.all   = C74    and 
               var75.all   = C75    and 
               var76.all   = C76    ) 
      report "***FAILED TEST: c06s03b00x00p06n01i00988 - Prefix of a selected name used to denote an object designated by an access value should be an access type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s03b00x00p06n01i00988arch;
