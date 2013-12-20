
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
-- $Id: tc987.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c06s03b00x00p06n01i00987pkg is
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
  
end c06s03b00x00p06n01i00987pkg;

package body c06s03b00x00p06n01i00987pkg is
  function resolution(i:in four_value_vector) return four_value is
    variable temp :four_value := 'Z';
  begin
    return temp;
  end;
end c06s03b00x00p06n01i00987pkg;

use work.c06s03b00x00p06n01i00987pkg.all;

ENTITY c06s03b00x00p06n01i00987ent IS
END c06s03b00x00p06n01i00987ent;

ARCHITECTURE c06s03b00x00p06n01i00987arch OF c06s03b00x00p06n01i00987ent IS

BEGIN
  TESTING: PROCESS
    variable var1 : boolean_ptr ;
    variable var2 : bit_ptr ;
    variable var3 : char_ptr ;
    variable var4 : severity_level_ptr ;
    variable var5 : integer_ptr ;
    variable var6 : real_ptr ;
    variable var7 : time_ptr ;
    variable var8 : natural_ptr ;
    variable var9 : positive_ptr ;
    variable var10 : string_ptr ;
    variable var11 : bit_vector_ptr ;
    variable var12 : boolean_vector_ptr ;
    variable var13 : severity_level_vector_ptr ;
    variable var14 : integer_vector_ptr ;
    variable var15 : real_vector_ptr ;
    variable var16 : time_vector_ptr ;
    variable var17 : natural_vector_ptr ;
    variable var18 : positive_vector_ptr ;
    variable var19 : boolean_cons_vector_ptr ;
    variable var20 : severity_level_cons_vector_ptr ;
    variable var21 : integer_cons_vector_ptr ;
    variable var22 : real_cons_vector_ptr ;
    variable var23 : time_cons_vector_ptr ;
    variable var24 : natural_cons_vector_ptr ;
    variable var25 : positive_cons_vector_ptr ;
    variable var26 : boolean_cons_vectorofvector_ptr ;
    variable var27 : sev_lvl_cons_vecofvec_ptr ;
    variable var28 : integer_cons_vectorofvector_ptr ;
    variable var29 : real_cons_vectorofvector_ptr ;
    variable var30 : time_cons_vectorofvector_ptr ;
    variable var31 : natural_cons_vectorofvector_ptr ;
    variable var32 : posi_cons_vecofvec_ptr ;
-- variable var33 : s2boolean_vector_ptr ;
-- variable var34 : s2bit_vector_ptr ;
-- variable var35 : s2char_vector_ptr ;
-- variable var36 : s2severity_level_vector_ptr ;
-- variable var37 : s2integer_vector_ptr ;
-- variable var38 : s2real_vector_ptr ;
-- variable var39 : s2time_vector_ptr ;
-- variable var40 : s2positive_vector_ptr ;
    variable var41 : s2boolean_cons_vector_ptr ;
    variable var42 : s2bit_cons_vector_ptr ;
    variable var43 : s2char_cons_vector_ptr ;
    variable var44 : s2sev_lvl_cons_vec_ptr ;
    variable var45 : s2integer_cons_vector_ptr ;
    variable var46 : s2real_cons_vector_ptr ;
    variable var47 : s2time_cons_vector_ptr ;
    variable var48 : s2natural_cons_vector_ptr ;
    variable var49 : s2positive_cons_vector_ptr ;
    variable var50 : record_std_package_ptr ;
    variable var51 : record_cons_array_ptr ;
    variable var52 : record_2cons_array_ptr ;
    variable var53 : record_cons_arrayofarray_ptr ;
    variable var54 : record_of_ptr_ptr ;
    variable var54a : record_array_st_ptr;
    variable var55 : record_of_records_ptr ;
    variable var56 : four_value_ptr ;
    variable var57 : four_value_map_ptr ;
    variable var58 : binary_ptr ;
    variable var59 : four_value_vector_ptr ;
    variable var60 : byte_ptr ;
    variable var61 : word_ptr ;
    variable var62 : four_value_state_ptr ;
    variable var63 : state_vector_ptr ;
    variable var64 : primary_memory_ptr ;
    variable var65 : primary_memory_module_ptr;
    variable var66 : whole_memory_ptr ;
    variable var67 : current_ptr ;
    variable var68 : resistance_ptr ;
    variable var69 : delay_ptr ;
    variable var70 : boolean_vector_st_ptr;
    variable var71 :  severity_level_vector_st_ptr;
    variable var72 :  integer_vector_st_ptr;
    variable var73 :  real_vector_st_ptr;
    variable var74 :  time_vector_st_ptr;
    variable var75 :  natural_vector_st_ptr;
    variable var76 : positive_vector_st_ptr;
    
    variable vari1 : boolean;
    variable vari2 : bit;
    variable vari3 : character;
    variable vari4 : severity_level;
    variable vari5 : integer;
    variable vari6 : real;
    variable vari7 : time;
    variable vari8 : natural;
    variable vari9 : positive;
    variable vari10 : string(1 to 7);
    variable vari11 : bit_vector(0 to 3);
    variable vari12 : boolean_vector(0 to 1);
    variable vari13 : severity_level_vector(0 to 1);
    variable vari14 : integer_vector(0 to 3);
    variable vari15 : real_vector(0 to 3);
    variable vari16 : time_vector(0 to 3);
    variable vari17 : natural_vector(0 to 3);
    variable vari18 : positive_vector(0 to 3);
    variable vari19 : boolean_cons_vector;
    variable vari20 : severity_level_cons_vector;
    variable vari21 : integer_cons_vector;
    variable vari22 : real_cons_vector;
    variable vari23 : time_cons_vector;
    variable vari24 : natural_cons_vector;
    variable vari25 : positive_cons_vector;
    variable vari26 : boolean_cons_vectorofvector;
    variable vari27 : severity_level_cons_vectorofvector;
    variable vari28 : integer_cons_vectorofvector;
    variable vari29 : real_cons_vectorofvector;
    variable vari30 : time_cons_vectorofvector;
    variable vari31 : natural_cons_vectorofvector;
    variable vari32 : positive_cons_vectorofvector;
--variable vari33 : s2boolean_vector;
--variable vari34 : s2bit_vector;
--variable vari35 : s2char_vector;
--variable vari36 : s2severity_level_vector;
--variable vari37 : s2integer_vector;
--variable vari38 : s2real_vector;
--variable vari39 : s2time_vector;
--variable vari40 : s2positive_vector;
    variable vari41 : s2boolean_cons_vector;
    variable vari42 : s2bit_cons_vector;
    variable vari43 : s2char_cons_vector;
    variable vari44 : s2severity_level_cons_vector;
    variable vari45 : s2integer_cons_vector;
    variable vari46 : s2real_cons_vector;
    variable vari47 : s2time_cons_vector;
    variable vari48 : s2natural_cons_vector;
    variable vari49 : s2positive_cons_vector;
    variable vari50 : record_std_package;
    variable vari51 : record_cons_array;
    variable vari52 : record_2cons_array;
    variable vari53 : record_cons_arrayofarray;
    variable vari54 : record_of_ptr;
    variable vari55 : record_of_records;
    variable vari56 : four_value;
    variable vari57 : four_value_map;
    variable vari58 : binary;
    variable vari59 : four_value_vector(0 to 3);
    variable vari60 : byte;
    variable vari61 : word;
    variable vari62 : four_value_state;
    variable vari63 : state_vector(0 to 3);
    variable vari64 : primary_memory;
    variable vari65 : primary_memory_module;
    variable vari66 : whole_memory;
    variable vari67 : current;
    variable vari68 : resistance;
    variable vari69 : delay;
    variable vari70 : boolean_vector_st;
    variable vari71 : severity_level_vector_st;
    variable vari72 : integer_vector_st;
    variable vari73 : real_vector_st;
    variable vari74 : time_vector_st;
    variable vari75 : natural_vector_st;
    variable vari76 : positive_vector_st;
    variable vari54a : record_array_st;

  BEGIN
    var1 := NEW boolean '(C1);
    var2 := NEW bit '(C2);
    var3 := NEW character '(C3);
    var4 := NEW severity_level '(C4);
    var5 := NEW integer '(C5);
    var6 := NEW real '(C6);
    var7 := NEW time '(C7);
    var8 := NEW natural '(C8);
    var9 := NEW positive '(C9);
    var10 := NEW string '(C10);
    var11 := NEW bit_vector '(C11);
    var12 := NEW boolean_vector '(C12);
    var13 := NEW severity_level_vector '(C13);
    var14 := NEW integer_vector '(C14);
    var15 := NEW real_vector '(C15);
    var16 := NEW time_vector '(C16);
    var17 := NEW natural_vector '(C17);
    var18 := NEW positive_vector '(C18);
    var19 := NEW boolean_cons_vector '(C19);
    var20 := NEW severity_level_cons_vector '(C20);
    var21 := NEW integer_cons_vector '(C21);
    var22 := NEW real_cons_vector '(C22);
    var23 := NEW time_cons_vector '(C23);
    var24 := NEW natural_cons_vector '(C24);
    var25 := NEW positive_cons_vector '(C25);
    var26 := NEW boolean_cons_vectorofvector '(C26);
    var27 := NEW severity_level_cons_vectorofvector '(C27);
    var28 := NEW integer_cons_vectorofvector '(C28);
    var29 := NEW real_cons_vectorofvector '(C29);
    var30 := NEW time_cons_vectorofvector '(C30);
    var31 := NEW natural_cons_vectorofvector '(C31);
    var32 := NEW positive_cons_vectorofvector '(C32);
--var33 := NEW s2boolean_vector '(C33);
--var34 := NEW s2bit_vector '(C34);
--var35 := NEW s2char_vector '(C35);
--var36 := NEW s2severity_level_vector '(C36);
--var37 := NEW s2integer_vector '(C37);
--var38 := NEW s2real_vector '(C38);
--var39 := NEW s2time_vector '(C39);
--var40 := NEW s2positive_vector '(C40);
    var41 := NEW s2boolean_cons_vector '(C41);
    var42 := NEW s2bit_cons_vector '(C42);
    var43 := NEW s2char_cons_vector '(C43);
    var44 := NEW s2severity_level_cons_vector '(C44);
    var45 := NEW s2integer_cons_vector '(C45);
    var46 := NEW s2real_cons_vector '(C46);
    var47 := NEW s2time_cons_vector '(C47);
    var48 := NEW s2natural_cons_vector '(C48);
    var49 := NEW s2positive_cons_vector '(C49);
    var50 := NEW record_std_package '(C50);
    var51 := NEW record_cons_array '(C51);
    var52 := NEW record_2cons_array '(C52);
    var53 := NEW record_cons_arrayofarray '(C53);
--var54 := NEW record_of_ptr '(C54);
--var54a := NEW record_array_st '(C54a);
--var55 := NEW record_of_records '(C55);
    var56 := NEW four_value '(C56);
    var57 := NEW four_value_map '(C57);
    var58 := NEW binary '(C58);
    var59 := NEW four_value_vector '(C59);
    var60 := NEW byte '(C60);
    var61 := NEW word '(C61);
    var62 := NEW four_value_state '(C62);
    var63 := NEW state_vector '(C63);
    var64 := NEW primary_memory '(C64);
    var65 := NEW primary_memory_module '(C65);
    var66 := NEW whole_memory '(C66);
    var67 := NEW current '(C67);
    var68 := NEW resistance '(C68);
    var69 := NEW delay '(C69);
    var70 := NEW boolean_vector_st '(C70);
    var71 := NEW severity_level_vector_st '(C71);
    var72 := NEW integer_vector_st '(C72);
    var73 := NEW real_vector_st '(C73);
    var74 := NEW time_vector_st '(C74);
    var75 := NEW natural_vector_st '(C75);
    var76 := NEW positive_vector_st '(C76);
    
    vari1 := var1.all;
    vari2 := var2.all;
    vari3 := var3.all;
    vari4 := var4.all;
    vari5 := var5.all;
    vari6 := var6.all;
    vari7 := var7.all;
    vari8 := var8.all;
    vari9 := var9.all;
    vari10 := var10.all;
    vari11 := var11.all;
    vari12 := var12.all;
    vari13 := var13.all;
    vari14 := var14.all;
    vari15 := var15.all;
    vari16 := var16.all;
    vari17 := var17.all;
    vari18 := var18.all;
    vari19 := var19.all;
    vari20 := var20.all;
    vari21 := var21.all;
    vari22 := var22.all;
    vari23 := var23.all;
    vari24 := var24.all;
    vari25 := var25.all;
    vari26 := var26.all;
    vari27 := var27.all;
    vari28 := var28.all;
    vari29 := var29.all;
    vari30 := var30.all;
    vari31 := var31.all;
    vari32 := var32.all;
--vari33 := var33.all;
--vari34 := var34.all;
--vari35 := var35.all;
--vari36 := var36.all;
--vari37 := var37.all;
--vari38 := var38.all;
--vari39 := var39.all;
--vari40 := var40.all;
    vari41 := var41.all;
    vari42 := var42.all;
    vari43 := var43.all;
    vari44 := var44.all;
    vari45 := var45.all;
    vari46 := var46.all;
    vari47 := var47.all;
    vari48 := var48.all;
    vari49 := var49.all;
    vari50 := var50.all;
    vari51 := var51.all;
    vari52 := var52.all;
    vari53 := var53.all;
--vari54 := var54.all;
--vari55 := var55.all;
    vari56 := var56.all;
    vari57 := var57.all;
    vari58 := var58.all;
    vari59 := var59.all;
    vari60 := var60.all;
    vari61 := var61.all;
    vari62 := var62.all;
    vari63 := var63.all;
    vari64 := var64.all;
    vari65 := var65.all;
    vari66 := var66.all;
    vari67 := var67.all;
    vari68 := var68.all;
    vari69 := var69.all;
    vari70 := var70.all;
    vari71 := var71.all;
    vari72 := var72.all;
    vari73 := var73.all;
    vari74 := var74.all;
    vari75 := var75.all;
    vari76 := var76.all;
--vari54a := var54a.all;

    ASSERT vari1= C1    report "Improper Assignment of vari1" SEVERITY FAILURE;
    ASSERT vari2 = C2    report "Improper Assignment of vari2" SEVERITY FAILURE;
    ASSERT vari3 = C3    report "Improper Assignment of vari3" SEVERITY FAILURE;
    ASSERT vari4 = C4    report "Improper Assignment of vari4" SEVERITY FAILURE;
    ASSERT vari5 = C5    report "Improper Assignment of vari5" SEVERITY FAILURE;
    ASSERT vari6 = C6    report "Improper Assignment of vari6" SEVERITY FAILURE;
    ASSERT vari7 = C7    report "Improper Assignment of vari7" SEVERITY FAILURE;
    ASSERT vari8 = C8    report "Improper Assignment of vari8" SEVERITY FAILURE;
    ASSERT vari9 = C9    report "Improper Assignment of vari9" SEVERITY FAILURE;
    ASSERT vari10 = C10    report "Improper Assignment of vari10" SEVERITY FAILURE;
    ASSERT vari11 = C11    report "Improper Assignment of vari11" SEVERITY FAILURE;
    ASSERT vari12 = C12    report "Improper Assignment of vari12" SEVERITY FAILURE;
    ASSERT vari13 = C13    report "Improper Assignment of vari13" SEVERITY FAILURE;
    ASSERT vari14 = C14    report "Improper Assignment of vari14" SEVERITY FAILURE;
    ASSERT vari15 = C15    report "Improper Assignment of vari15" SEVERITY FAILURE;
    ASSERT vari16 = C16    report "Improper Assignment of vari16" SEVERITY FAILURE;
    ASSERT vari17 = C17    report "Improper Assignment of vari17" SEVERITY FAILURE;
    ASSERT vari18 = C18    report "Improper Assignment of vari18" SEVERITY FAILURE;
    ASSERT vari19 = C19    report "Improper Assignment of vari19" SEVERITY FAILURE;
    ASSERT vari20 = C20    report "Improper Assignment of vari20" SEVERITY FAILURE;
    ASSERT vari21 = C21    report "Improper Assignment of vari21" SEVERITY FAILURE;
    ASSERT vari22 = C22    report "Improper Assignment of vari22" SEVERITY FAILURE;
    ASSERT vari23 = C23    report "Improper Assignment of vari23" SEVERITY FAILURE;
    ASSERT vari24 = C24    report "Improper Assignment of vari24" SEVERITY FAILURE;
    ASSERT vari25 = C25    report "Improper Assignment of vari25" SEVERITY FAILURE;
    ASSERT vari26 = C26    report "Improper Assignment of vari26" SEVERITY FAILURE;
    ASSERT vari27 = C27    report "Improper Assignment of vari27" SEVERITY FAILURE;
    ASSERT vari28 = C28    report "Improper Assignment of vari28" SEVERITY FAILURE;
    ASSERT vari29 = C29    report "Improper Assignment of vari29" SEVERITY FAILURE;
    ASSERT vari30 = C30    report "Improper Assignment of vari30" SEVERITY FAILURE; 
    ASSERT vari31 = C31    report "Improper Assignment of vari31" SEVERITY FAILURE; 
    ASSERT vari32 = C32    report "Improper Assignment of vari32" SEVERITY FAILURE; 
--ASSERT vari33 = C33    report "Improper Assignment of vari33" SEVERITY FAILURE; 
--ASSERT vari34 = C34    report "Improper Assignment of vari34" SEVERITY FAILURE; 
--ASSERT vari35 = C35    report "Improper Assignment of vari35" SEVERITY FAILURE; 
--ASSERT vari36 = C36    report "Improper Assignment of vari36" SEVERITY FAILURE; 
--ASSERT vari37 = C37    report "Improper Assignment of vari37" SEVERITY FAILURE; 
--ASSERT vari38 = C38    report "Improper Assignment of vari38" SEVERITY FAILURE; 
--ASSERT vari39 = C39    report "Improper Assignment of vari39" SEVERITY FAILURE; 
--ASSERT vari40 = C40    report "Improper Assignment of vari40" SEVERITY FAILURE; 
    ASSERT vari41 = C41    report "Improper Assignment of vari41" SEVERITY FAILURE; 
    ASSERT vari42 = C42    report "Improper Assignment of vari42" SEVERITY FAILURE; 
    ASSERT vari43 = C43    report "Improper Assignment of vari43" SEVERITY FAILURE; 
    ASSERT vari44 = C44    report "Improper Assignment of vari44" SEVERITY FAILURE; 
    ASSERT vari45 = C45    report "Improper Assignment of vari45" SEVERITY FAILURE; 
    ASSERT vari46 = C46    report "Improper Assignment of vari46" SEVERITY FAILURE; 
    ASSERT vari47 = C47    report "Improper Assignment of vari47" SEVERITY FAILURE; 
    ASSERT vari48 = C48    report "Improper Assignment of vari48" SEVERITY FAILURE; 
    ASSERT vari49 = C49    report "Improper Assignment of vari49" SEVERITY FAILURE;
    ASSERT vari50 = C50    report "Improper Assignment of vari50" SEVERITY FAILURE;
    ASSERT vari51 = C51    report "Improper Assignment of vari51" SEVERITY FAILURE;
    ASSERT vari52 = C52    report "Improper Assignment of vari52" SEVERITY FAILURE;
    ASSERT vari53 = C53    report "Improper Assignment of vari53" SEVERITY FAILURE;
--ASSERT vari54 = C54    report "Improper Assignment of vari54" SEVERITY FAILURE;
--ASSERT vari54a = C54a    report "Improper Assignment of vari54a" SEVERITY FAILURE;
--ASSERT vari55 = C55    report "Improper Assignment of vari55" SEVERITY FAILURE;
    ASSERT vari56 = C56    report "Improper Assignment of vari56" SEVERITY FAILURE;
    ASSERT vari57 = C57    report "Improper Assignment of vari57" SEVERITY FAILURE;
    ASSERT vari58 = C58    report "Improper Assignment of vari58" SEVERITY FAILURE;
    ASSERT vari59 = C59    report "Improper Assignment of vari59" SEVERITY FAILURE;
    ASSERT vari60 = C60    report "Improper Assignment of vari60" SEVERITY FAILURE;
    ASSERT vari61 = C61    report "Improper Assignment of vari61" SEVERITY FAILURE;
    ASSERT vari62 = C62    report "Improper Assignment of vari62" SEVERITY FAILURE;
    ASSERT vari63 = C63    report "Improper Assignment of vari63" SEVERITY FAILURE;
    ASSERT vari64 = C64    report "Improper Assignment of vari64" SEVERITY FAILURE;
    ASSERT vari65 = C65    report "Improper Assignment of vari65" SEVERITY FAILURE;
    ASSERT vari66 = C66    report "Improper Assignment of vari66" SEVERITY FAILURE;
    ASSERT vari67 = C67    report "Improper Assignment of vari67" SEVERITY FAILURE;
    ASSERT vari68 = C68    report "Improper Assignment of vari68" SEVERITY FAILURE;
    ASSERT vari69 = C69    report "Improper Assignment of vari69" SEVERITY FAILURE;
    ASSERT vari70 = C70    report "Improper Assignment of vari70" SEVERITY FAILURE;
    ASSERT vari71 = C71    report "Improper Assignment of vari71" SEVERITY FAILURE;
    ASSERT vari72 = C72    report "Improper Assignment of vari72" SEVERITY FAILURE;
    ASSERT vari73 = C73    report "Improper Assignment of vari73" SEVERITY FAILURE;
    ASSERT vari74 = C74    report "Improper Assignment of vari74" SEVERITY FAILURE;
    ASSERT vari74 = C74    report "Improper Assignment of vari74" SEVERITY FAILURE;
    ASSERT vari75 = C75    report "Improper Assignment of vari75" SEVERITY FAILURE;
    ASSERT vari76 = C76    report "Improper Assignment of vari76" SEVERITY FAILURE;

    assert NOT(   vari1 = C1    and 
                  vari2 = C2    and 
                  vari3 = C3    and 
                  vari4 = C4    and 
                  vari5 = C5    and 
                  vari6 = C6    and 
                  vari7 = C7    and 
                  vari8 = C8    and 
                  vari9 = C9    and 
                  vari10 = C10    and 
                  vari11 = C11    and 
                  vari12 = C12    and 
                  vari13 = C13    and 
                  vari14 = C14    and 
                  vari15 = C15    and 
                  vari16 = C16    and 
                  vari17 = C17    and 
                  vari18 = C18    and 
                  vari19 = C19    and 
                  vari20 = C20    and 
                  vari21 = C21    and 
                  vari22 = C22    and 
                  vari23 = C23    and 
                  vari24 = C24    and 
                  vari25 = C25    and 
                  vari26 = C26    and 
                  vari27 = C27    and 
                  vari28 = C28    and 
                  vari29 = C29    and 
                  vari30 = C30    and 
                  vari31 = C31    and 
                  vari32 = C32    and 
--      vari33 = C33    and 
--      vari34 = C34    and 
--      vari35 = C35    and 
--      vari36 = C36    and 
--      vari37 = C37    and 
--      vari38 = C38    and 
--      vari39 = C39    and 
--      vari40 = C40    and 
                  vari41 = C41    and 
                  vari42 = C42    and 
                  vari43 = C43    and 
                  vari44 = C44    and 
                  vari45 = C45    and 
                  vari46 = C46    and 
                  vari47 = C47    and 
                  vari48 = C48    and 
                  vari49 = C49    and 
                  vari50 = C50    and 
                  vari51 = C51    and 
                  vari52 = C52    and 
                  vari53 = C53    and 
--      vari54 = C54    and 
--      vari54a = C54a    and 
--      vari55 = C55    and 
                  vari56 = C56    and 
                  vari57 = C57    and 
                  vari58 = C58    and 
                  vari59 = C59    and 
                  vari60 = C60    and 
                  vari61 = C61    and 
                  vari62 = C62    and 
                  vari63 = C63    and 
                  vari64 = C64    and 
                  vari65 = C65    and 
                  vari66 = C66    and 
                  vari67 = C67    and 
                  vari68 = C68    and 
                  vari69 = C69    and 
                  vari70 = C70    and 
                  vari71 = C71    and 
                  vari72 = C72    and 
                  vari73 = C73    and 
                  vari74 = C74    and 
                  vari75 = C75    and 
                  vari76 = C76    ) 
      report "***PASSED TEST: c06s03b00x00p06n01i00987" 
      severity NOTE;
    assert (   vari1 = C1    and 
               vari2 = C2    and 
               vari3 = C3    and 
               vari4 = C4    and 
               vari5 = C5    and 
               vari6 = C6    and 
               vari7 = C7    and 
               vari8 = C8    and 
               vari9 = C9    and 
               vari10 = C10    and 
               vari11 = C11    and 
               vari12 = C12    and 
               vari13 = C13    and 
               vari14 = C14    and 
               vari15 = C15    and 
               vari16 = C16    and 
               vari17 = C17    and 
               vari18 = C18    and 
               vari19 = C19    and 
               vari20 = C20    and 
               vari21 = C21    and 
               vari22 = C22    and 
               vari23 = C23    and 
               vari24 = C24    and 
               vari25 = C25    and 
               vari26 = C26    and 
               vari27 = C27    and 
               vari28 = C28    and 
               vari29 = C29    and 
               vari30 = C30    and 
               vari31 = C31    and 
               vari32 = C32    and 
--      vari33 = C33    and 
--      vari34 = C34    and 
--      vari35 = C35    and 
--      vari36 = C36    and 
--      vari37 = C37    and 
--      vari38 = C38    and 
--      vari39 = C39    and 
--      vari40 = C40    and 
               vari41 = C41    and 
               vari42 = C42    and 
               vari43 = C43    and 
               vari44 = C44    and 
               vari45 = C45    and 
               vari46 = C46    and 
               vari47 = C47    and 
               vari48 = C48    and 
               vari49 = C49    and 
               vari50 = C50    and 
               vari51 = C51    and 
               vari52 = C52    and 
               vari53 = C53    and 
--      vari54 = C54    and 
--      vari54a = C54a    and 
--      vari55 = C55    and 
               vari56 = C56    and 
               vari57 = C57    and 
               vari58 = C58    and 
               vari59 = C59    and 
               vari60 = C60    and 
               vari61 = C61    and 
               vari62 = C62    and 
               vari63 = C63    and 
               vari64 = C64    and 
               vari65 = C65    and 
               vari66 = C66    and 
               vari67 = C67    and 
               vari68 = C68    and 
               vari69 = C69    and 
               vari70 = C70    and 
               vari71 = C71    and 
               vari72 = C72    and 
               vari73 = C73    and 
               vari74 = C74    and 
               vari75 = C75    and 
               vari76 = C76    ) 
      report "***FAILED TEST: c06s03b00x00p06n01i00987 - Prefix of a selected name used to denote an object designated by an access value should be an access type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s03b00x00p06n01i00987arch;
