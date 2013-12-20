
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
-- $Id: tc519.vhd,v 1.2 2001-10-26 16:29:56 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c03s03b00x00p03n01i00519pkg is
----------------------------------USING ONLY WHITE MATTER---------------------------------
------------------------------------------------------------------------------------------
---ACCESS TYPE FROM STANDARD PACKAGE

  type boolean_ptr    is access boolean ;              --simple boolean type
  type bit_ptr    is access bit ;                  --simple bit type
  type char_ptr    is access character;             --simple character type
  type severity_level_ptr    is access severity_level;--simple severity type
  type integer_ptr    is access integer;               --simple integer type
  type real_ptr    is access real;                  --simple real type
  type time_ptr    is access time;                  --simple time type
  type natural_ptr    is access natural;               --simple natural type
  type positive_ptr    is access positive;              --simple positive type
  type string_ptr    is access string;                --simple string type
  type bit_vector_ptr    is access bit_vector;            --simple bit_vector type

------------------------------------------------------------------------------------------

--UNCONSTRAINED ARRAY OF TYPES FROM STANDARD PACKAGE
--Index type is natural
  type boolean_vector    is array (natural range <>) of boolean;
  type severity_level_vector    is array (natural range <>) of severity_level;
  type integer_vector    is array (natural range <>) of integer;
  type real_vector    is array (natural range <>) of real;
  type time_vector    is array (natural range <>) of time;
  type natural_vector    is array (natural range <>) of natural;
  type positive_vector is array (natural range <>) of positive;

-------------------------------------------------------------------------------------------
--CONSTRAINED ARRAY OF TYPES FROM STANDARD PACKAGE
--Index type is natural
  subtype boolean_vector_st    is boolean_vector(0 to 15);
  subtype severity_level_vector_st    is  severity_level_vector(0 to 15);
  subtype integer_vector_st    is  integer_vector(0 to 15);
  subtype real_vector_st    is  real_vector(0 to 15);
  subtype time_vector_st    is  time_vector(0 to 15);
  subtype natural_vector_st    is  natural_vector(0 to 15);
  subtype positive_vector_st    is  positive_vector(0 to 15);
  
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
  
--CONSTRAINED ARRAY OF TYPES FROM STANDARD PACKAGE
--Index type is natural
  type boolean_cons_vector    is array (15 downto 0) of boolean;
  type severity_level_cons_vector    is array (15 downto 0) of severity_level;
  type integer_cons_vector    is array (15 downto 0) of integer;
  type real_cons_vector    is array (15 downto 0) of real;
  type time_cons_vector    is array (15 downto 0) of time;
  type natural_cons_vector    is array (15 downto 0) of natural;
  type positive_cons_vector    is array (15 downto 0) of positive;
  
-------------------------------------------------------------------------------------------
  
--CONSTRAINED ARRAY OF ARRAY OF TYPES FROM STANDARD PACKAGE
--Index type is natural
  type boolean_cons_vectorofvector    is array (0 to 15) of boolean_cons_vector;
  type severity_level_cons_vectorofvector    is array (0 to 15) of severity_level_cons_vector;
  type integer_cons_vectorofvector    is array (0 to 15) of integer_cons_vector ;
  type real_cons_vectorofvector    is array (0 to 15) of real_cons_vector;
  type time_cons_vectorofvector    is array (0 to 15) of time_cons_vector;
  type natural_cons_vectorofvector    is array (0 to 15) of natural_cons_vector;
  type positive_cons_vectorofvector    is array (0 to 15) of positive_cons_vector;
  
-------------------------------------------------------------------------------------------
  
--UNCONSTRAINED 2-DIMENSIONAL ARRAY OF TYPES FROM STANDARD PACKAGE
--Index type is natural
  type s2boolean_vector    is array (natural range <>,natural range <>) of boolean;
  type s2bit_vector       is array (natural range <>,natural range <>) of bit;
  type s2char_vector       is array (natural range <>,natural range <>) of character;
  type s2severity_level_vector is array (natural range <>,natural range <>) of severity_level;
  type s2integer_vector    is array (natural range <>,natural range <>) of integer;
  type s2real_vector       is array (natural range <>,natural range <>) of real;
  type s2time_vector       is array (natural range <>,natural range <>) of time;
  type s2natural_vector    is array (natural range <>,natural range <>) of natural;
  type s2positive_vector    is array (natural range <>,natural range <>) of positive;
  
-------------------------------------------------------------------------------------------
  
--CONSTRAINED 2-DIMENSIONAL ARRAY OF TYPES FROM STANDARD PACKAGE
--Index type is natural
  type column    is range 1 to 64;
  type row    is range 1 to 1024;
  type s2boolean_cons_vector    is array (row,column) of boolean;
  type s2bit_cons_vector    is array (row,column) of bit;
  type s2char_cons_vector    is array (row,column) of character;
  type s2severity_level_cons_vector    is array (row,column) of severity_level;
  type s2integer_cons_vector    is array (row,column) of integer;
  type s2real_cons_vector    is array (row,column) of real;
  type s2time_cons_vector    is array (row,column) of time;
  type s2natural_cons_vector    is array (row,column) of natural;
  type s2positive_cons_vector    is array (row,column) of positive;

-------------------------------------------------------------------------------------------
  
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
  
  
-------------------------------------------------------------------------------------------

--RECORD WITH FIELDS AS CONSTRAINT ARRAYS
  
  type record_array_st is record
                            a:boolean_vector_st;
                            b:severity_level_vector_st;
                            c:integer_vector_st;
                            d:real_vector_st;
                            e:time_vector_st;
                            f:natural_vector_st;
                            g:positive_vector_st;
                          end record;
  
-------------------------------------------------------------------------------------------
  
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
  
-------------------------------------------------------------------------------------------

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
  
-------------------------------------------------------------------------------------------

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
  
--------------------------------------------------------------------------------------------

  type record_of_ptr is record
                          a:boolean_ptr ;                                     --simple boolean type
                          b:bit_ptr;                                        --simple bit type
                          c:char_ptr;                                       --simple character type
                          e:severity_level_ptr;                        --simple severity type
                          f:integer_ptr;                                      --simple integer type
                          g:real_ptr ;                                 --simple real type
                          h:time_ptr;                                       --simple time type
                          i:natural_ptr;                                  --simple natural type
                          j:positive_ptr;                                    --simple positive type
                          k:string_ptr;                                   --simple string type
                          l:bit_vector_ptr;                                --simple bit_vector type
                        end record;
  
--------------------------------------------------------------------------------------------

  type record_of_records is record
                              a: record_std_package;
                              c: record_cons_array;
                              e: record_2cons_array;
                              g: record_cons_arrayofarray;
                              h: record_of_ptr;
                              i: record_array_st;
                            end record;
  
--------------------------------------------------------------------------------------------
--ACCESS TYPES FOR ABOVE
--------------------------------------------------------------------------------------------
  
  type boolean_vector_ptr       is access boolean_vector;
  type severity_level_vector_ptr    is access severity_level_vector;
  type integer_vector_ptr       is access integer_vector;
  type real_vector_ptr       is access real_vector;
  type time_vector_ptr       is access time_vector;
  type natural_vector_ptr       is access natural_vector;
  type positive_vector_ptr       is access positive_vector;
--------------------------------------------------------------------------------------------
  type boolean_cons_vector_ptr    is access boolean_cons_vector;
  type severity_level_cons_vector_ptr    is access severity_level_cons_vector;
  type integer_cons_vector_ptr    is access integer_cons_vector;
  type real_cons_vector_ptr       is access real_cons_vector;
  type time_cons_vector_ptr       is access time_cons_vector;
  type natural_cons_vector_ptr    is access natural_cons_vector;
  type positive_cons_vector_ptr    is access positive_cons_vector;
--------------------------------------------------------------------------------------------
  type boolean_cons_vectorofvector_ptr is access boolean_cons_vectorofvector;
  type sev_lvl_cons_vecofvec_ptr    is access severity_level_cons_vectorofvector;
  type integer_cons_vectorofvector_ptr is access integer_cons_vectorofvector;
  type real_cons_vectorofvector_ptr    is access real_cons_vectorofvector;
  type time_cons_vectorofvector_ptr    is access time_cons_vectorofvector;
  type natural_cons_vectorofvector_ptr is access natural_cons_vectorofvector;
  type posi_cons_vecofvec_ptr       is access positive_cons_vectorofvector;
--------------------------------------------------------------------------------------------
  type s2boolean_vector_ptr       is access boolean_vector;
  type s2bit_vector_ptr       is access s2bit_vector;
  type s2char_vector_ptr       is access s2char_vector;
  type s2severity_level_vector_ptr    is access s2severity_level_vector;
  type s2integer_vector_ptr       is access s2integer_vector;
  type s2real_vector_ptr       is access s2real_vector;
  type s2time_vector_ptr       is access s2time_vector;
  type s2positive_vector_ptr       is access s2positive_vector;
--------------------------------------------------------------------------------------------
  type s2boolean_cons_vector_ptr    is access s2boolean_cons_vector;
  type s2bit_cons_vector_ptr       is access s2bit_cons_vector;
  type s2char_cons_vector_ptr       is access s2char_cons_vector;
  type s2sev_lvl_cons_vec_ptr       is access s2severity_level_cons_vector;
  type s2integer_cons_vector_ptr    is access s2integer_cons_vector;
  type s2real_cons_vector_ptr       is access s2real_cons_vector;
  type s2time_cons_vector_ptr       is access s2time_cons_vector;
  type s2natural_cons_vector_ptr    is access natural_cons_vector;
  type s2positive_cons_vector_ptr    is access s2positive_cons_vector;
--------------------------------------------------------------------------------------------
  type record_std_package_ptr    is access record_std_package;
  type record_cons_array_ptr       is access record_cons_array;
  type record_2cons_array_ptr    is access record_2cons_array;
  type record_cons_arrayofarray_ptr    is access record_cons_arrayofarray;
  type record_of_ptr_ptr       is access record_of_ptr;
  type record_of_records_ptr       is access record_of_records;
  
--------------------------------------------------------------------------------------------
  
--------------------USING PARTIAL GRAY & PARTIAL WHITE MATTER-------------------------------
  
  
  type four_value is ('Z','0','1','X');                                    --enumerated type
  type four_value_map is array(four_value) of boolean;
  subtype binary is four_value range '0' to '1';
  type four_value_vector is array (natural range <>) of four_value;              --unconstraint array of
  type byte is array(0 to 7) of bit;
  subtype word is bit_vector(0 to  15);                                    --constrained array
  function resolution(i:in four_value_vector) return four_value;           --bus resolution
  subtype four_value_state is resolution four_value;                       --function type   
  type state_vector is array (natural range <>) of four_value_state;             --unconstraint array of
  constant size :integer := 63;
  type primary_memory is array(0 to size) of word;                        --array of an array
  type primary_memory_module is                                             --record with field
    record                                                                  --as an array
      enable:binary;
      memory_number:primary_memory;
    end record;
  type whole_memory is array(0 to size) of primary_memory_module;           --array of a complex record
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
  
  type four_value_ptr       is access four_value;
  type four_value_map_ptr    is access four_value_map;
  type binary_ptr       is access binary;
  type four_value_vector_ptr    is access four_value_vector;            --ennumerated type
  type byte_ptr       is access byte;
  type word_ptr       is access word;
  type four_value_state_ptr    is access four_value_state;
  type state_vector_ptr    is access state_vector;                 --type returned by resolu.
  type primary_memory_ptr    is access primary_memory;
  type whole_memory_ptr    is access whole_memory;
  type current_ptr       is access current;
  type resistance_ptr       is access resistance;
  type delay_ptr       is access delay;
  
-----------------------------------------------------------------------------------------
end c03s03b00x00p03n01i00519pkg;

package body c03s03b00x00p03n01i00519pkg is
  function resolution(i:in four_value_vector) return four_value is
    variable temp :four_value := 'Z';
  begin
    return temp;
  end;
end c03s03b00x00p03n01i00519pkg;

use work.c03s03b00x00p03n01i00519pkg.all;
ENTITY c03s03b00x00p03n01i00519ent IS
END c03s03b00x00p03n01i00519ent;

ARCHITECTURE c03s03b00x00p03n01i00519arch OF c03s03b00x00p03n01i00519ent IS
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
    variable var33 : s2boolean_vector_ptr ;
    variable var34 : s2bit_vector_ptr ;
    variable var35 : s2char_vector_ptr ;
    variable var36 : s2severity_level_vector_ptr ;
    variable var37 : s2integer_vector_ptr ;
    variable var38 : s2real_vector_ptr ;
    variable var39 : s2time_vector_ptr ;
    variable var40 : s2positive_vector_ptr ;
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
    variable var65 : whole_memory_ptr ;
    variable var66 : current_ptr ;
    variable var67 : resistance_ptr ;
    variable var68 : delay_ptr ;
  BEGIN
    assert (var1 = null)
      report "var1 has not been set to null." severity FAILURE ;
    assert (var2 = null)
      report "var2 has not been set to null." severity FAILURE ;
    assert (var3 = null)
      report "var3 has not been set to null." severity FAILURE ;
    assert (var4 = null)
      report "var4 has not been set to null." severity FAILURE ;
    assert (var5 = null)
      report "var5 has not been set to null." severity FAILURE ;
    assert (var6 = null)
      report "var6 has not been set to null." severity FAILURE ;
    assert (var7 = null)
      report "var7 has not been set to null." severity FAILURE ;
    assert (var8 = null)
      report "var8 has not been set to null." severity FAILURE ;
    assert (var9 = null)
      report "var9 has not been set to null." severity FAILURE ;
    assert (var10 = null)
      report "var10 has not been set to null." severity FAILURE ;
    assert (var11 = null)
      report "var11 has not been set to null." severity FAILURE ;
    assert (var12 = null)
      report "var12 has not been set to null." severity FAILURE ;
    assert (var13 = null)
      report "var13 has not been set to null." severity FAILURE ;
    assert (var14 = null)
      report "var14 has not been set to null." severity FAILURE ;
    assert (var15 = null)
      report "var15 has not been set to null." severity FAILURE ;
    assert (var16 = null)
      report "var16 has not been set to null." severity FAILURE ;
    assert (var17 = null)
      report "var17 has not been set to null." severity FAILURE ;
    assert (var18 = null)
      report "var18 has not been set to null." severity FAILURE ;
    assert (var19 = null)
      report "var19 has not been set to null." severity FAILURE ;
    assert (var20 = null)
      report "var20 has not been set to null." severity FAILURE ;
    assert (var21 = null)
      report "var21 has not been set to null." severity FAILURE ;
    assert (var22 = null)
      report "var22 has not been set to null." severity FAILURE ;
    assert (var23 = null)
      report "var23 has not been set to null." severity FAILURE ;
    assert (var24 = null)
      report "var24 has not been set to null." severity FAILURE ;
    assert (var25 = null)
      report "var25 has not been set to null." severity FAILURE ;
    assert (var26 = null)
      report "var26 has not been set to null." severity FAILURE ;
    assert (var27 = null)
      report "var27 has not been set to null." severity FAILURE ;
    assert (var28 = null)
      report "var28 has not been set to null." severity FAILURE ;
    assert (var29 = null)
      report "var29 has not been set to null." severity FAILURE ;
    assert (var30 = null)
      report "var30 has not been set to null." severity FAILURE ;
    assert (var31 = null)
      report "var31 has not been set to null." severity FAILURE ;
    assert (var32 = null)
      report "var32 has not been set to null." severity FAILURE ;
    assert (var33 = null)
      report "var33 has not been set to null." severity FAILURE ;
    assert (var34 = null)
      report "var34 has not been set to null." severity FAILURE ;
    assert (var35 = null)
      report "var35 has not been set to null." severity FAILURE ;
    assert (var36 = null)
      report "var36 has not been set to null." severity FAILURE ;
    assert (var37 = null)
      report "var37 has not been set to null." severity FAILURE ;
    assert (var38 = null)
      report "var38 has not been set to null." severity FAILURE ;
    assert (var39 = null)
      report "var39 has not been set to null." severity FAILURE ;
    assert (var40 = null)
      report "var40 has not been set to null." severity FAILURE ;
    assert (var41 = null)
      report "var41 has not been set to null." severity FAILURE ;
    assert (var42 = null)
      report "var42 has not been set to null." severity FAILURE ;
    assert (var43 = null)
      report "var43 has not been set to null." severity FAILURE ;
    assert (var44 = null)
      report "var44 has not been set to null." severity FAILURE ;
    assert (var45 = null)
      report "var45 has not been set to null." severity FAILURE ;
    assert (var46 = null)
      report "var46 has not been set to null." severity FAILURE ;
    assert (var47 = null)
      report "var47 has not been set to null." severity FAILURE ;
    assert (var48 = null)
      report "var48 has not been set to null." severity FAILURE ;
    assert (var49 = null)
      report "var49 has not been set to null." severity FAILURE ;
    assert (var50 = null)
      report "var50 has not been set to null." severity FAILURE ;
    assert (var51 = null)
      report "var51 has not been set to null." severity FAILURE ;
    assert (var52 = null)
      report "var52 has not been set to null." severity FAILURE ;
    assert (var53 = null)
      report "var53 has not been set to null." severity FAILURE ;
    assert (var54 = null)
      report "var54 has not been set to null." severity FAILURE ;
    assert (var55 = null)
      report "var55 has not been set to null." severity FAILURE ;
    assert (var56 = null)
      report "var56 has not been set to null." severity FAILURE ;
    assert (var57 = null)
      report "var57 has not been set to null." severity FAILURE ;
    assert (var58 = null)
      report "var58 has not been set to null." severity FAILURE ;
    assert (var59 = null)
      report "var59 has not been set to null." severity FAILURE ;
    assert (var60 = null)
      report "var60 has not been set to null." severity FAILURE ;
    assert (var61 = null)
      report "var61 has not been set to null." severity FAILURE ;
    assert (var62 = null)
      report "var62 has not been set to null." severity FAILURE ;
    assert (var63 = null)
      report "var63 has not been set to null." severity FAILURE ;
    assert (var64 = null)
      report "var64 has not been set to null." severity FAILURE ;
    assert (var65 = null)
      report "var65 has not been set to null." severity FAILURE ;
    assert (var66 = null)
      report "var66 has not been set to null." severity FAILURE ;
    assert (var67 = null)
      report "var67 has not been set to null." severity FAILURE ;
    assert (var68 = null)
      report "var68 has not been set to null." severity FAILURE ;
    assert   NOT((var1 = null)
                 and (var2 = null)
                 and (var3 = null)
                 and (var4 = null)
                 and (var5 = null)
                 and (var6 = null)
                 and (var7 = null)
                 and (var8 = null)
                 and (var9 = null)
                 and (var10 = null)
                 and (var11 = null)
                 and (var12 = null)
                 and (var13 = null)
                 and (var14 = null)
                 and (var15 = null)
                 and (var16 = null)
                 and (var17 = null)
                 and (var18 = null)
                 and (var19 = null)
                 and (var20 = null)
                 and (var21 = null)
                 and (var22 = null)
                 and (var23 = null)
                 and (var24 = null)
                 and (var25 = null)
                 and (var26 = null)
                 and (var27 = null)
                 and (var28 = null)
                 and (var29 = null)
                 and (var30 = null)
                 and (var31 = null)
                 and (var32 = null)
                 and (var33 = null)
                 and (var34 = null)
                 and (var35 = null)
                 and (var36 = null)
                 and (var37 = null)
                 and (var38 = null)
                 and (var39 = null)
                 and (var40 = null)
                 and (var41 = null)
                 and (var42 = null)
                 and (var43 = null)
                 and (var44 = null)
                 and (var45 = null)
                 and (var46 = null)
                 and (var47 = null)
                 and (var48 = null)
                 and (var49 = null)
                 and (var50 = null)
                 and (var51 = null)
                 and (var52 = null)
                 and (var53 = null)
                 and (var54 = null)
                 and (var55 = null)
                 and (var56 = null)
                 and (var57 = null)
                 and (var58 = null)
                 and (var59 = null)
                 and (var60 = null)
                 and (var61 = null)
                 and (var62 = null)
                 and (var63 = null)
                 and (var64 = null)
                 and (var65 = null)
                 and (var66 = null)
                 and (var67 = null)
                 and (var68 = null))
      report "***PASSED TEST: c03s03b00x00p03n01i00519"
      severity NOTE;
    assert      ((var1 = null)
                 and (var2 = null)
                 and (var3 = null)
                 and (var4 = null)
                 and (var5 = null)
                 and (var6 = null)
                 and (var7 = null)
                 and (var8 = null)
                 and (var9 = null)
                 and (var10 = null)
                 and (var11 = null)
                 and (var12 = null)
                 and (var13 = null)
                 and (var14 = null)
                 and (var15 = null)
                 and (var16 = null)
                 and (var17 = null)
                 and (var18 = null)
                 and (var19 = null)
                 and (var20 = null)
                 and (var21 = null)
                 and (var22 = null)
                 and (var23 = null)
                 and (var24 = null)
                 and (var25 = null)
                 and (var26 = null)
                 and (var27 = null)
                 and (var28 = null)
                 and (var29 = null)
                 and (var30 = null)
                 and (var31 = null)
                 and (var32 = null)
                 and (var33 = null)
                 and (var34 = null)
                 and (var35 = null)
                 and (var36 = null)
                 and (var37 = null)
                 and (var38 = null)
                 and (var39 = null)
                 and (var40 = null)
                 and (var41 = null)
                 and (var42 = null)
                 and (var43 = null)
                 and (var44 = null)
                 and (var45 = null)
                 and (var46 = null)
                 and (var47 = null)
                 and (var48 = null)
                 and (var49 = null)
                 and (var50 = null)
                 and (var51 = null)
                 and (var52 = null)
                 and (var53 = null)
                 and (var54 = null)
                 and (var55 = null)
                 and (var56 = null)
                 and (var57 = null)
                 and (var58 = null)
                 and (var59 = null)
                 and (var60 = null)
                 and (var61 = null)
                 and (var62 = null)
                 and (var63 = null)
                 and (var64 = null)
                 and (var65 = null)
                 and (var66 = null)
                 and (var67 = null)
                 and (var68 = null))
      report "***FAILED TEST: c03s03b00x00p03n01i00519 - The null value of an access type is the default initial value of the type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s03b00x00p03n01i00519arch;
