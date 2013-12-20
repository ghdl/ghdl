
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
-- $Id: tc1359.vhd,v 1.2 2001-10-26 16:29:40 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s05b00x00p03n01i01359ent IS
END c08s05b00x00p03n01i01359ent;

ARCHITECTURE c08s05b00x00p03n01i01359arch OF c08s05b00x00p03n01i01359ent IS

BEGIN
  TESTING: PROCESS
--
-- Define constants for package
--
    constant lowb : integer := 1 ;
    constant highb : integer := 5 ;
    constant lowb_i2 : integer := 0 ;
    constant highb_i2 : integer := 1000 ;
    constant lowb_p : integer := -100 ;
    constant highb_p : integer := 1000 ;
    constant lowb_r : real := 0.0 ;
    constant highb_r : real := 1000.0 ;
    constant lowb_r2 : real := 8.0 ;
    constant highb_r2 : real := 80.0 ;
    
    constant c_boolean_1 : boolean := false ;
    constant c_boolean_2 : boolean := true ;
--
-- bit
    constant c_bit_1 : bit := '0' ;
    constant c_bit_2 : bit := '1' ;
    
-- severity_level
    constant c_severity_level_1 : severity_level := NOTE ;
    constant c_severity_level_2 : severity_level := WARNING ;
--
-- character
    constant c_character_1 : character := 'A' ;
    constant c_character_2 : character := 'a' ;
    
-- integer types
-- predefined
    constant c_integer_1 : integer := lowb ;
    constant c_integer_2 : integer := highb ;
--
-- user defined integer type
    type     t_int1 is range 0 to 100 ;
    constant c_t_int1_1 : t_int1 := 0 ;
    constant c_t_int1_2 : t_int1 := 10 ;
    subtype  st_int1 is t_int1 range 8 to 60 ;
    constant c_st_int1_1 : st_int1 := 8 ;
    constant c_st_int1_2 : st_int1 := 9 ;
--
-- physical types
-- predefined
    constant c_time_1 : time := 1 ns ;
    constant c_time_2 : time := 2 ns ;
--
--
-- floating point types
-- predefined
    constant c_real_1 : real := 0.0 ;
    constant c_real_2 : real := 1.0 ;
--
-- simple record
    type     t_rec1 is record
                         f1 : integer range lowb_i2 to highb_i2 ;
                         f2 : time ;
                         f3 : boolean ;
                         f4 : real ;
                       end record ;
    constant c_t_rec1_1 : t_rec1 :=
      (c_integer_1, c_time_1, c_boolean_1, c_real_1) ;
    constant c_t_rec1_2 : t_rec1 :=
      (c_integer_2, c_time_2, c_boolean_2, c_real_2) ;
    subtype  st_rec1 is t_rec1 ;
    constant c_st_rec1_1 : st_rec1 := c_t_rec1_1 ;
    constant c_st_rec1_2 : st_rec1 := c_t_rec1_2 ;
--
-- more complex record
    type     t_rec2 is record
                         f1 : boolean ;
                         f2 : st_rec1 ;
                         f3 : time ;
                       end record ;
    constant c_t_rec2_1 : t_rec2 :=
      (c_boolean_1, c_st_rec1_1, c_time_1) ;
    constant c_t_rec2_2 : t_rec2 :=
      (c_boolean_2, c_st_rec1_2, c_time_2) ;
    subtype  st_rec2 is t_rec2 ;
    constant c_st_rec2_1 : st_rec2 := c_t_rec2_1 ;
    constant c_st_rec2_2 : st_rec2 := c_t_rec2_2 ;
--
-- simple array
    type     t_arr1 is array (integer range <>) of st_int1 ;
    subtype  t_arr1_range1 is integer range lowb to highb ;
    subtype  st_arr1 is t_arr1 (t_arr1_range1) ;
    constant c_st_arr1_1 : st_arr1 := (others => c_st_int1_1) ;
    constant c_st_arr1_2 : st_arr1 := (others => c_st_int1_2) ;
    constant c_t_arr1_1  : st_arr1 := c_st_arr1_1 ;
    constant c_t_arr1_2  : st_arr1 := c_st_arr1_2 ;
--
-- more complex array
    type     t_arr2 is array (integer range <>, boolean range <>) of st_arr1 ;
    subtype  t_arr2_range1 is integer range lowb to highb ;
    subtype  t_arr2_range2 is boolean range false to true ;
    subtype  st_arr2 is t_arr2 (t_arr2_range1, t_arr2_range2);
    constant c_st_arr2_1 : st_arr2 := (others => (others => c_st_arr1_1)) ;
    constant c_st_arr2_2 : st_arr2 := (others => (others => c_st_arr1_2)) ;
    constant c_t_arr2_1  : st_arr2 := c_st_arr2_1 ;
    constant c_t_arr2_2  : st_arr2 := c_st_arr2_2 ;
--
-- most complex record
    type     t_rec3 is record
                         f1 : boolean ;
                         f2 : st_rec2 ;
                         f3 : st_arr2 ;
                       end record ;
    constant c_t_rec3_1 : t_rec3 :=
      (c_boolean_1, c_st_rec2_1, c_st_arr2_1) ;
    constant c_t_rec3_2 : t_rec3 :=
      (c_boolean_2, c_st_rec2_2, c_st_arr2_2) ;
    subtype  st_rec3 is t_rec3 ;
    constant c_st_rec3_1 : st_rec3 := c_t_rec3_1 ;
    constant c_st_rec3_2 : st_rec3 := c_t_rec3_2 ;
--
-- most complex array
    type     t_arr3 is array (integer range <>, boolean range <>) of st_rec3 ;
    subtype  t_arr3_range1 is integer range lowb to highb ;
    subtype  t_arr3_range2 is boolean range true downto false ;
    subtype  st_arr3 is t_arr3 (t_arr3_range1, t_arr3_range2) ;
    constant c_st_arr3_1 : st_arr3 := (others => (others => c_st_rec3_1)) ;
    constant c_st_arr3_2 : st_arr3 := (others => (others => c_st_rec3_2)) ;
    constant c_t_arr3_1  : st_arr3 := c_st_arr3_1 ;
    constant c_t_arr3_2  : st_arr3 := c_st_arr3_2 ;
--
    variable v_st_rec3 : st_rec3 := c_st_rec3_1 ;
--
  BEGIN
    v_st_rec3.f3(st_arr2'Left(1),st_arr2'Left(2)) :=
      c_st_rec3_2.f3(st_arr2'Right(1),st_arr2'Right(2)) ;
    assert NOT(v_st_rec3.f3(st_arr2'Left(1),st_arr2'Left(2)) = c_st_arr1_2) 
      report "***PASSED TEST: c08s05b00x00p03n01i01359" 
      severity NOTE;
    assert (v_st_rec3.f3(st_arr2'Left(1),st_arr2'Left(2)) = c_st_arr1_2) 
      report "***FAILED TEST: c08s05b00x00p03n01i01359 - Target of a variable assignment is not a variable." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s05b00x00p03n01i01359arch;
