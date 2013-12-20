
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
-- $Id: tc494.vhd,v 1.2 2001-10-26 16:29:55 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b02x00p01n01i00494ent IS
END c03s02b02x00p01n01i00494ent;

ARCHITECTURE c03s02b02x00p01n01i00494arch OF c03s02b02x00p01n01i00494ent IS
  type colors is (orange,blue,red,black,white,magenta,ochre,yellow,green);
  type TR is RECORD
               i    : integer;
               ch    : character;
               bi    : bit;
               bool    : boolean;
               bv    : bit_vector (3000 to 3007);
               r    : real;
               str    : STRING (1 to 7);
               ti    : TIME;
               color    : colors;
             END RECORD;
  type TY is array(integer range <>) of TR;
  subtype T0 is TY (1 to 10);
  subtype T1 is TY (1 to 6);
  subtype T2 is TY (5 to 7);
  
  function FUNC1(array1: T0) return T1 is -- formal param object class defaults to constant
    variable array2:T1;
  begin -- procedure FUNC1
    array2 := array1(6) & array1(1) & array1(3) & array1(2) & array1(6) & array1(10); --indexed names
    return array2;
  end FUNC1;
  
  function FUNC2(array1: TY) return T1 is
    variable array2:T1;
  begin -- procedure FUNC2
    array2 := array1(6) & array1(1) & array1(3) & array1(2) & array1(6) & array1(10); --indexed names
    return array2;
  end FUNC2;
  
  function FUNC3(array1: T0) return T2 is
    variable array2:T2;
  begin -- procedure FUNC3
    array2 := array1(5 to 6) & array1(1 to 1); --slices
    return array2;
  end FUNC3;
  
  function FUNC4(array1: T2) return T2 is
    variable array2:T2;
  begin -- procedure FUNC4
    array2 := array1 (7 to 7) & array1(5 to 5) &
              array1(5 to 5); --slices
    return array2;
  end FUNC4; 
BEGIN
  TESTING: PROCESS
    variable arr1: T0;
    variable v1,v2:T1;
    variable v3,v4:T2;
  BEGIN
    arr1(1) := (1,'a','0',false,"00000001",1.1,"one    ",1.1 ms,orange);
    arr1(2) := (2,'b','1',true, "00000010",2.2,"two    ",2.2 ms,blue);
    arr1(3) := (3,'c','0',false,"00000011",3.3,"three  ",3.3 ms,red);
    arr1(4) := (4,'d','1',true, "00000100",4.4,"four   ",4.4 ms,black);
    arr1(5) := (5,'e','0',false,"00000101",5.5,"five   ",5.5 ms,white);
    arr1(6) := (6,'f','1',true, "00000110",6.6,"six    ",6.6 ms,magenta);
    arr1(7) := (7,'g','0',false,"00000111",7.7,"seven  ",7.7 ms,ochre);
    arr1(8) := (8,'h','1',true, "00001000",8.8,"eight  ",8.8 ms,yellow);
    arr1(9) := (9,'i','0',false,"00001001",9.9,"nine   ",9.9 ms,green);
    arr1(10):=(10,'j','1',true, "00001010",10.01,"ten    ",10.01 ms,white);
    wait for 1 ns;
    v1 := FUNC1(arr1);
    v2 := FUNC2(arr1);
    v3 := FUNC3(arr1);
    v4 := FUNC4(v3);
    
    wait for 1 ns;
    assert NOT( (v1(1) = (6,'f','1',true, "00000110",6.6,"six    ",6.6 ms,magenta)) AND
                (v1(2) = (1,'a','0',false,"00000001",1.1,"one    ",1.1 ms,orange)) AND
                (v1(3) = (3,'c','0',false,"00000011",3.3,"three  ",3.3 ms,red)) AND
                (v1(4) = (2,'b','1',true, "00000010",2.2,"two    ",2.2 ms,blue)) AND
                (v1(5) = (6,'f','1',true, "00000110",6.6,"six    ",6.6 ms,magenta)) AND
                (v1(6) = (10,'j','1',true, "00001010",10.01,"ten    ",10.01 ms,white)) AND
                (v2(1) = (6,'f','1',true, "00000110",6.6,"six    ",6.6 ms,magenta)) AND
                (v2(2) = (1,'a','0',false,"00000001",1.1,"one    ",1.1 ms,orange)) AND
                (v2(3) = (3,'c','0',false,"00000011",3.3,"three  ",3.3 ms,red)) AND
                (v2(4) = (2,'b','1',true, "00000010",2.2,"two    ",2.2 ms,blue)) AND
                (v2(5) = (6,'f','1',true, "00000110",6.6,"six    ",6.6 ms,magenta)) AND
                (v2(6) = (10,'j','1',true, "00001010",10.01,"ten    ",10.01 ms,white)) AND
                (v3(5) = (5,'e','0',false,"00000101",5.5,"five   ",5.5 ms,white)) AND
                (v3(6) = (6,'f','1',true, "00000110",6.6,"six    ",6.6 ms,magenta)) AND
                (v3(7) = (1,'a','0',false,"00000001",1.1,"one    ",1.1 ms,orange)) AND
                (v4(5) = (1,'a','0',false,"00000001",1.1,"one    ",1.1 ms,orange)) AND
                (v4(6) = (5,'e','0',false,"00000101",5.5,"five   ",5.5 ms,white)) AND
                (v4(7) = (5,'e','0',false,"00000101",5.5,"five   ",5.5 ms,white)))
      report "***PASSED TEST: c03s02b02x00p01n01i00494"
      severity NOTE;
    assert      ( (v1(1) = (6,'f','1',true, "00000110",6.6,"six    ",6.6 ms,magenta)) AND
                  (v1(2) = (1,'a','0',false,"00000001",1.1,"one    ",1.1 ms,orange)) AND
                  (v1(3) = (3,'c','0',false,"00000011",3.3,"three  ",3.3 ms,red)) AND
                  (v1(4) = (2,'b','1',true, "00000010",2.2,"two    ",2.2 ms,blue)) AND
                  (v1(5) = (6,'f','1',true, "00000110",6.6,"six    ",6.6 ms,magenta)) AND
                  (v1(6) = (10,'j','1',true, "00001010",10.01,"ten    ",10.01 ms,white)) AND
                  (v2(1) = (6,'f','1',true, "00000110",6.6,"six    ",6.6 ms,magenta)) AND
                  (v2(2) = (1,'a','0',false,"00000001",1.1,"one    ",1.1 ms,orange)) AND
                  (v2(3) = (3,'c','0',false,"00000011",3.3,"three  ",3.3 ms,red)) AND
                  (v2(4) = (2,'b','1',true, "00000010",2.2,"two    ",2.2 ms,blue)) AND
                  (v2(5) = (6,'f','1',true, "00000110",6.6,"six    ",6.6 ms,magenta)) AND
                  (v2(6) = (10,'j','1',true, "00001010",10.01,"ten    ",10.01 ms,white)) AND
                  (v3(5) = (5,'e','0',false,"00000101",5.5,"five   ",5.5 ms,white)) AND
                  (v3(6) = (6,'f','1',true, "00000110",6.6,"six    ",6.6 ms,magenta)) AND
                  (v3(7) = (1,'a','0',false,"00000001",1.1,"one    ",1.1 ms,orange)) AND
                  (v4(5) = (1,'a','0',false,"00000001",1.1,"one    ",1.1 ms,orange)) AND
                  (v4(6) = (5,'e','0',false,"00000101",5.5,"five   ",5.5 ms,white)) AND
                  (v4(7) = (5,'e','0',false,"00000101",5.5,"five   ",5.5 ms,white)))
      report "***FAILED TEST:c03s02b02x00p01n01i00494 - Problem assigning record subelements in function."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b02x00p01n01i00494arch;
