
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
-- $Id: tc31.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b01x01p01n01i00031ent IS
END c04s03b01x01p01n01i00031ent;

ARCHITECTURE c04s03b01x01p01n01i00031arch OF c04s03b01x01p01n01i00031ent IS

--
--
--          Declaration of composite types
--           - array types and subtypes
--
  TYPE    ut_chary    IS ARRAY (CHARACTER RANGE <>) OF INTEGER;      -- unconstrained array type    
  TYPE    ct_word     IS ARRAY (0 TO 15)            OF BIT;          -- constrained array type

  SUBTYPE ust_subchary IS ut_chary;                                  -- unconstrained array subtype
  SUBTYPE cst_str10    IS STRING    ( 1  TO 10 );                    -- constrained array subtype
  SUBTYPE cst_digit    IS ut_chary  ('0' TO '9');                    -- constrained array subtype
--
--          Declaration of composite types
--           - records types and subtypes
--
  TYPE month_name IS (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec );
  TYPE rt_date IS
    RECORD
      day   : INTEGER RANGE 1 TO 31;
      month : month_name;
      year  : INTEGER RANGE 0 TO 4000;
    END RECORD;
--
  SUBTYPE rst_date IS rt_date;

BEGIN
  TESTING: PROCESS
--
--          Constant declarations - without range constraint
--
    CONSTANT     STRING_con_1 :     STRING := "sailing";
    CONSTANT     STRING_con_2 :     STRING := ( 's', 'a', 'i', 'l', 'i', 'n', 'g');
    CONSTANT BIT_VECTOR_con_1 : BIT_VECTOR := B"10101110";
    CONSTANT BIT_VECTOR_con_2 : BIT_VECTOR := ( '1', '0', '1', '0', '1', '1', '1', '0');
    CONSTANT   ut_chary_con   :   ut_chary := ( 1, 2, 3, 9, 8, 7);
    CONSTANT    ct_word_con   :    ct_word := ( '0', '0', '0', '0', '0', '0', '0', '0',
                                                '0', '0', '0', '0', '0', '0', '0', '0');
    CONSTANT  cst_str10_con_1 :  cst_str10 := "abcdefghij";
    CONSTANT  cst_str10_con_2 :  cst_str10 := ( 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j');    
    CONSTANT  cst_digit_con   :  cst_digit := ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
    CONSTANT    rt_date_con   :    rt_date := (1, Jan, 1989);
    CONSTANT   rst_date_con   :   rst_date := (1, Apr, 2000);
    
----------------------------------------------------------------------------------------------------------
  BEGIN
    ASSERT STRING_con_1(1) = 's' REPORT "STRING_con_1(1) not properly intialized" SEVERITY FAILURE;
    ASSERT STRING_con_1(2) = 'a' REPORT "STRING_con_1(2) not properly intialized" SEVERITY FAILURE;
    ASSERT STRING_con_1(3) = 'i' REPORT "STRING_con_1(3) not properly intialized" SEVERITY FAILURE;
    ASSERT STRING_con_1(4) = 'l' REPORT "STRING_con_1(4) not properly intialized" SEVERITY FAILURE;
    ASSERT STRING_con_1(5) = 'i' REPORT "STRING_con_1(5) not properly intialized" SEVERITY FAILURE;
    ASSERT STRING_con_1(6) = 'n' REPORT "STRING_con_1(6) not properly intialized" SEVERITY FAILURE;
    ASSERT STRING_con_1(7) = 'g' REPORT "STRING_con_1(7) not properly intialized" SEVERITY FAILURE;
    
    ASSERT STRING_con_2(1) = 's' REPORT "STRING_con_2(1) not properly intialized" SEVERITY FAILURE;
    ASSERT STRING_con_2(2) = 'a' REPORT "STRING_con_2(2) not properly intialized" SEVERITY FAILURE;
    ASSERT STRING_con_2(3) = 'i' REPORT "STRING_con_2(3) not properly intialized" SEVERITY FAILURE;
    ASSERT STRING_con_2(4) = 'l' REPORT "STRING_con_2(4) not properly intialized" SEVERITY FAILURE;
    ASSERT STRING_con_2(5) = 'i' REPORT "STRING_con_2(5) not properly intialized" SEVERITY FAILURE;
    ASSERT STRING_con_2(6) = 'n' REPORT "STRING_con_2(6) not properly intialized" SEVERITY FAILURE;
    ASSERT STRING_con_2(7) = 'g' REPORT "STRING_con_2(7) not properly intialized" SEVERITY FAILURE;
    
    ASSERT BIT_VECTOR_con_1(0) = '1' REPORT "BIT_VECTOR_con_1(1) not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_1(1) = '0' REPORT "BIT_VECTOR_con_1(2) not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_1(2) = '1' REPORT "BIT_VECTOR_con_1(3) not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_1(3) = '0' REPORT "BIT_VECTOR_con_1(4) not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_1(4) = '1' REPORT "BIT_VECTOR_con_1(5) not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_1(5) = '1' REPORT "BIT_VECTOR_con_1(6) not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_1(6) = '1' REPORT "BIT_VECTOR_con_1(7) not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_1(7) = '0' REPORT "BIT_VECTOR_con_1(8) not properly intialized" SEVERITY FAILURE;

    ASSERT BIT_VECTOR_con_2(0) = '1' REPORT "BIT_VECTOR_con_2(1) not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_2(1) = '0' REPORT "BIT_VECTOR_con_2(2) not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_2(2) = '1' REPORT "BIT_VECTOR_con_2(3) not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_2(3) = '0' REPORT "BIT_VECTOR_con_2(4) not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_2(4) = '1' REPORT "BIT_VECTOR_con_2(5) not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_2(5) = '1' REPORT "BIT_VECTOR_con_2(6) not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_2(6) = '1' REPORT "BIT_VECTOR_con_2(7) not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_2(7) = '0' REPORT "BIT_VECTOR_con_2(8) not properly intialized" SEVERITY FAILURE;
    
    ASSERT ut_chary_con(NUL) = 1 REPORT "ut_chary_con('a') not properly intialized" SEVERITY FAILURE;
    ASSERT ut_chary_con(SOH) = 2 REPORT "ut_chary_con('b') not properly intialized" SEVERITY FAILURE;
    ASSERT ut_chary_con(STX) = 3 REPORT "ut_chary_con('c') not properly intialized" SEVERITY FAILURE;
    ASSERT ut_chary_con(ETX) = 9 REPORT "ut_chary_con('d') not properly intialized" SEVERITY FAILURE;
    ASSERT ut_chary_con(EOT) = 8 REPORT "ut_chary_con('e') not properly intialized" SEVERITY FAILURE;
    ASSERT ut_chary_con(ENQ) = 7 REPORT "ut_chary_con('f') not properly intialized" SEVERITY FAILURE;
    
    FOR I IN 0 TO 15
    LOOP
      ASSERT ct_word_con(I) = '0' REPORT "ct_word_con(I) not properly intialized" SEVERITY FAILURE;
    END LOOP;
    
    ASSERT cst_str10_con_1(1) = 'a' REPORT "cst_str10_con_1(1) not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_1(2) = 'b' REPORT "cst_str10_con_1(2) not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_1(3) = 'c' REPORT "cst_str10_con_1(3) not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_1(4) = 'd' REPORT "cst_str10_con_1(4) not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_1(5) = 'e' REPORT "cst_str10_con_1(5) not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_1(6) = 'f' REPORT "cst_str10_con_1(6) not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_1(7) = 'g' REPORT "cst_str10_con_1(7) not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_1(8) = 'h' REPORT "cst_str10_con_1(8) not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_1(9) = 'i' REPORT "cst_str10_con_1(9) not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_1(10)= 'j' REPORT "cst_str10_con_1(10)not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_2(1) = 'a' REPORT "cst_str10_con_2(1) not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_2(2) = 'b' REPORT "cst_str10_con_2(2) not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_2(3) = 'c' REPORT "cst_str10_con_2(3) not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_2(4) = 'd' REPORT "cst_str10_con_2(4) not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_2(5) = 'e' REPORT "cst_str10_con_2(5) not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_2(6) = 'f' REPORT "cst_str10_con_2(6) not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_2(7) = 'g' REPORT "cst_str10_con_2(7) not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_2(8) = 'h' REPORT "cst_str10_con_2(8) not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_2(9) = 'i' REPORT "cst_str10_con_2(9) not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_2(10)= 'j' REPORT "cst_str10_con_2(10)not properly intialized" SEVERITY FAILURE;
    
    ASSERT cst_digit_con('0') = 0 REPORT "cst_digit_con('0') not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con('1') = 1 REPORT "cst_digit_con('1') not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con('2') = 2 REPORT "cst_digit_con('2') not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con('3') = 3 REPORT "cst_digit_con('3') not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con('4') = 4 REPORT "cst_digit_con('4') not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con('5') = 5 REPORT "cst_digit_con('5') not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con('6') = 6 REPORT "cst_digit_con('6') not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con('7') = 7 REPORT "cst_digit_con('7') not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con('8') = 8 REPORT "cst_digit_con('8') not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con('9') = 9 REPORT "cst_digit_con('9') not properly intialized" SEVERITY FAILURE;
    
    ASSERT rt_date_con.day   = 1    REPORT "rt_date_con.day   not properly intialized" SEVERITY FAILURE;
    ASSERT rt_date_con.month = Jan  REPORT "rt_date_con.month not properly intialized" SEVERITY FAILURE;
    ASSERT rt_date_con.year  = 1989 REPORT "rt_date_con.year  not properly intialized" SEVERITY FAILURE;
    
    ASSERT rst_date_con.day   = 1    REPORT "rst_date_con.day   not properly intialized" SEVERITY
      FAILURE;
    ASSERT rst_date_con.month = Apr  REPORT "rst_date_con.month not properly intialized" SEVERITY
      FAILURE;
    ASSERT rst_date_con.year  = 2000 REPORT "rst_date_con.year  not properly intialized" SEVERITY
      FAILURE;

---------------------------------------------------------------------------------------------

    assert NOT(     STRING_con_1(1) = 's'      and 
                    STRING_con_1(2) = 'a'      and    
                    STRING_con_1(3) = 'i'      and    
                    STRING_con_1(4) = 'l'      and    
                    STRING_con_1(5) = 'i'      and 
                    STRING_con_1(6) = 'n'      and 
                    STRING_con_1(7) = 'g'      and 
                    STRING_con_2(1) = 's'      and 
                    STRING_con_2(2) = 'a'      and 
                    STRING_con_2(3) = 'i'      and 
                    STRING_con_2(4) = 'l'      and 
                    STRING_con_2(5) = 'i'      and 
                    STRING_con_2(6) = 'n'      and 
                    STRING_con_2(7) = 'g'      and 
                    BIT_VECTOR_con_1(0) = '1'   and 
                    BIT_VECTOR_con_1(1) = '0'   and 
                    BIT_VECTOR_con_1(2) = '1'   and 
                    BIT_VECTOR_con_1(3) = '0'   and 
                    BIT_VECTOR_con_1(4) = '1'   and 
                    BIT_VECTOR_con_1(5) = '1'   and 
                    BIT_VECTOR_con_1(6) = '1'   and 
                    BIT_VECTOR_con_1(7) = '0'   and 
                    BIT_VECTOR_con_2(0) = '1'   and 
                    BIT_VECTOR_con_2(1) = '0'   and 
                    BIT_VECTOR_con_2(2) = '1'   and 
                    BIT_VECTOR_con_2(3) = '0'   and 
                    BIT_VECTOR_con_2(4) = '1'   and 
                    BIT_VECTOR_con_2(5) = '1'   and 
                    BIT_VECTOR_con_2(6) = '1'   and 
                    BIT_VECTOR_con_2(7) = '0'   and 
                    ut_chary_con(NUL) = 1      and 
                    ut_chary_con(SOH) = 2      and 
                    ut_chary_con(STX) = 3      and 
                    ut_chary_con(ETX) = 9      and 
                    ut_chary_con(EOT) = 8      and 
                    ut_chary_con(ENQ) = 7      and  
                    ct_word_con(0) = '0'      and 
                    ct_word_con(1) = '0'      and 
                    ct_word_con(2) = '0'      and 
                    ct_word_con(3) = '0'      and 
                    ct_word_con(4) = '0'      and 
                    ct_word_con(5) = '0'      and 
                    ct_word_con(6) = '0'      and 
                    ct_word_con(7) = '0'      and 
                    ct_word_con(8) = '0'      and 
                    ct_word_con(9) = '0'      and 
                    ct_word_con(10) = '0'      and 
                    ct_word_con(11) = '0'      and 
                    ct_word_con(12) = '0'      and 
                    ct_word_con(13) = '0'      and 
                    ct_word_con(14) = '0'      and 
                    ct_word_con(15) = '0'      and 
                    cst_str10_con_1(1) = 'a'   and 
                    cst_str10_con_1(2) = 'b'   and 
                    cst_str10_con_1(3) = 'c'   and 
                    cst_str10_con_1(4) = 'd'   and 
                    cst_str10_con_1(5) = 'e'   and 
                    cst_str10_con_1(6) = 'f'   and 
                    cst_str10_con_1(7) = 'g'   and 
                    cst_str10_con_1(8) = 'h'   and 
                    cst_str10_con_1(9) = 'i'   and 
                    cst_str10_con_1(10)= 'j'   and 
                    cst_str10_con_2(1) = 'a'   and 
                    cst_str10_con_2(2) = 'b'   and 
                    cst_str10_con_2(3) = 'c'   and 
                    cst_str10_con_2(4) = 'd'   and 
                    cst_str10_con_2(5) = 'e'   and 
                    cst_str10_con_2(6) = 'f'   and 
                    cst_str10_con_2(7) = 'g'   and 
                    cst_str10_con_2(8) = 'h'   and 
                    cst_str10_con_2(9) = 'i'   and 
                    cst_str10_con_2(10)= 'j'   and 
                    cst_digit_con('0') = 0      and 
                    cst_digit_con('1') = 1      and 
                    cst_digit_con('2') = 2      and 
                    cst_digit_con('3') = 3      and 
                    cst_digit_con('4') = 4      and 
                    cst_digit_con('5') = 5      and 
                    cst_digit_con('6') = 6      and 
                    cst_digit_con('7') = 7      and 
                    cst_digit_con('8') = 8      and 
                    cst_digit_con('9') = 9      and 
                    rt_date_con.day   = 1      and 
                    rt_date_con.month = Jan    and 
                    rt_date_con.year  = 1989   and 
                    rst_date_con.day   = 1      and 
                    rst_date_con.month = Apr    and 
                    rst_date_con.year  = 2000   )    
      report "***PASSED TEST: /src/ch04/sc03/sb01/ss01/p001/s010101.vhd"
      severity NOTE;
    assert (        STRING_con_1(1) = 's'      and 
                    STRING_con_1(2) = 'a'      and    
                    STRING_con_1(3) = 'i'      and    
                    STRING_con_1(4) = 'l'      and    
                    STRING_con_1(5) = 'i'      and 
                    STRING_con_1(6) = 'n'      and 
                    STRING_con_1(7) = 'g'      and 
                    STRING_con_2(1) = 's'      and 
                    STRING_con_2(2) = 'a'      and 
                    STRING_con_2(3) = 'i'      and 
                    STRING_con_2(4) = 'l'      and 
                    STRING_con_2(5) = 'i'      and 
                    STRING_con_2(6) = 'n'      and 
                    STRING_con_2(7) = 'g'      and 
                    BIT_VECTOR_con_1(0) = '1'   and 
                    BIT_VECTOR_con_1(1) = '0'   and 
                    BIT_VECTOR_con_1(2) = '1'   and 
                    BIT_VECTOR_con_1(3) = '0'   and 
                    BIT_VECTOR_con_1(4) = '1'   and 
                    BIT_VECTOR_con_1(5) = '1'   and 
                    BIT_VECTOR_con_1(6) = '1'   and 
                    BIT_VECTOR_con_1(7) = '0'   and 
                    BIT_VECTOR_con_2(0) = '1'   and 
                    BIT_VECTOR_con_2(1) = '0'   and 
                    BIT_VECTOR_con_2(2) = '1'   and 
                    BIT_VECTOR_con_2(3) = '0'   and 
                    BIT_VECTOR_con_2(4) = '1'   and 
                    BIT_VECTOR_con_2(5) = '1'   and 
                    BIT_VECTOR_con_2(6) = '1'   and 
                    BIT_VECTOR_con_2(7) = '0'   and 
                    ut_chary_con(NUL) = 1      and 
                    ut_chary_con(SOH) = 2      and 
                    ut_chary_con(STX) = 3      and 
                    ut_chary_con(ETX) = 9      and 
                    ut_chary_con(EOT) = 8      and 
                    ut_chary_con(ENQ) = 7      and  
                    ct_word_con(0) = '0'      and 
                    ct_word_con(1) = '0'      and 
                    ct_word_con(2) = '0'      and 
                    ct_word_con(3) = '0'      and 
                    ct_word_con(4) = '0'      and 
                    ct_word_con(5) = '0'      and 
                    ct_word_con(6) = '0'      and 
                    ct_word_con(7) = '0'      and 
                    ct_word_con(8) = '0'      and 
                    ct_word_con(9) = '0'      and 
                    ct_word_con(10) = '0'      and 
                    ct_word_con(11) = '0'      and 
                    ct_word_con(12) = '0'      and 
                    ct_word_con(13) = '0'      and 
                    ct_word_con(14) = '0'      and 
                    ct_word_con(15) = '0'      and 
                    cst_str10_con_1(1) = 'a'   and 
                    cst_str10_con_1(2) = 'b'   and 
                    cst_str10_con_1(3) = 'c'   and 
                    cst_str10_con_1(4) = 'd'   and 
                    cst_str10_con_1(5) = 'e'   and 
                    cst_str10_con_1(6) = 'f'   and 
                    cst_str10_con_1(7) = 'g'   and 
                    cst_str10_con_1(8) = 'h'   and 
                    cst_str10_con_1(9) = 'i'   and 
                    cst_str10_con_1(10)= 'j'   and 
                    cst_str10_con_2(1) = 'a'   and 
                    cst_str10_con_2(2) = 'b'   and 
                    cst_str10_con_2(3) = 'c'   and 
                    cst_str10_con_2(4) = 'd'   and 
                    cst_str10_con_2(5) = 'e'   and 
                    cst_str10_con_2(6) = 'f'   and 
                    cst_str10_con_2(7) = 'g'   and 
                    cst_str10_con_2(8) = 'h'   and 
                    cst_str10_con_2(9) = 'i'   and 
                    cst_str10_con_2(10)= 'j'   and 
                    cst_digit_con('0') = 0      and 
                    cst_digit_con('1') = 1      and 
                    cst_digit_con('2') = 2      and 
                    cst_digit_con('3') = 3      and 
                    cst_digit_con('4') = 4      and 
                    cst_digit_con('5') = 5      and 
                    cst_digit_con('6') = 6      and 
                    cst_digit_con('7') = 7      and 
                    cst_digit_con('8') = 8      and 
                    cst_digit_con('9') = 9      and 
                    rt_date_con.day   = 1      and 
                    rt_date_con.month = Jan    and 
                    rt_date_con.year  = 1989   and 
                    rst_date_con.day   = 1      and 
                    rst_date_con.month = Apr    and 
                    rst_date_con.year  = 2000   )   
      report "***FAILED TEST: c04s03b01x01p01n01i00031 - A constant declares a constant of the specified type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b01x01p01n01i00031arch;
