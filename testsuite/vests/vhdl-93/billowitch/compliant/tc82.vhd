
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
-- $Id: tc82.vhd,v 1.2 2001-10-26 16:30:00 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b01x03p01n01i00082ent IS
END c04s03b01x03p01n01i00082ent;

ARCHITECTURE c04s03b01x03p01n01i00082arch OF c04s03b01x03p01n01i00082ent IS
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
      day   : INTEGER RANGE 0 TO 31;
      month : month_name;
      year  : INTEGER RANGE 0 TO 4000;
    END RECORD;
--
  SUBTYPE rst_date IS rt_date;
  
BEGIN
  TESTING: PROCESS
--
--          VARIABLE declarations
--
    
    VARIABLE     STRING_con_0 :     STRING (1 TO 7);
    VARIABLE     STRING_con_1 :     STRING (1 TO 7) := "sailing";
    VARIABLE     STRING_con_2 :     STRING (1 TO 7) := ( 's', 'a', 'i', 'l', 'i', 'n', 'g');
    
    VARIABLE BIT_VECTOR_con_0 : BIT_VECTOR (0 TO 7);
    VARIABLE BIT_VECTOR_con_1 : BIT_VECTOR (0 TO 7) := B"10101110";
    VARIABLE BIT_VECTOR_con_2 : BIT_VECTOR (0 TO 7) := ( '1', '0', '1', '0', '1', '1', '1', '0');
    
    VARIABLE   ut_chary_con_0 :   ut_chary (NUL TO ENQ);
    VARIABLE   ut_chary_con_1 :   ut_chary (NUL TO ENQ) := ( 1, 2, 3, 9, 8, 7);
    
    VARIABLE    ct_word_con_0 :    ct_word;
    VARIABLE    ct_word_con_1 :    ct_word := ( '1', '1', '1', '1', '1', '1', '1', '1',
                                                '1', '1', '1', '1', '1', '1', '1', '1');
    
    VARIABLE  cst_str10_con_0 :  cst_str10;
    VARIABLE  cst_str10_con_1 :  cst_str10 := "abcdefghij";
    VARIABLE  cst_str10_con_2 :  cst_str10 := ( 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'); 
    VARIABLE  cst_digit_con_0 :  cst_digit;
    VARIABLE  cst_digit_con_1 :  cst_digit := ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
    
    VARIABLE    rt_date_con_0 :    rt_date;
    VARIABLE    rt_date_con_1 :    rt_date := (1, Jan, 1989);
    
    VARIABLE   rst_date_con_0 :   rst_date;
    VARIABLE   rst_date_con_1 :   rst_date := (1, Apr, 2000);
    
----------------------------------------------------------------------------------------------------------
  BEGIN
    ASSERT STRING_con_0(1) = NUL REPORT "STRING_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT STRING_con_0(2) = NUL REPORT "STRING_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT STRING_con_0(3) = NUL REPORT "STRING_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT STRING_con_0(4) = NUL REPORT "STRING_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT STRING_con_0(5) = NUL REPORT "STRING_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT STRING_con_0(6) = NUL REPORT "STRING_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT STRING_con_0(7) = NUL REPORT "STRING_con_0 not properly intialized" SEVERITY FAILURE;
    
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
    
    ASSERT BIT_VECTOR_con_0(0) = '0' REPORT "BIT_VECTOR_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_0(1) = '0' REPORT "BIT_VECTOR_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_0(2) = '0' REPORT "BIT_VECTOR_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_0(3) = '0' REPORT "BIT_VECTOR_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_0(4) = '0' REPORT "BIT_VECTOR_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_0(5) = '0' REPORT "BIT_VECTOR_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_0(6) = '0' REPORT "BIT_VECTOR_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT BIT_VECTOR_con_0(7) = '0' REPORT "BIT_VECTOR_con_0 not properly intialized" SEVERITY FAILURE;
    
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
    
    ASSERT ut_chary_con_0(NUL) = INTEGER'LEFT REPORT "ut_chary_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT ut_chary_con_0(SOH) = INTEGER'LEFT REPORT "ut_chary_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT ut_chary_con_0(STX) = INTEGER'LEFT REPORT "ut_chary_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT ut_chary_con_0(ETX) = INTEGER'LEFT REPORT "ut_chary_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT ut_chary_con_0(EOT) = INTEGER'LEFT REPORT "ut_chary_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT ut_chary_con_0(ENQ) = INTEGER'LEFT REPORT "ut_chary_con_0 not properly intialized" SEVERITY FAILURE;
    
    ASSERT ut_chary_con_1(NUL) = 1 REPORT "ut_chary_con_1('a') not properly intialized" SEVERITY FAILURE;
    ASSERT ut_chary_con_1(SOH) = 2 REPORT "ut_chary_con_1('b') not properly intialized" SEVERITY FAILURE;
    ASSERT ut_chary_con_1(STX) = 3 REPORT "ut_chary_con_1('c') not properly intialized" SEVERITY FAILURE;
    ASSERT ut_chary_con_1(ETX) = 9 REPORT "ut_chary_con_1('d') not properly intialized" SEVERITY FAILURE;
    ASSERT ut_chary_con_1(EOT) = 8 REPORT "ut_chary_con_1('e') not properly intialized" SEVERITY FAILURE;
    ASSERT ut_chary_con_1(ENQ) = 7 REPORT "ut_chary_con_1('f') not properly intialized" SEVERITY FAILURE;
    
    ASSERT ct_word_con_0(0) = '0' REPORT "ct_word_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_0(1) = '0' REPORT "ct_word_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_0(2) = '0' REPORT "ct_word_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_0(3) = '0' REPORT "ct_word_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_0(4) = '0' REPORT "ct_word_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_0(5) = '0' REPORT "ct_word_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_0(6) = '0' REPORT "ct_word_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_0(7) = '0' REPORT "ct_word_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_0(8) = '0' REPORT "ct_word_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_0(9) = '0' REPORT "ct_word_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_0(10) = '0' REPORT "ct_word_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_0(11) = '0' REPORT "ct_word_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_0(12) = '0' REPORT "ct_word_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_0(13) = '0' REPORT "ct_word_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_0(14) = '0' REPORT "ct_word_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_0(15) = '0' REPORT "ct_word_con_0 not properly intialized" SEVERITY FAILURE;
    
    ASSERT ct_word_con_1(0) = '1' REPORT "ct_word_con_1 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_1(1) = '1' REPORT "ct_word_con_1 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_1(2) = '1' REPORT "ct_word_con_1 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_1(3) = '1' REPORT "ct_word_con_1 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_1(4) = '1' REPORT "ct_word_con_1 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_1(5) = '1' REPORT "ct_word_con_1 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_1(6) = '1' REPORT "ct_word_con_1 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_1(7) = '1' REPORT "ct_word_con_1 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_1(8) = '1' REPORT "ct_word_con_1 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_1(9) = '1' REPORT "ct_word_con_1 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_1(10) = '1' REPORT "ct_word_con_1 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_1(11) = '1' REPORT "ct_word_con_1 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_1(12) = '1' REPORT "ct_word_con_1 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_1(13) = '1' REPORT "ct_word_con_1 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_1(14) = '1' REPORT "ct_word_con_1 not properly intialized" SEVERITY FAILURE;
    ASSERT ct_word_con_1(15) = '1' REPORT "ct_word_con_1 not properly intialized" SEVERITY FAILURE;
    
    ASSERT cst_str10_con_0(1) = NUL REPORT "cst_str10_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_0(2) = NUL REPORT "cst_str10_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_0(3) = NUL REPORT "cst_str10_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_0(4) = NUL REPORT "cst_str10_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_0(5) = NUL REPORT "cst_str10_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_0(6) = NUL REPORT "cst_str10_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_0(7) = NUL REPORT "cst_str10_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_0(8) = NUL REPORT "cst_str10_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_0(9) = NUL REPORT "cst_str10_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT cst_str10_con_0(10) = NUL REPORT "cst_str10_con_0 not properly intialized" SEVERITY FAILURE;

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

    ASSERT cst_digit_con_0('0') = INTEGER'LEFT REPORT "cst_digit_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con_0('1') = INTEGER'LEFT REPORT "cst_digit_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con_0('2') = INTEGER'LEFT REPORT "cst_digit_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con_0('3') = INTEGER'LEFT REPORT "cst_digit_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con_0('4') = INTEGER'LEFT REPORT "cst_digit_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con_0('5') = INTEGER'LEFT REPORT "cst_digit_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con_0('6') = INTEGER'LEFT REPORT "cst_digit_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con_0('7') = INTEGER'LEFT REPORT "cst_digit_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con_0('8') = INTEGER'LEFT REPORT "cst_digit_con_0 not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con_0('9') = INTEGER'LEFT REPORT "cst_digit_con_0 not properly intialized" SEVERITY FAILURE;
    
    ASSERT cst_digit_con_1('0') = 0 REPORT "cst_digit_con_1('0') not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con_1('1') = 1 REPORT "cst_digit_con_1('1') not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con_1('2') = 2 REPORT "cst_digit_con_1('2') not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con_1('3') = 3 REPORT "cst_digit_con_1('3') not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con_1('4') = 4 REPORT "cst_digit_con_1('4') not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con_1('5') = 5 REPORT "cst_digit_con_1('5') not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con_1('6') = 6 REPORT "cst_digit_con_1('6') not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con_1('7') = 7 REPORT "cst_digit_con_1('7') not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con_1('8') = 8 REPORT "cst_digit_con_1('8') not properly intialized" SEVERITY FAILURE;
    ASSERT cst_digit_con_1('9') = 9 REPORT "cst_digit_con_1('9') not properly intialized" SEVERITY FAILURE;
    
    ASSERT  rt_date_con_0.day   = 0    REPORT " rt_date_con_0.day   not properly intialized" SEVERITY FAILURE;
    ASSERT  rt_date_con_0.month = Jan  REPORT " rt_date_con_0.month not properly intialized" SEVERITY FAILURE;
    ASSERT  rt_date_con_0.year  = 0    REPORT " rt_date_con_0.year  not properly intialized" SEVERITY FAILURE;
    
    ASSERT  rt_date_con_1.day   = 1    REPORT " rt_date_con_1.day   not properly intialized" SEVERITY FAILURE;
    ASSERT  rt_date_con_1.month = Jan  REPORT " rt_date_con_1.month not properly intialized" SEVERITY FAILURE;
    ASSERT  rt_date_con_1.year  = 1989 REPORT " rt_date_con_1.year  not properly intialized" SEVERITY FAILURE;
    
    ASSERT rst_date_con_0.day   = 0    REPORT "rst_date_con_0.day   not properly intialized" SEVERITY FAILURE;
    ASSERT rst_date_con_0.month = Jan  REPORT "rst_date_con_0.month not properly intialized" SEVERITY FAILURE;
    ASSERT rst_date_con_0.year  = 0    REPORT "rst_date_con_0.year  not properly intialized" SEVERITY FAILURE;
    
    ASSERT rst_date_con_1.day   = 1    REPORT "rst_date_con_1.day   not properly intialized" SEVERITY FAILURE;
    ASSERT rst_date_con_1.month = Apr  REPORT "rst_date_con_1.month not properly intialized" SEVERITY FAILURE;
    ASSERT rst_date_con_1.year  = 2000 REPORT "rst_date_con_1.year  not properly intialized" SEVERITY FAILURE;

--------------------------------------------------------------------------------------------------------------

    assert NOT(    STRING_con_0(1) = NUL      and 
                   STRING_con_0(2) = NUL       and
                   STRING_con_0(3) = NUL       and
                   STRING_con_0(4) = NUL       and
                   STRING_con_0(5) = NUL       and
                   STRING_con_0(6) = NUL       and
                   STRING_con_0(7) = NUL       and
                   STRING_con_1(1) = 's'       and
                   STRING_con_1(2) = 'a'       and
                   STRING_con_1(3) = 'i'       and
                   STRING_con_1(4) = 'l'       and
                   STRING_con_1(5) = 'i'       and
                   STRING_con_1(6) = 'n'       and
                   STRING_con_1(7) = 'g'       and
                   STRING_con_2(1) = 's'       and
                   STRING_con_2(2) = 'a'       and
                   STRING_con_2(3) = 'i'       and
                   STRING_con_2(4) = 'l'       and
                   STRING_con_2(5) = 'i'       and
                   STRING_con_2(6) = 'n'       and
                   STRING_con_2(7) = 'g'       and
                   BIT_VECTOR_con_0(0) = '0'    and
                   BIT_VECTOR_con_0(1) = '0'    and
                   BIT_VECTOR_con_0(2) = '0'    and
                   BIT_VECTOR_con_0(3) = '0'    and
                   BIT_VECTOR_con_0(4) = '0'    and
                   BIT_VECTOR_con_0(5) = '0'    and
                   BIT_VECTOR_con_0(6) = '0'    and
                   BIT_VECTOR_con_0(7) = '0'    and
                   BIT_VECTOR_con_1(0) = '1'    and
                   BIT_VECTOR_con_1(1) = '0'    and
                   BIT_VECTOR_con_1(2) = '1'    and
                   BIT_VECTOR_con_1(3) = '0'    and
                   BIT_VECTOR_con_1(4) = '1'    and
                   BIT_VECTOR_con_1(5) = '1'    and
                   BIT_VECTOR_con_1(6) = '1'    and
                   BIT_VECTOR_con_1(7) = '0'    and
                   BIT_VECTOR_con_2(0) = '1'    and
                   BIT_VECTOR_con_2(1) = '0'    and
                   BIT_VECTOR_con_2(2) = '1'    and
                   BIT_VECTOR_con_2(3) = '0'    and
                   BIT_VECTOR_con_2(4) = '1'    and
                   BIT_VECTOR_con_2(5) = '1'    and
                   BIT_VECTOR_con_2(6) = '1'    and
                   BIT_VECTOR_con_2(7) = '0'    and
                   ut_chary_con_0(NUL) = INTEGER'LEFT    and
                   ut_chary_con_0(SOH) = INTEGER'LEFT    and
                   ut_chary_con_0(STX) = INTEGER'LEFT    and
                   ut_chary_con_0(ETX) = INTEGER'LEFT    and
                   ut_chary_con_0(EOT) = INTEGER'LEFT    and
                   ut_chary_con_0(ENQ) = INTEGER'LEFT    and
                   ut_chary_con_1(NUL) = 1    and
                   ut_chary_con_1(SOH) = 2    and
                   ut_chary_con_1(STX) = 3    and
                   ut_chary_con_1(ETX) = 9    and
                   ut_chary_con_1(EOT) = 8    and
                   ut_chary_con_1(ENQ) = 7    and
                   ct_word_con_0(0) = '0'       and
                   ct_word_con_0(1) = '0'       and
                   ct_word_con_0(2) = '0'       and
                   ct_word_con_0(3) = '0'       and
                   ct_word_con_0(4) = '0'       and
                   ct_word_con_0(5) = '0'       and
                   ct_word_con_0(6) = '0'       and
                   ct_word_con_0(7) = '0'       and
                   ct_word_con_0(8) = '0'       and
                   ct_word_con_0(9) = '0'       and
                   ct_word_con_0(10) = '0'    and
                   ct_word_con_0(11) = '0'    and
                   ct_word_con_0(12) = '0'    and
                   ct_word_con_0(13) = '0'    and
                   ct_word_con_0(14) = '0'    and
                   ct_word_con_0(15) = '0'    and
                   ct_word_con_1(0) = '1'       and
                   ct_word_con_1(1) = '1'       and
                   ct_word_con_1(2) = '1'       and
                   ct_word_con_1(3) = '1'       and
                   ct_word_con_1(4) = '1'       and
                   ct_word_con_1(5) = '1'       and
                   ct_word_con_1(6) = '1'       and
                   ct_word_con_1(7) = '1'       and
                   ct_word_con_1(8) = '1'       and
                   ct_word_con_1(9) = '1'       and
                   ct_word_con_1(10) = '1'    and
                   ct_word_con_1(11) = '1'    and
                   ct_word_con_1(12) = '1'    and
                   ct_word_con_1(13) = '1'    and
                   ct_word_con_1(14) = '1'    and
                   ct_word_con_1(15) = '1'    and
                   cst_str10_con_0(1) = NUL    and
                   cst_str10_con_0(2) = NUL    and
                   cst_str10_con_0(3) = NUL    and
                   cst_str10_con_0(4) = NUL    and
                   cst_str10_con_0(5) = NUL    and
                   cst_str10_con_0(6) = NUL    and
                   cst_str10_con_0(7) = NUL    and
                   cst_str10_con_0(8) = NUL    and
                   cst_str10_con_0(9) = NUL    and
                   cst_str10_con_0(10) = NUL    and
                   cst_str10_con_1(1) = 'a'    and
                   cst_str10_con_1(2) = 'b'    and
                   cst_str10_con_1(3) = 'c'    and
                   cst_str10_con_1(4) = 'd'    and
                   cst_str10_con_1(5) = 'e'    and
                   cst_str10_con_1(6) = 'f'    and
                   cst_str10_con_1(7) = 'g'    and
                   cst_str10_con_1(8) = 'h'    and
                   cst_str10_con_1(9) = 'i'    and
                   cst_str10_con_1(10)= 'j'    and
                   cst_str10_con_2(1) = 'a'    and
                   cst_str10_con_2(2) = 'b'    and
                   cst_str10_con_2(3) = 'c'    and
                   cst_str10_con_2(4) = 'd'    and
                   cst_str10_con_2(5) = 'e'    and
                   cst_str10_con_2(6) = 'f'    and
                   cst_str10_con_2(7) = 'g'    and
                   cst_str10_con_2(8) = 'h'    and
                   cst_str10_con_2(9) = 'i'    and
                   cst_str10_con_2(10)= 'j'    and
                   cst_digit_con_0('0') = INTEGER'LEFT    and
                   cst_digit_con_0('1') = INTEGER'LEFT    and
                   cst_digit_con_0('2') = INTEGER'LEFT    and
                   cst_digit_con_0('3') = INTEGER'LEFT    and
                   cst_digit_con_0('4') = INTEGER'LEFT    and
                   cst_digit_con_0('5') = INTEGER'LEFT    and
                   cst_digit_con_0('6') = INTEGER'LEFT    and
                   cst_digit_con_0('7') = INTEGER'LEFT    and
                   cst_digit_con_0('8') = INTEGER'LEFT    and
                   cst_digit_con_0('9') = INTEGER'LEFT    and
                   cst_digit_con_1('0') = 0    and
                   cst_digit_con_1('1') = 1    and
                   cst_digit_con_1('2') = 2    and
                   cst_digit_con_1('3') = 3    and
                   cst_digit_con_1('4') = 4    and
                   cst_digit_con_1('5') = 5    and
                   cst_digit_con_1('6') = 6    and
                   cst_digit_con_1('7') = 7    and
                   cst_digit_con_1('8') = 8    and
                   cst_digit_con_1('9') = 9    and
                   rt_date_con_0.day   = 0       and
                   rt_date_con_0.month = Jan     and
                   rt_date_con_0.year  = 0       and
                   rt_date_con_1.day   = 1       and
                   rt_date_con_1.month = Jan     and
                   rt_date_con_1.year  = 1989    and
                   rst_date_con_0.day   = 0       and
                   rst_date_con_0.month = Jan     and
                   rst_date_con_0.year  = 0       and
                   rst_date_con_1.day   = 1       and
                   rst_date_con_1.month = Apr     and
                   rst_date_con_1.year  = 2000    )   
      report "***PASSED TEST: /src/ch04/sc03/sb01/ss03/p001/s010101.vhd"
      severity NOTE;
    assert (       STRING_con_0(1) = NUL      and 
                   STRING_con_0(2) = NUL       and
                   STRING_con_0(3) = NUL       and
                   STRING_con_0(4) = NUL       and
                   STRING_con_0(5) = NUL       and
                   STRING_con_0(6) = NUL       and
                   STRING_con_0(7) = NUL       and
                   STRING_con_1(1) = 's'       and
                   STRING_con_1(2) = 'a'       and
                   STRING_con_1(3) = 'i'       and
                   STRING_con_1(4) = 'l'       and
                   STRING_con_1(5) = 'i'       and
                   STRING_con_1(6) = 'n'       and
                   STRING_con_1(7) = 'g'       and
                   STRING_con_2(1) = 's'       and
                   STRING_con_2(2) = 'a'       and
                   STRING_con_2(3) = 'i'       and
                   STRING_con_2(4) = 'l'       and
                   STRING_con_2(5) = 'i'       and
                   STRING_con_2(6) = 'n'       and
                   STRING_con_2(7) = 'g'       and
                   BIT_VECTOR_con_0(0) = '0'    and
                   BIT_VECTOR_con_0(1) = '0'    and
                   BIT_VECTOR_con_0(2) = '0'    and
                   BIT_VECTOR_con_0(3) = '0'    and
                   BIT_VECTOR_con_0(4) = '0'    and
                   BIT_VECTOR_con_0(5) = '0'    and
                   BIT_VECTOR_con_0(6) = '0'    and
                   BIT_VECTOR_con_0(7) = '0'    and
                   BIT_VECTOR_con_1(0) = '1'    and
                   BIT_VECTOR_con_1(1) = '0'    and
                   BIT_VECTOR_con_1(2) = '1'    and
                   BIT_VECTOR_con_1(3) = '0'    and
                   BIT_VECTOR_con_1(4) = '1'    and
                   BIT_VECTOR_con_1(5) = '1'    and
                   BIT_VECTOR_con_1(6) = '1'    and
                   BIT_VECTOR_con_1(7) = '0'    and
                   BIT_VECTOR_con_2(0) = '1'    and
                   BIT_VECTOR_con_2(1) = '0'    and
                   BIT_VECTOR_con_2(2) = '1'    and
                   BIT_VECTOR_con_2(3) = '0'    and
                   BIT_VECTOR_con_2(4) = '1'    and
                   BIT_VECTOR_con_2(5) = '1'    and
                   BIT_VECTOR_con_2(6) = '1'    and
                   BIT_VECTOR_con_2(7) = '0'    and
                   ut_chary_con_0(NUL) = INTEGER'LEFT    and
                   ut_chary_con_0(SOH) = INTEGER'LEFT    and
                   ut_chary_con_0(STX) = INTEGER'LEFT    and
                   ut_chary_con_0(ETX) = INTEGER'LEFT    and
                   ut_chary_con_0(EOT) = INTEGER'LEFT    and
                   ut_chary_con_0(ENQ) = INTEGER'LEFT    and
                   ut_chary_con_1(NUL) = 1    and
                   ut_chary_con_1(SOH) = 2    and
                   ut_chary_con_1(STX) = 3    and
                   ut_chary_con_1(ETX) = 9    and
                   ut_chary_con_1(EOT) = 8    and
                   ut_chary_con_1(ENQ) = 7    and
                   ct_word_con_0(0) = '0'       and
                   ct_word_con_0(1) = '0'       and
                   ct_word_con_0(2) = '0'       and
                   ct_word_con_0(3) = '0'       and
                   ct_word_con_0(4) = '0'       and
                   ct_word_con_0(5) = '0'       and
                   ct_word_con_0(6) = '0'       and
                   ct_word_con_0(7) = '0'       and
                   ct_word_con_0(8) = '0'       and
                   ct_word_con_0(9) = '0'       and
                   ct_word_con_0(10) = '0'    and
                   ct_word_con_0(11) = '0'    and
                   ct_word_con_0(12) = '0'    and
                   ct_word_con_0(13) = '0'    and
                   ct_word_con_0(14) = '0'    and
                   ct_word_con_0(15) = '0'    and
                   ct_word_con_1(0) = '1'       and
                   ct_word_con_1(1) = '1'       and
                   ct_word_con_1(2) = '1'       and
                   ct_word_con_1(3) = '1'       and
                   ct_word_con_1(4) = '1'       and
                   ct_word_con_1(5) = '1'       and
                   ct_word_con_1(6) = '1'       and
                   ct_word_con_1(7) = '1'       and
                   ct_word_con_1(8) = '1'       and
                   ct_word_con_1(9) = '1'       and
                   ct_word_con_1(10) = '1'    and
                   ct_word_con_1(11) = '1'    and
                   ct_word_con_1(12) = '1'    and
                   ct_word_con_1(13) = '1'    and
                   ct_word_con_1(14) = '1'    and
                   ct_word_con_1(15) = '1'    and
                   cst_str10_con_0(1) = NUL    and
                   cst_str10_con_0(2) = NUL    and
                   cst_str10_con_0(3) = NUL    and
                   cst_str10_con_0(4) = NUL    and
                   cst_str10_con_0(5) = NUL    and
                   cst_str10_con_0(6) = NUL    and
                   cst_str10_con_0(7) = NUL    and
                   cst_str10_con_0(8) = NUL    and
                   cst_str10_con_0(9) = NUL    and
                   cst_str10_con_0(10) = NUL    and
                   cst_str10_con_1(1) = 'a'    and
                   cst_str10_con_1(2) = 'b'    and
                   cst_str10_con_1(3) = 'c'    and
                   cst_str10_con_1(4) = 'd'    and
                   cst_str10_con_1(5) = 'e'    and
                   cst_str10_con_1(6) = 'f'    and
                   cst_str10_con_1(7) = 'g'    and
                   cst_str10_con_1(8) = 'h'    and
                   cst_str10_con_1(9) = 'i'    and
                   cst_str10_con_1(10)= 'j'    and
                   cst_str10_con_2(1) = 'a'    and
                   cst_str10_con_2(2) = 'b'    and
                   cst_str10_con_2(3) = 'c'    and
                   cst_str10_con_2(4) = 'd'    and
                   cst_str10_con_2(5) = 'e'    and
                   cst_str10_con_2(6) = 'f'    and
                   cst_str10_con_2(7) = 'g'    and
                   cst_str10_con_2(8) = 'h'    and
                   cst_str10_con_2(9) = 'i'    and
                   cst_str10_con_2(10)= 'j'    and
                   cst_digit_con_0('0') = INTEGER'LEFT    and
                   cst_digit_con_0('1') = INTEGER'LEFT    and
                   cst_digit_con_0('2') = INTEGER'LEFT    and
                   cst_digit_con_0('3') = INTEGER'LEFT    and
                   cst_digit_con_0('4') = INTEGER'LEFT    and
                   cst_digit_con_0('5') = INTEGER'LEFT    and
                   cst_digit_con_0('6') = INTEGER'LEFT    and
                   cst_digit_con_0('7') = INTEGER'LEFT    and
                   cst_digit_con_0('8') = INTEGER'LEFT    and
                   cst_digit_con_0('9') = INTEGER'LEFT    and
                   cst_digit_con_1('0') = 0    and
                   cst_digit_con_1('1') = 1    and
                   cst_digit_con_1('2') = 2    and
                   cst_digit_con_1('3') = 3    and
                   cst_digit_con_1('4') = 4    and
                   cst_digit_con_1('5') = 5    and
                   cst_digit_con_1('6') = 6    and
                   cst_digit_con_1('7') = 7    and
                   cst_digit_con_1('8') = 8    and
                   cst_digit_con_1('9') = 9    and
                   rt_date_con_0.day   = 0       and
                   rt_date_con_0.month = Jan     and
                   rt_date_con_0.year  = 0       and
                   rt_date_con_1.day   = 1       and
                   rt_date_con_1.month = Jan     and
                   rt_date_con_1.year  = 1989    and
                   rst_date_con_0.day   = 0       and
                   rst_date_con_0.month = Jan     and
                   rst_date_con_0.year  = 0       and
                   rst_date_con_1.day   = 1       and
                   rst_date_con_1.month = Apr     and
                   rst_date_con_1.year  = 2000    )   
      report "***FAILED TEST: c04s03b01x03p01n01i00082 - A variable declaration declares a variable of the specified type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b01x03p01n01i00082arch;
