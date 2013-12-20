
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
-- $Id: tc91.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

PACKAGE c04s03b02x00p01n01i00091pkg IS
--
--
--          Declaration of composite types
--           - array types and subtypes
--
  TYPE    ut_chary    IS ARRAY (CHARACTER RANGE <>) OF INTEGER;      --unconstrained array type

  TYPE    ct_word     IS ARRAY (0 TO 15)            OF BIT;          --constrained array type

  SUBTYPE ust_subchary IS ut_chary;                                  --unconstrained array subtype

  SUBTYPE cst_str10    IS STRING    ( 1  TO 10 );                    --constrained array subtype

  SUBTYPE cst_digit    IS ut_chary  ('0' TO '9');                    --constrained array subtype

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

END c04s03b02x00p01n01i00091pkg;


USE WORK.c04s03b02x00p01n01i00091pkg.ALL;
ENTITY c04s03b02x00p01n01i00091ent_a IS
  PORT (
    SIGNAL     STRING_prt : IN     STRING (1 TO 7);
    SIGNAL BIT_VECTOR_prt : IN BIT_VECTOR (0 TO 7);
    SIGNAL   ut_chary_prt : IN   ut_chary (NUL TO ENQ);
    SIGNAL    ct_word_prt : IN    ct_word;
    SIGNAL  cst_str10_prt : IN  cst_str10;
    SIGNAL  cst_digit_prt : IN  cst_digit;
    SIGNAL    rt_date_prt : IN    rt_date;
    SIGNAL   rst_date_prt : IN   rst_date
    );
END c04s03b02x00p01n01i00091ent_a;



ARCHITECTURE c04s03b02x00p01n01i00091arch_a OF c04s03b02x00p01n01i00091ent_a IS

BEGIN
  PROCESS
  BEGIN
--
    FOR I IN 1 TO 7
    LOOP
      ASSERT STRING_prt(I) = NUL REPORT "STRING_prt not properly intialized" SEVERITY FAILURE;
    END LOOP;
    
    FOR I IN 0 TO 7
    LOOP
      ASSERT BIT_VECTOR_prt(I) = '0' REPORT "BIT_VECTOR_prt not properly intialized" SEVERITY FAILURE;
    END LOOP;
    
    FOR I IN NUL TO ENQ
    LOOP
      ASSERT ut_chary_prt(I) = INTEGER'LEFT
        REPORT "ut_chary_prt not properly intialized" SEVERITY FAILURE;
    END LOOP;
    
    FOR I IN 0 TO 15
    LOOP
      ASSERT ct_word_prt(I) = '0' REPORT "ct_word_prt not properly intialized" SEVERITY FAILURE;
    END LOOP;
    FOR I IN 1 TO 10
    LOOP
      ASSERT cst_str10_prt(I) = NUL REPORT "cst_str10_prt not properly intialized" SEVERITY FAILURE;
    END LOOP;
    
    FOR I IN '0' TO '9'
    LOOP
      ASSERT cst_digit_prt(I) = INTEGER'LEFT
        REPORT "cst_digit_prt not properly intialized" SEVERITY FAILURE;      END LOOP;
    
    ASSERT  rt_date_prt.day   = 0    REPORT " rt_date_prt.day   not properly intialized" SEVERITY FAILURE;
    ASSERT  rt_date_prt.month = Jan  REPORT " rt_date_prt.month not properly intialized" SEVERITY FAILURE;
    ASSERT  rt_date_prt.year  = 0    REPORT " rt_date_prt.year  not properly intialized" SEVERITY FAILURE;
    
    ASSERT rst_date_prt.day   = 0    REPORT "rst_date_prt.day   not properly intialized" SEVERITY FAILURE;
    ASSERT rst_date_prt.month = Jan  REPORT "rst_date_prt.month not properly intialized" SEVERITY FAILURE;
    ASSERT rst_date_prt.year  = 0    REPORT "rst_date_prt.year  not properly intialized" SEVERITY FAILURE;
    
    
    assert NOT(   STRING_prt(1)      = NUL   and
                  STRING_prt(2)      = NUL   and   
                  STRING_prt(3)      = NUL   and   
                  STRING_prt(4)      = NUL   and   
                  STRING_prt(5)      = NUL   and   
                  STRING_prt(6)      = NUL   and   
                  STRING_prt(7)      = NUL   and   
                  BIT_VECTOR_prt(1)   = '0'   and
                  BIT_VECTOR_prt(2)   = '0'   and
                  BIT_VECTOR_prt(3)   = '0'   and
                  BIT_VECTOR_prt(4)   = '0'   and
                  BIT_VECTOR_prt(5)   = '0'   and
                  BIT_VECTOR_prt(6)   = '0'   and
                  BIT_VECTOR_prt(7)   = '0'   and
                  ut_chary_prt(NUL)   = integer'left   and
                  ut_chary_prt(SOH)   = integer'left   and
                  ut_chary_prt(STX)   = integer'left   and
                  ut_chary_prt(ETX)   = integer'left   and
                  ut_chary_prt(EOT)   = integer'left   and
                  ut_chary_prt(ENQ)   = integer'left   and
                  ct_word_prt( 0)      = '0'   and
                  ct_word_prt( 1)      = '0'   and
                  ct_word_prt( 2)      = '0'   and
                  ct_word_prt( 3)      = '0'   and
                  ct_word_prt( 4)      = '0'   and
                  ct_word_prt( 5)      = '0'   and
                  ct_word_prt( 6)      = '0'   and
                  ct_word_prt( 7)      = '0'   and
                  ct_word_prt( 8)      = '0'   and
                  ct_word_prt( 9)      = '0'   and
                  ct_word_prt(10)      = '0'   and
                  ct_word_prt(11)      = '0'   and
                  ct_word_prt(12)      = '0'   and
                  ct_word_prt(13)      = '0'   and
                  ct_word_prt(14)      = '0'   and
                  ct_word_prt(15)      = '0'   and
                  cst_str10_prt( 1)   = NUL   and
                  cst_str10_prt( 2)   = NUL   and
                  cst_str10_prt( 3)   = NUL   and
                  cst_str10_prt( 4)   = NUL   and
                  cst_str10_prt( 5)   = NUL   and
                  cst_str10_prt( 6)   = NUL   and
                  cst_str10_prt( 7)   = NUL   and
                  cst_str10_prt( 8)   = NUL   and
                  cst_str10_prt( 9)   = NUL   and
                  cst_str10_prt(10)   = NUL   and
                  cst_digit_prt('0')   = integer'left   and
                  cst_digit_prt('1')   = integer'left   and
                  cst_digit_prt('2')   = integer'left   and
                  cst_digit_prt('3')   = integer'left   and
                  cst_digit_prt('4')   = integer'left   and
                  cst_digit_prt('5')   = integer'left   and
                  cst_digit_prt('6')   = integer'left   and
                  cst_digit_prt('7')   = integer'left   and
                  cst_digit_prt('8')   = integer'left   and
                  cst_digit_prt('9')   = integer'left   and
                  rt_date_prt.day      = 0   and
                  rt_date_prt.month   = Jan   and
                  rt_date_prt.year   = 0   and
                  rst_date_prt.day   = 0   and
                  rst_date_prt.month   = Jan   and
                  rst_date_prt.year   = 0   )
      report "***PASSED TEST: c04s03b02x00p01n01i00091"
      severity NOTE;
    assert (   STRING_prt(1)      = NUL   and
               STRING_prt(2)      = NUL   and   
               STRING_prt(3)      = NUL   and   
               STRING_prt(4)      = NUL   and   
               STRING_prt(5)      = NUL   and   
               STRING_prt(6)      = NUL   and   
               STRING_prt(7)      = NUL   and   
               BIT_VECTOR_prt(1)   = '0'   and
               BIT_VECTOR_prt(2)   = '0'   and
               BIT_VECTOR_prt(3)   = '0'   and
               BIT_VECTOR_prt(4)   = '0'   and
               BIT_VECTOR_prt(5)   = '0'   and
               BIT_VECTOR_prt(6)   = '0'   and
               BIT_VECTOR_prt(7)   = '0'   and
               ut_chary_prt(NUL)   = integer'left   and
               ut_chary_prt(SOH)   = integer'left   and
               ut_chary_prt(STX)   = integer'left   and
               ut_chary_prt(ETX)   = integer'left   and
               ut_chary_prt(EOT)   = integer'left   and
               ut_chary_prt(ENQ)   = integer'left   and
               ct_word_prt( 0)      = '0'   and
               ct_word_prt( 1)      = '0'   and
               ct_word_prt( 2)      = '0'   and
               ct_word_prt( 3)      = '0'   and
               ct_word_prt( 4)      = '0'   and
               ct_word_prt( 5)      = '0'   and
               ct_word_prt( 6)      = '0'   and
               ct_word_prt( 7)      = '0'   and
               ct_word_prt( 8)      = '0'   and
               ct_word_prt( 9)      = '0'   and
               ct_word_prt(10)      = '0'   and
               ct_word_prt(11)      = '0'   and
               ct_word_prt(12)      = '0'   and
               ct_word_prt(13)      = '0'   and
               ct_word_prt(14)      = '0'   and
               ct_word_prt(15)      = '0'   and
               cst_str10_prt( 1)   = NUL   and
               cst_str10_prt( 2)   = NUL   and
               cst_str10_prt( 3)   = NUL   and
               cst_str10_prt( 4)   = NUL   and
               cst_str10_prt( 5)   = NUL   and
               cst_str10_prt( 6)   = NUL   and
               cst_str10_prt( 7)   = NUL   and
               cst_str10_prt( 8)   = NUL   and
               cst_str10_prt( 9)   = NUL   and
               cst_str10_prt(10)   = NUL   and
               cst_digit_prt('0')   = integer'left   and
               cst_digit_prt('1')   = integer'left   and
               cst_digit_prt('2')   = integer'left   and
               cst_digit_prt('3')   = integer'left   and
               cst_digit_prt('4')   = integer'left   and
               cst_digit_prt('5')   = integer'left   and
               cst_digit_prt('6')   = integer'left   and
               cst_digit_prt('7')   = integer'left   and
               cst_digit_prt('8')   = integer'left   and
               cst_digit_prt('9')   = integer'left   and
               rt_date_prt.day      = 0   and
               rt_date_prt.month   = Jan   and
               rt_date_prt.year   = 0   and
               rst_date_prt.day   = 0   and
               rst_date_prt.month   = Jan   and
               rst_date_prt.year   = 0   )
      report "***FAILED TEST: c04s03b02x00p01n01i00091 - Variables as the interface objects that appear as variable parameters of subprogram."
      severity ERROR;
    wait;
  END PROCESS;

END c04s03b02x00p01n01i00091arch_a;


USE WORK.c04s03b02x00p01n01i00091pkg.ALL;
ENTITY c04s03b02x00p01n01i00091ent IS
END c04s03b02x00p01n01i00091ent;

ARCHITECTURE c04s03b02x00p01n01i00091arch OF c04s03b02x00p01n01i00091ent IS
  COMPONENT c04s03b02x00p01n01i00091ent_a
    PORT (
      SIGNAL     STRING_prt : IN     STRING (1 TO 7);
      SIGNAL BIT_VECTOR_prt : IN BIT_VECTOR (0 TO 7);
      SIGNAL   ut_chary_prt : IN   ut_chary (NUL TO ENQ);
      SIGNAL    ct_word_prt : IN    ct_word;
      SIGNAL  cst_str10_prt : IN  cst_str10;
      SIGNAL  cst_digit_prt : IN  cst_digit;
      SIGNAL    rt_date_prt : IN    rt_date;
      SIGNAL   rst_date_prt : IN   rst_date
      );
  END COMPONENT;
  for c : c04s03b02x00p01n01i00091ent_a use entity work.c04s03b02x00p01n01i00091ent_a(c04s03b02x00p01n01i00091arch_a);
  
  SIGNAL     STRING_prt :     STRING (1 TO 7);
  SIGNAL BIT_VECTOR_prt : BIT_VECTOR (0 TO 7);
  SIGNAL   ut_chary_prt :   ut_chary (NUL TO ENQ);
  SIGNAL    ct_word_prt :    ct_word;
  SIGNAL  cst_str10_prt :  cst_str10;
  SIGNAL  cst_digit_prt :  cst_digit;
  SIGNAL    rt_date_prt :    rt_date;
  SIGNAL   rst_date_prt :   rst_date;
  
BEGIN
  C : c04s03b02x00p01n01i00091ent_a
    PORT MAP ( STRING_prt,
               BIT_VECTOR_prt,
               ut_chary_prt,
               ct_word_prt,
               cst_str10_prt,
               cst_digit_prt,
               rt_date_prt,
               rst_date_prt );
  

END c04s03b02x00p01n01i00091arch;
