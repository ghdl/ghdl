
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
-- $Id: tc523.vhd,v 1.2 2001-10-26 16:29:56 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c03s03b00x00p03n03i00523pkg is
  type udf_int       is range -500 to 500;
  type udf_real       is range -2000.0 to 2000.0;
  type udf_int_ptr    is access udf_int;
  type udf_real_ptr    is access udf_real;
end c03s03b00x00p03n03i00523pkg;

use work.c03s03b00x00p03n03i00523pkg.all;

ENTITY c03s03b00x00p03n03i00523ent IS
END c03s03b00x00p03n03i00523ent;

ARCHITECTURE c03s03b00x00p03n03i00523arch OF c03s03b00x00p03n03i00523ent IS

BEGIN
  TESTING: PROCESS
    subtype int_ptr is udf_int_ptr;
    
    variable v_int_ptr1: int_ptr    := new udf_int'(365);
    variable v_int_ptr2: int_ptr;
    variable v_int_ptr3: int_ptr    := v_int_ptr1;
    variable v_int_ptr4: int_ptr    := new udf_int'(-365);
    
    subtype real_ptr is udf_real_ptr;
    
    variable v_real_ptr1: real_ptr    := new udf_real'(365.12);
    variable v_real_ptr2: real_ptr;
    variable v_real_ptr3: real_ptr    := v_real_ptr1;
    variable v_real_ptr4: real_ptr    := new udf_real'(-365.12);
    constant epislon: udf_real    := 0.00001;

  BEGIN

    assert v_int_ptr1.all    = 365;
    assert v_int_ptr2    = null;
    assert v_int_ptr3.all    = 365;
    assert v_int_ptr4.all    = -365;
    
    v_int_ptr2 := new udf_int'(100);
    
    assert v_int_ptr2.all    = 100;
    
    assert (v_int_ptr1.all + v_int_ptr3.all) = 730;
    assert (v_int_ptr2.all + v_int_ptr3.all) = 465;
    assert (v_int_ptr1.all + v_int_ptr4.all) = 0;
    assert (v_int_ptr1.all - v_int_ptr3.all) = 0;
    assert (v_int_ptr3.all * v_int_ptr1.all) = 133225;
    assert (v_int_ptr3.all / v_int_ptr1.all) = 1;

    assert v_real_ptr1.all    = 365.12;
    assert v_real_ptr2    = null;
    assert v_real_ptr3.all    = 365.12;
    assert v_real_ptr4.all    = -365.12;
    
    v_real_ptr2 := new udf_real'(100.0);
    
    assert v_real_ptr2.all    = 100.0;
    
    assert (v_real_ptr1.all + v_real_ptr3.all - 730.24 < epislon);
    assert (v_real_ptr1.all + v_real_ptr2.all - 465.12 < epislon);
    assert (v_real_ptr1.all + v_real_ptr4.all < epislon);
    assert (v_real_ptr1.all - v_real_ptr3.all < epislon);
    assert (v_real_ptr1.all * v_real_ptr3.all - 133312.6144 < epislon);
    assert (v_real_ptr1.all / v_real_ptr3.all - 1.0 < epislon);

    assert NOT(((v_int_ptr1.all + v_int_ptr3.all) = 730)   and
               (  (v_int_ptr2.all + v_int_ptr3.all) = 465)   and
               (  (v_int_ptr1.all + v_int_ptr4.all) = 0)   and
               (  (v_int_ptr1.all - v_int_ptr3.all) = 0)   and
               (  (v_int_ptr3.all * v_int_ptr1.all) = 133225)   and
               (  (v_int_ptr3.all / v_int_ptr1.all) = 1)   and
               (  (v_real_ptr1.all + v_real_ptr3.all - 730.24 < epislon))   and
               (  (v_real_ptr1.all + v_real_ptr2.all - 465.12 < epislon))   and
               (  (v_real_ptr1.all + v_real_ptr4.all < epislon))      and
               (  (v_real_ptr1.all - v_real_ptr3.all < epislon))      and
               (  (v_real_ptr1.all * v_real_ptr3.all - 133312.6144 < epislon))and
               (  (v_real_ptr1.all / v_real_ptr3.all - 1.0 < epislon)))   
      report "***PASSED TEST: c03s03b00x00p03n03i00523"
      severity NOTE;
    assert   (( (v_int_ptr1.all + v_int_ptr3.all) = 730)   and
              (  (v_int_ptr2.all + v_int_ptr3.all) = 465)   and
              (  (v_int_ptr1.all + v_int_ptr4.all) = 0)   and
              (  (v_int_ptr1.all - v_int_ptr3.all) = 0)   and
              (  (v_int_ptr3.all * v_int_ptr1.all) = 133225)   and
              (  (v_int_ptr3.all / v_int_ptr1.all) = 1)   and
              (  (v_real_ptr1.all + v_real_ptr3.all - 730.24 < epislon))   and
              (  (v_real_ptr1.all + v_real_ptr2.all - 465.12 < epislon))   and
              (  (v_real_ptr1.all + v_real_ptr4.all < epislon))      and
              (  (v_real_ptr1.all - v_real_ptr3.all < epislon))      and
              (  (v_real_ptr1.all * v_real_ptr3.all - 133312.6144 < epislon))and
              (  (v_real_ptr1.all / v_real_ptr3.all - 1.0 < epislon)))   
      report "***FAILED TEST: c03s03b00x00p03n03i00523 - User defined integer and floating point types to test for user defined integer and floating point using as base for access type failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s03b00x00p03n03i00523arch;
