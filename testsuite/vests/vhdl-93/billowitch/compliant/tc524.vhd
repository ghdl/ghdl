
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
-- $Id: tc524.vhd,v 1.2 2001-10-26 16:29:56 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s03b00x00p03n04i00524ent IS
END c03s03b00x00p03n04i00524ent;

ARCHITECTURE c03s03b00x00p03n04i00524arch OF c03s03b00x00p03n04i00524ent IS

BEGIN
  TESTING: PROCESS
    type integer_ptr is access integer;
    variable v_integer_ptr1: integer_ptr := new integer'(365);
    variable v_integer_ptr2: integer_ptr;
    variable v_integer_ptr3: integer_ptr := v_integer_ptr1;


    type int is range -500 to 500;
    type int_ptr is access int;
    variable v_int_ptr1: int_ptr := new int'(365);
    variable v_int_ptr2: int_ptr;
    variable v_int_ptr3: int_ptr := v_int_ptr1;
    variable v_int_ptr4: int_ptr := new int'(-365);

    variable OKtest : integer := 0;

  BEGIN
    assert v_integer_ptr1.all = 365;
    if (v_integer_ptr1.all = 365) then
      Oktest := OKtest + 1;
    end if;
    assert v_integer_ptr2     = null;
    if (v_integer_ptr2     = null) then
      Oktest := OKtest + 1;
    end if;
    assert v_integer_ptr3.all = 365;
    if (v_integer_ptr3.all = 365) then
      Oktest := OKtest + 1;
    end if;

    assert (v_integer_ptr1.all + v_integer_ptr3.all) = 730;
    if ((v_integer_ptr1.all + v_integer_ptr3.all) = 730) then
      Oktest := OKtest + 1;
    end if;
    assert (v_integer_ptr1.all - v_integer_ptr3.all) = 0;
    if ((v_integer_ptr1.all - v_integer_ptr3.all) = 0) then
      Oktest := OKtest + 1;
    end if;
    assert (v_integer_ptr3.all * v_integer_ptr1.all) = 133225;
    if ((v_integer_ptr3.all * v_integer_ptr1.all) = 133225) then
      Oktest := OKtest + 1;
    end if;
    assert (v_integer_ptr3.all / v_integer_ptr1.all) = 1;
    if ((v_integer_ptr3.all / v_integer_ptr1.all) = 1) then
      Oktest := OKtest + 1;
    end if;

    deallocate(v_integer_ptr2);
    deallocate(v_integer_ptr1);

    assert v_int_ptr1.all = 365;
    if (v_int_ptr1.all = 365) then
      Oktest := OKtest + 1;
    end if;
    assert v_int_ptr2 = null;
    if (v_int_ptr2 = null) then
      Oktest := OKtest + 1;
    end if;
    assert v_int_ptr3.all = 365;
    if (v_int_ptr3.all = 365) then
      Oktest := OKtest + 1;
    end if;
    assert v_int_ptr4.all = -365;
    if (v_int_ptr4.all = -365) then
      Oktest := OKtest + 1;
    end if;

    v_int_ptr2 := new int'(100);

    assert v_int_ptr2.all = 100;
    if (v_int_ptr2.all = 100) then
      Oktest := OKtest + 1;
    end if;

    assert (v_int_ptr1.all + v_int_ptr3.all) = 730;
    if ((v_int_ptr1.all + v_int_ptr3.all) = 730) then
      Oktest := OKtest + 1;
    end if;
    assert (v_int_ptr2.all + v_int_ptr3.all) = 465;
    if ((v_int_ptr2.all + v_int_ptr3.all) = 465) then
      Oktest := OKtest + 1;
    end if;
    assert (v_int_ptr1.all + v_int_ptr4.all) = 0;
    if ((v_int_ptr1.all + v_int_ptr4.all) = 0) then
      Oktest := OKtest + 1;
    end if;
    assert (v_int_ptr1.all - v_int_ptr3.all) = 0;
    if ((v_int_ptr1.all - v_int_ptr3.all) = 0) then
      Oktest := OKtest + 1;
    end if;
    assert (v_int_ptr3.all * v_int_ptr1.all) = 133225;
    if ((v_int_ptr3.all * v_int_ptr1.all) = 133225) then
      Oktest := OKtest + 1;
    end if;
    assert (v_int_ptr3.all / v_int_ptr1.all) = 1;
    if ((v_int_ptr3.all / v_int_ptr1.all) = 1) then
      Oktest := OKtest + 1;
    end if;

    assert NOT(OKtest = 18) 
      report "***PASSED TEST: c03s03b00x00p03n04i00524"
      severity NOTE;
    assert (OKtest = 18) 
      report "***FAILED TEST: c03s03b00x00p03n04i00524 - Integer type using as base for access type test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s03b00x00p03n04i00524arch;
