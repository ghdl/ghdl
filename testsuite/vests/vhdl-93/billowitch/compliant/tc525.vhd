
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
-- $Id: tc525.vhd,v 1.2 2001-10-26 16:29:56 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s03b00x00p03n04i00525ent IS
END c03s03b00x00p03n04i00525ent;

ARCHITECTURE c03s03b00x00p03n04i00525arch OF c03s03b00x00p03n04i00525ent IS

BEGIN
  TESTING : PROCESS

    type bit_ptr is access bit;
    variable v_bit_ptr1: bit_ptr := new bit'('1');
    variable v_bit_ptr2: bit_ptr;
    variable v_bit_ptr3: bit_ptr := v_bit_ptr1;
    variable v_bit_ptr4: bit_ptr := new bit'('0');
    variable v_bit_ptr5: bit_ptr := v_bit_ptr4;

    variable OKtest : integer := 0;

  BEGIN
    assert v_bit_ptr1.all = '1';
    if (v_bit_ptr1.all = '1') then
      OKtest := OKtest + 1;
    end if;
    assert v_bit_ptr2 = null;
    if (v_bit_ptr2 = null) then
      OKtest := OKtest + 1;
    end if;
    assert v_bit_ptr3.all = '1';
    if (v_bit_ptr3.all = '1') then
      OKtest := OKtest + 1;
    end if;
    assert v_bit_ptr4.all = '0';
    if (v_bit_ptr4.all = '0') then
      OKtest := OKtest + 1;
    end if;
    assert v_bit_ptr5.all = '0';
    if (v_bit_ptr5.all = '0') then
      OKtest := OKtest + 1;
    end if;

    v_bit_ptr2 := new bit'('0');

    assert v_bit_ptr2.all = '0';
    if (v_bit_ptr2.all = '0') then
      OKtest := OKtest + 1;
    end if;

    assert (v_bit_ptr1.all & v_bit_ptr3.all) = "11";
    if ((v_bit_ptr1.all & v_bit_ptr3.all) = "11") then
      OKtest := OKtest + 1;
    end if;
    assert (v_bit_ptr3.all & v_bit_ptr5.all) = "10";
    if ((v_bit_ptr3.all & v_bit_ptr5.all) = "10") then
      OKtest := OKtest + 1;
    end if;
    assert (v_bit_ptr3.all & v_bit_ptr2.all) = "10";
    if ((v_bit_ptr3.all & v_bit_ptr2.all) = "10") then
      OKtest := OKtest + 1;
    end if;
    assert (v_bit_ptr3.all > v_bit_ptr5.all) = true;
    if ((v_bit_ptr3.all > v_bit_ptr5.all) = true) then
      OKtest := OKtest + 1;
    end if;

    deallocate(v_bit_ptr1);
    deallocate(v_bit_ptr2);
    deallocate(v_bit_ptr4);

    assert NOT(OKtest = 10) 
      report "***PASSED TEST: c03s03b00x00p03n04i00525" 
      severity NOTE;
    assert (OKtest = 10) 
      report "***FAILED TEST: c03s03b00x00p03n04i00525 - Bit type using as base for access type test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s03b00x00p03n04i00525arch;
