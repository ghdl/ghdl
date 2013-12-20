
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
-- $Id: tc527.vhd,v 1.2 2001-10-26 16:29:56 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s03b00x00p03n04i00527ent IS
END c03s03b00x00p03n04i00527ent;

ARCHITECTURE c03s03b00x00p03n04i00527arch OF c03s03b00x00p03n04i00527ent IS

BEGIN
  TESTING : PROCESS

    -- first index constraint method
    type bv_ptr is access bit_vector(0 to 7);
    variable v_bv_ptr1: bv_ptr    := new bit_vector'("00000001");
    variable v_bv_ptr2: bv_ptr;
    variable v_bv_ptr3: bv_ptr    := v_bv_ptr1;
    
    
    -- second index constraint method
    subtype tbus is bit_vector(1 to 8);
    type bus_ptr is access tbus;
    variable v_bv_ptr4: bus_ptr    := new tbus'("10000000");
    
    
    -- third index constraint method
    type bus_ptr2 is access bit_vector;
    variable v_bv_ptr5: bus_ptr2    := new bit_vector'("1111");
    variable v_bv_ptr6: bus_ptr2    := new bit_vector(1 to 4);

    variable OKtest : integer := 0;

  BEGIN
    assert v_bv_ptr1.all = "00000001";
    if (v_bv_ptr1.all = "00000001") then
      OKtest := Oktest + 1;
    end if;
    assert v_bv_ptr2 = null;
    if (v_bv_ptr2 = null) then
      OKtest := Oktest + 1;
    end if;
    assert v_bv_ptr3.all = "00000001";
    if (v_bv_ptr3.all = "00000001") then
      OKtest := Oktest + 1;
    end if;
    assert v_bv_ptr4.all = "10000000";
    if (v_bv_ptr4.all = "10000000") then
      OKtest := Oktest + 1;
    end if;
    assert v_bv_ptr5.all = "1111";
    if (v_bv_ptr5.all = "1111") then
      OKtest := Oktest + 1;
    end if;
    assert v_bv_ptr6.all = "0000";
    if (v_bv_ptr6.all = "0000") then
      OKtest := Oktest + 1;
    end if;
    
    v_bv_ptr2 := new bit_vector'("00110011");
    
    assert v_bv_ptr2.all = "00110011";
    if (v_bv_ptr6.all = "0000") then
      OKtest := Oktest + 1;
    end if;
    
    assert (v_bv_ptr1.all & v_bv_ptr3.all) = "0000000100000001";
    if ((v_bv_ptr1.all & v_bv_ptr3.all) = "0000000100000001") then
      OKtest := Oktest + 1;
    end if;
    assert (v_bv_ptr1.all & v_bv_ptr2.all) = "0000000100110011";
    if ((v_bv_ptr1.all & v_bv_ptr2.all) = "0000000100110011") then
      OKtest := Oktest + 1;
    end if;
    assert (v_bv_ptr5.all & v_bv_ptr6.all) = "11110000";
    if ((v_bv_ptr5.all & v_bv_ptr6.all) = "11110000") then
      OKtest := Oktest + 1;
    end if;
    assert (v_bv_ptr5.all & v_bv_ptr1.all) = "111100000001";
    if ((v_bv_ptr5.all & v_bv_ptr1.all) = "111100000001") then
      OKtest := Oktest + 1;
    end if;
    assert (v_bv_ptr6.all /= v_bv_ptr5.all) = true;
    if ((v_bv_ptr6.all /= v_bv_ptr5.all) = true) then
      OKtest := Oktest + 1;
    end if;
    
    deallocate(v_bv_ptr1);
    deallocate(v_bv_ptr2);
    deallocate(v_bv_ptr4);
    deallocate(v_bv_ptr5);
    deallocate(v_bv_ptr6);

    assert NOT(OKtest = 12) 
      report "***PASSED TEST: c03s03b00x00p03n04i00527" 
      severity NOTE;
    assert (OKtest = 12) 
      report "***FAILED TEST: c03s03b00x00p03n04i00527 - Bit Vector type using as base for access type test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s03b00x00p03n04i00527arch;
