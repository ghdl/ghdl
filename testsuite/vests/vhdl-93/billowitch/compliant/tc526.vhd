
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
-- $Id: tc526.vhd,v 1.2 2001-10-26 16:29:56 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s03b00x00p03n04i00526ent IS
END c03s03b00x00p03n04i00526ent;

ARCHITECTURE c03s03b00x00p03n04i00526arch OF c03s03b00x00p03n04i00526ent IS

BEGIN
  TESTING : PROCESS

    type beta is range 1000 downto 0
      units
        b1;
        b2  =  5 b1;
        b3  =  7 b2;
        b4  =  1 b3;
      end units;

    type phys_ptr is access beta;
    variable v_phys_ptr1: phys_ptr := new beta'(6 b1);
    variable v_phys_ptr2: phys_ptr;
    variable v_phys_ptr3: phys_ptr := v_phys_ptr1;
    variable v_phys_ptr4: phys_ptr := new beta'(1 b3);
    variable v_phys_ptr5: phys_ptr := v_phys_ptr4;

    variable OKtest : integer := 0;

  BEGIN
    assert v_phys_ptr1.all = 6 b1;
    if (v_phys_ptr1.all = 6 b1) then
      OKtest := Oktest + 1;
    end if;
    assert v_phys_ptr2 = null;
    if (v_phys_ptr2 = null) then
      OKtest := Oktest + 1;
    end if;
    assert v_phys_ptr3.all = 6 b1;
    if (v_phys_ptr3.all = 6 b1) then
      OKtest := Oktest + 1;
    end if;
    assert v_phys_ptr4.all = 1 b3;
    if (v_phys_ptr4.all = 1 b3) then
      OKtest := Oktest + 1;
    end if;
    assert v_phys_ptr5.all = b4;
    if (v_phys_ptr5.all = b4) then
      OKtest := Oktest + 1;
    end if;

    v_phys_ptr2 := new beta'(7 b2);

    assert v_phys_ptr2.all = b3;
    if (v_phys_ptr2.all = b3) then
      OKtest := Oktest + 1;
    end if;

    assert (40 * v_phys_ptr3.all / 6 = 1 b2 + 1 b4);
    if (40 * v_phys_ptr3.all / 6 = 1 b2 + 1 b4) then
      OKtest := Oktest + 1;
    end if;
    assert (v_phys_ptr5.all = 1 b2 + 6 b2);
    if (v_phys_ptr5.all = 1 b2 + 6 b2) then
      OKtest := Oktest + 1;
    end if;
    assert (v_phys_ptr5.all = v_phys_ptr2.all - 5 * 7 b1 + 1 b4);
    if (v_phys_ptr5.all = v_phys_ptr2.all - 5 * 7 b1 + 1 b4) then
      OKtest := Oktest + 1;
    end if;
    assert (v_phys_ptr5.all > v_phys_ptr1.all) = true;
    if ((v_phys_ptr5.all > v_phys_ptr1.all) = true) then
      OKtest := Oktest + 1;
    end if;

    deallocate(v_phys_ptr1);
    deallocate(v_phys_ptr2);
    deallocate(v_phys_ptr4);

    assert NOT(OKtest = 10) 
      report "***PASSED TEST: c03s03b00x00p03n04i00526" 
      severity NOTE;
    assert (OKtest = 10) 
      report "***FAILED TEST: c03s03b00x00p03n04i00526 - Physical type using as base for access type test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s03b00x00p03n04i00526arch;
