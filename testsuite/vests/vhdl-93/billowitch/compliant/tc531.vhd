
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
-- $Id: tc531.vhd,v 1.2 2001-10-26 16:29:56 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s03b00x00p03n04i00531ent IS
END c03s03b00x00p03n04i00531ent;

ARCHITECTURE c03s03b00x00p03n04i00531arch OF c03s03b00x00p03n04i00531ent IS

BEGIN
  TESTING : PROCESS

    type bool_ptr is access boolean;
    variable v_bool_ptr1: bool_ptr := new boolean'(true);
    variable v_bool_ptr2: bool_ptr;
    variable v_bool_ptr3: bool_ptr := v_bool_ptr1;
    variable v_bool_ptr4: bool_ptr := new boolean'(false);
    variable v_bool_ptr5: bool_ptr := v_bool_ptr4;

    variable OKtest : integer := 0;

  BEGIN
    assert v_bool_ptr1.all = true;
    if (v_bool_ptr1.all = true) then
      OKtest := Oktest + 1;
    end if;
    assert v_bool_ptr2 = null;
    if (v_bool_ptr2 = null) then
      OKtest := Oktest + 1;
    end if;
    assert v_bool_ptr3.all = true;
    if (v_bool_ptr3.all = true) then
      OKtest := Oktest + 1;
    end if;
    assert v_bool_ptr4.all = false;
    if (v_bool_ptr4.all = false) then
      OKtest := Oktest + 1;
    end if;
    assert v_bool_ptr5.all = false;
    if (v_bool_ptr5.all = false) then
      OKtest := Oktest + 1;
    end if;
    
    v_bool_ptr2 := new boolean'(true);
    
    assert v_bool_ptr2.all = true;
    if (v_bool_ptr2.all = true) then
      OKtest := Oktest + 1;
    end if;
    
    assert (v_bool_ptr1.all and v_bool_ptr5.all) = false;
    if ((v_bool_ptr1.all and v_bool_ptr5.all) = false) then
      OKtest := Oktest + 1;
    end if;
    assert (v_bool_ptr1.all and v_bool_ptr2.all) = true;
    if ((v_bool_ptr1.all and v_bool_ptr2.all) = true) then
      OKtest := Oktest + 1;
    end if;
    assert (v_bool_ptr3.all or v_bool_ptr5.all) = true;
    if ((v_bool_ptr3.all or v_bool_ptr5.all) = true) then
      OKtest := Oktest + 1;
    end if;
    assert (v_bool_ptr3.all > v_bool_ptr5.all) = true;
    if ((v_bool_ptr3.all > v_bool_ptr5.all) = true) then
      OKtest := Oktest + 1;
    end if;
    assert (not v_bool_ptr3.all) = false;
    if ((not v_bool_ptr3.all) = false) then
      OKtest := Oktest + 1;
    end if;
    
    deallocate(v_bool_ptr1);
    deallocate(v_bool_ptr2);
    deallocate(v_bool_ptr4);

    assert NOT(OKtest = 11) 
      report "***PASSED TEST: c03s03b00x00p03n04i00531" 
      severity NOTE;
    assert (OKtest = 11) 
      report "***FAILED TEST: c03s03b00x00p03n04i00531 - Boolean type using as base for access type test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s03b00x00p03n04i00531arch;
