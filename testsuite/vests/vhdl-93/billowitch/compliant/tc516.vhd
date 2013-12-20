
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
-- $Id: tc516.vhd,v 1.2 2001-10-26 16:29:55 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s03b00x00p03n04i00516ent IS
END c03s03b00x00p03n04i00516ent;

ARCHITECTURE c03s03b00x00p03n04i00516arch OF c03s03b00x00p03n04i00516ent IS

BEGIN
  TESTING: PROCESS

    type color is (red, green, blue);
    constant azure : color := blue;
    constant first : color := color'low;

    type enum_ptr is access color;
    variable v_enum_ptr1: enum_ptr := new color'(blue);
    variable v_enum_ptr2: enum_ptr;
    variable v_enum_ptr3: enum_ptr := v_enum_ptr1;
    variable v_enum_ptr4: enum_ptr := new color'(red);
    variable v_enum_ptr5: enum_ptr := v_enum_ptr4;

    variable OKtest : integer := 0;

  BEGIN

    assert v_enum_ptr1.all = blue;
    if (v_enum_ptr1.all = blue) then
      OKtest := OKtest + 1;
    end if;
    assert v_enum_ptr2     = null;
    if (v_enum_ptr2     = null) then
      OKtest := OKtest + 1;
    end if;
    assert v_enum_ptr3.all = blue;
    if (v_enum_ptr3.all = blue) then
      OKtest := OKtest + 1;
    end if;
    assert v_enum_ptr4.all = red;
    if (v_enum_ptr4.all = red) then
      OKtest := OKtest + 1;
    end if;
    assert v_enum_ptr5.all = red;
    if (v_enum_ptr5.all = red) then
      OKtest := OKtest + 1;
    end if;

    v_enum_ptr2 := new color'(green);

    assert v_enum_ptr2.all = green;
    if (v_enum_ptr2.all = green) then
      OKtest := OKtest + 1;
    end if;

    assert (v_enum_ptr3.all = color'succ(green));
    if (v_enum_ptr3.all = color'succ(green)) then
      OKtest := OKtest + 1;
    end if;
    assert (v_enum_ptr5.all = color'pred(v_enum_ptr2.all));
    if (v_enum_ptr5.all = color'pred(v_enum_ptr2.all)) then
      OKtest := OKtest + 1;
    end if;
    assert (color'pred(v_enum_ptr3.all) = green);
    if (color'pred(v_enum_ptr3.all) = green) then
      OKtest := OKtest + 1;
    end if;
    assert (v_enum_ptr5.all = color'low);
    if (v_enum_ptr5.all = color'low) then
      OKtest := OKtest + 1;
    end if;
    assert (v_enum_ptr3.all = color'high);
    if (v_enum_ptr3.all = color'high) then
      OKtest := OKtest + 1;
    end if;
    assert (v_enum_ptr5.all = color'left);
    if (v_enum_ptr5.all = color'left) then
      OKtest := OKtest + 1;
    end if;
    assert (v_enum_ptr3.all = color'right);
    if (v_enum_ptr3.all = color'right) then
      OKtest := OKtest + 1;
    end if;
    assert (v_enum_ptr3.all > v_enum_ptr5.all) = true;
    if (v_enum_ptr3.all > v_enum_ptr5.all) then
      OKtest := OKtest + 1;
    end if;

    assert NOT(OKtest = 14) 
      report "***PASSED TEST: c03s03b00x00p03n04i00516"
      severity NOTE;
    assert (OKtest = 14) 
      report "***FAILED TEST: c03s03b00x00p03n04i00516 - Enumeration type using as base for access type test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s03b00x00p03n04i00516arch;
