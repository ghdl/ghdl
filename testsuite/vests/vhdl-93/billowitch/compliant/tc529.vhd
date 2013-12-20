
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
-- $Id: tc529.vhd,v 1.2 2001-10-26 16:29:56 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s03b00x00p03n04i00529ent IS
END c03s03b00x00p03n04i00529ent;

ARCHITECTURE c03s03b00x00p03n04i00529arch OF c03s03b00x00p03n04i00529ent IS

BEGIN
  TESTING : PROCESS

    type string_ptr is access string(1 to 8);

    variable v_string_ptr1: string_ptr := new string'("abcd0123");
    variable v_string_ptr2: string_ptr;
    variable v_string_ptr3: string_ptr := v_string_ptr1;
    variable v_string_ptr4: string_ptr := new string'("=>*/&^!)");

    variable OKtest : integer := 0;

  BEGIN
    assert v_string_ptr1(1 to 8) = "abcd0123";
    if (v_string_ptr1(1 to 8) = "abcd0123") then
      OKtest := OKtest + 1;
    end if;
    assert v_string_ptr2 = null;
    if (v_string_ptr2 = null) then
      OKtest := OKtest + 1;
    end if;
    assert v_string_ptr3(1 to 8) = "abcd0123";
    if (v_string_ptr3(1 to 8) = "abcd0123") then
      OKtest := OKtest + 1;
    end if;
    assert v_string_ptr3(2 to 7) = "bcd012";
    if (v_string_ptr3(2 to 7) = "bcd012") then
      OKtest := OKtest + 1;
    end if;
    assert v_string_ptr3(6) = '1';
    if (v_string_ptr3(6) = '1') then
      OKtest := OKtest + 1;
    end if;

    v_string_ptr2 := new string'("ABCD----");
    
    assert v_string_ptr2(6) = '-';
    if (v_string_ptr3(6) = '1') then
      OKtest := OKtest + 1;
    end if;
    
    assert (v_string_ptr1(1 to 8) & v_string_ptr4(1 to 8)) = "abcd0123=>*/&^!)";        
    if ((v_string_ptr1(1 to 8) & v_string_ptr4(1 to 8)) = "abcd0123=>*/&^!)") then
      OKtest := OKtest + 1;
    end if;
    assert (v_string_ptr1(1 to 8) & v_string_ptr2(1 to 8)) = "abcd0123ABCD----";        
    if ((v_string_ptr1(1 to 8) & v_string_ptr2(1 to 8)) = "abcd0123ABCD----") then
      OKtest := OKtest + 1;
    end if;
    assert (v_string_ptr1(1 to 8) /= v_string_ptr4(1 to 8)) = true;
    if ((v_string_ptr1(1 to 8) /= v_string_ptr4(1 to 8)) = true) then
      OKtest := OKtest + 1;
    end if;
    assert (v_string_ptr1(1) /= v_string_ptr1(2)) = true;
    if ((v_string_ptr1(1) /= v_string_ptr1(2)) = true) then
      OKtest := OKtest + 1;
    end if;
    
    deallocate(v_string_ptr1);
    deallocate(v_string_ptr2);
    deallocate(v_string_ptr4);

    assert NOT(OKtest = 10) 
      report "***PASSED TEST: c03s03b00x00p03n04i00529" 
      severity NOTE;
    assert (OKtest = 10) 
      report "***FAILED TEST: c03s03b00x00p03n04i00529 - String type using as base for access type test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s03b00x00p03n04i00529arch;
