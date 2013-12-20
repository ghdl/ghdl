
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
-- $Id: tc528.vhd,v 1.2 2001-10-26 16:29:56 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s03b00x00p03n04i00528ent IS
END c03s03b00x00p03n04i00528ent;

ARCHITECTURE c03s03b00x00p03n04i00528arch OF c03s03b00x00p03n04i00528ent IS

BEGIN
  TESTING : PROCESS

    type char_ptr is access character;

    variable v_char_ptr1: char_ptr := new character'('a');
    variable v_char_ptr2: char_ptr;
    variable v_char_ptr3: char_ptr := v_char_ptr1;
    variable v_char_ptr4: char_ptr := new character'('|');

    variable OKtest : integer := 0;

  BEGIN
    assert v_char_ptr1.all = 'a';
    if (v_char_ptr1.all = 'a') then
      OKtest := Oktest + 1;
    end if;
    assert v_char_ptr2 = null;
    if (v_char_ptr2 = null) then
      OKtest := Oktest + 1;
    end if;
    assert v_char_ptr3.all = 'a';
    if (v_char_ptr3.all = 'a') then
      OKtest := Oktest + 1;
    end if;
    assert v_char_ptr4.all = '|';
    if (v_char_ptr4.all = '|') then
      OKtest := Oktest + 1;
    end if;

    v_char_ptr2 := new character'('K');

    assert v_char_ptr2.all = 'K';
    if (v_char_ptr2.all = 'K') then
      OKtest := Oktest + 1;
    end if;

    assert (v_char_ptr1.all & v_char_ptr3.all) = "aa";
    if ((v_char_ptr1.all & v_char_ptr3.all) = "aa") then
      OKtest := Oktest + 1;
    end if;
    assert (v_char_ptr1.all & v_char_ptr2.all) = "aK";
    if ((v_char_ptr1.all & v_char_ptr2.all) = "aK") then
      OKtest := Oktest + 1;
    end if;
    assert (v_char_ptr1.all & v_char_ptr4.all) = "a|";
    if ((v_char_ptr1.all & v_char_ptr4.all) = "a|") then
      OKtest := Oktest + 1;
    end if;
    assert (v_char_ptr1.all /= v_char_ptr4.all) = true;
    if ((v_char_ptr1.all /= v_char_ptr4.all) = true) then
      OKtest := Oktest + 1;
    end if;

    deallocate(v_char_ptr1);
    deallocate(v_char_ptr2);
    deallocate(v_char_ptr4);

    assert NOT(OKtest = 9) 
      report "***PASSED TEST: c03s03b00x00p03n04i00528" 
      severity NOTE;
    assert (OKtest = 9) 
      report "***FAILED TEST: c03s03b00x00p03n04i00528 - Character type using as base for access type test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s03b00x00p03n04i00528arch;
