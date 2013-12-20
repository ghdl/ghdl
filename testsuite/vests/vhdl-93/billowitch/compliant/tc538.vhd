
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
-- $Id: tc538.vhd,v 1.2 2001-10-26 16:29:56 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s03b00x00p05n02i00538ent IS
END c03s03b00x00p05n02i00538ent;

ARCHITECTURE c03s03b00x00p05n02i00538arch OF c03s03b00x00p05n02i00538ent IS

BEGIN
  TESTING: PROCESS
    subtype byte is bit_vector (7 downto 0);
    type byte_mem is array (0 to 15) of byte;

    type ar_bv_ptr is access byte_mem;
    variable v_ar_bv_ptr1: ar_bv_ptr := new byte_mem'(0 => "10000000",
                                                      1 => "00000001",
                                                      others => "00000000");
    variable v_ar_bv_ptr2: ar_bv_ptr;
    variable v_ar_bv_ptr3: ar_bv_ptr := v_ar_bv_ptr1;
    variable   OKtest : integer := 0;
  BEGIN
    assert v_ar_bv_ptr1(1)    = "00000001";
    if (v_ar_bv_ptr1(1)  = "00000001") then
      OKtest := Oktest + 1;
    end if;
    assert v_ar_bv_ptr2    = null;
    if (v_ar_bv_ptr2     = null) then
      OKtest := Oktest + 1;
    end if;
    assert v_ar_bv_ptr3(0)    = "10000000";
    if (v_ar_bv_ptr3(0)  = "10000000") then
      OKtest := Oktest + 1;
    end if;
    assert v_ar_bv_ptr3(15) = "00000000";
    if (v_ar_bv_ptr3(15) = "00000000") then
      OKtest := Oktest + 1;
    end if;
    assert v_ar_bv_ptr3(1)(0)    = '1';        -- (7 downto 0)
    if (v_ar_bv_ptr3(1)(0)       = '1') then
      OKtest := Oktest + 1;
    end if;

    v_ar_bv_ptr2 := new byte_mem'(0 => "10000000",
                                  1 => "00000001",
                                  others => "00000000");

    assert v_ar_bv_ptr2(0)(7) = '1';        -- (7 downto 0)
    if (v_ar_bv_ptr2(0)(7) = '1') then
      OKtest := Oktest + 1;
    end if;

    assert (v_ar_bv_ptr1(1) & v_ar_bv_ptr3(7)) = "0000000100000000";
    if ((v_ar_bv_ptr1(1) & v_ar_bv_ptr3(7)) = "0000000100000000") then
      OKtest := Oktest + 1;
    end if;
    assert (v_ar_bv_ptr3(1) & v_ar_bv_ptr2(0)) = "0000000110000000";
    if ((v_ar_bv_ptr3(1) & v_ar_bv_ptr2(0)) = "0000000110000000") then
      OKtest := Oktest + 1;
    end if;
    assert (v_ar_bv_ptr1(1) /= v_ar_bv_ptr3(0)) = true;
    if ((v_ar_bv_ptr1(1) /= v_ar_bv_ptr3(0)) = true) then
      OKtest := Oktest + 1;
    end if;

    assert NOT(OKtest = 9)
      report "***PASSED TEST: c03s03b00x00p05n02i00538" 
      severity NOTE;
    assert (OKtest = 9)
      report "***FAILED TEST: c03s03b00x00p05n02i00538 - An access value belongs to a corresponding subtype of an access type if the value of the designated object satisfies the constraint."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s03b00x00p05n02i00538arch;
