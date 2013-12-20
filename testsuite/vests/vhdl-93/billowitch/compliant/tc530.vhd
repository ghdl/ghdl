
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
-- $Id: tc530.vhd,v 1.2 2001-10-26 16:29:56 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s03b00x00p03n04i00530ent IS
END c03s03b00x00p03n04i00530ent;

ARCHITECTURE c03s03b00x00p03n04i00530arch OF c03s03b00x00p03n04i00530ent IS

BEGIN
  TESTING : PROCESS

    type small is
      record
        bt : bit;
        bv : bit_vector (11 downto 0);
        r  : real range 0.0 to real'high;
        bb : boolean;
        i  : integer range 1 to 20;
      end record;
    type color    is (red, green, blue);
    type two_d    is array (color, 1 to 3) of bit;
    type smar    is array (1 to 3) of small;
    
    type record_ptr is access small;
    type smar_ptr    is access smar;
    type two_d_ptr    is access two_d;

    variable v_record_ptr1: record_ptr := new small'(bt => '1',
                                                     bv => "010101010101",
                                                     r  => 0.1234,
                                                     bb => true,
                                                     i  => 20);
    variable v_record_ptr2: record_ptr;
    variable v_record_ptr3: record_ptr := v_record_ptr1;
    variable v_record_ptr4: record_ptr := new small'(bt => '0',
                                                     bv => "010101010101",
                                                     r  => 0.9999,
                                                     bb => false,
                                                     i  => 1);
    
    variable v_smar_ptr1: smar_ptr := new smar'(others => (bt => '1',
                                                           bv => "010101010101",
                                                           r  => 0.1234,
                                                           bb => true,
                                                           i  => 20));
    variable v_smar_ptr2: smar_ptr;
    variable v_smar_ptr3: smar_ptr := v_smar_ptr1;
    variable v_smar_ptr4: smar_ptr := new smar'(1 => (bt => '1',
                                                      bv => "010101010101",
                                                      r  => 0.1234,
                                                      bb => true,
                                                      i  => 20),
                                                others => (bt => '0',
                                                           bv => "010101010101",
                                                           r  => 0.9999,
                                                           bb => false,
                                                           i  => 1));
    
    variable v_two_d_ptr1: two_d_ptr := new two_d'(others => (others => '1'));
    variable v_two_d_ptr2: two_d_ptr;
    variable v_two_d_ptr3: two_d_ptr := v_two_d_ptr1;
    variable v_two_d_ptr4: two_d_ptr := new two_d'(red   => "111",
                                                   green => "000",
                                                   blue  => "101");

    variable OKtest : integer := 0;

  BEGIN

    assert v_record_ptr1.bt = '1';
    if (v_record_ptr1.bt = '1') then
      OKtest := OKtest + 1;
    end if;
    assert v_record_ptr1.bv = "010101010101";
    if (v_record_ptr1.bv = "010101010101") then
      OKtest := OKtest + 1;
    end if;
    assert v_record_ptr1.r = 0.1234;
    if (v_record_ptr1.r = 0.1234) then
      OKtest := OKtest + 1;
    end if;
    assert v_record_ptr1.bb = true;
    if (v_record_ptr1.bb = true) then
      OKtest := OKtest + 1;
    end if;
    assert v_record_ptr1.i = 20;
    if (v_record_ptr1.i = 20) then
      OKtest := OKtest + 1;
    end if;
    assert v_record_ptr2 = null;
    if (v_record_ptr2 = null) then
      OKtest := OKtest + 1;
    end if;
    
    v_record_ptr2 := new small'(bt => '0',
                                bv => "010101010101",
                                r  => 0.1234,
                                bb => true,
                                i  => 10);
    
    assert v_record_ptr2.bt = '0';
    if (v_record_ptr2.bt = '0') then
      OKtest := Oktest + 1;
    end if;
    assert v_record_ptr2.bv = "010101010101";
    if (v_record_ptr2.bv = "010101010101") then
      OKtest := Oktest + 1;
    end if;
    assert v_record_ptr2.r = 0.1234;
    if (v_record_ptr2.r = 0.1234) then
      OKtest := Oktest + 1;
    end if;
    assert v_record_ptr2.bb = true;
    if (v_record_ptr2.bb = true) then
      OKtest := Oktest + 1;
    end if;
    assert v_record_ptr2.i = 10;
    if (v_record_ptr2.i = 10) then
      OKtest := Oktest + 1;
    end if;
    assert v_record_ptr2 /= null;
    if (v_record_ptr2 /= null) then
      OKtest := Oktest + 1;
    end if;
    
    assert v_record_ptr1.all = v_record_ptr3.all;
    if (v_record_ptr1.all = v_record_ptr3.all) then
      OKtest := Oktest + 1;
    end if;
    assert v_record_ptr1.all /= v_record_ptr4.all;
    if (v_record_ptr1.all /= v_record_ptr4.all) then
      OKtest := Oktest + 1;
    end if;
    assert (v_record_ptr1.bt & v_record_ptr4.bt) = "10";
    if ((v_record_ptr1.bt & v_record_ptr4.bt) = "10") then
      OKtest := Oktest + 1;
    end if;
    assert (v_record_ptr1.i - v_record_ptr4.i) = 19;
    if ((v_record_ptr1.i - v_record_ptr4.i) = 19) then
      OKtest := Oktest + 1;
    end if;
    
    deallocate(v_record_ptr1);
    deallocate(v_record_ptr2);
    deallocate(v_record_ptr4);
    
    assert v_smar_ptr1(1).bt = '1';
    if (v_smar_ptr1(1).bt = '1') then
      OKtest := Oktest + 1;
    end if;
    assert v_smar_ptr1(1).bv = "010101010101";
    if (v_smar_ptr1(1).bv = "010101010101") then
      OKtest := Oktest + 1;
    end if;
    assert v_smar_ptr1(1).r = 0.1234;
    if (v_smar_ptr1(1).r = 0.1234) then
      OKtest := Oktest + 1;
    end if;
    assert v_smar_ptr1(1).bb = true;
    if (v_smar_ptr1(1).bb = true) then
      OKtest := Oktest + 1;
    end if;
    assert v_smar_ptr1(1).i = 20;
    if (v_smar_ptr1(1).i = 20) then
      OKtest := Oktest + 1;
    end if;
    assert v_smar_ptr2 = null;
    if (v_smar_ptr2 = null) then
      OKtest := Oktest + 1;
    end if;
    
    v_smar_ptr2 := new smar'(others => (bt => '1',
                                        bv => "010101010101",
                                        r  => 0.1234,
                                        bb => true,
                                        i  => 10));
    
    assert v_smar_ptr2(1).bt = '1';
    if (v_smar_ptr2(1).bt = '1') then
      OKtest := Oktest + 1;
    end if;
    assert v_smar_ptr2(1).bv = "010101010101";
    if (v_smar_ptr2(1).bv = "010101010101") then
      OKtest := Oktest + 1;
    end if;
    assert v_smar_ptr2(1).r = 0.1234;
    if (v_smar_ptr2(1).r = 0.1234) then
      OKtest := Oktest + 1;
    end if;
    assert v_smar_ptr2(1).bb = true;
    if (v_smar_ptr2(1).bb = true) then
      OKtest := Oktest + 1;
    end if;
    assert v_smar_ptr2(1).i = 10;
    if (v_smar_ptr2(1).i = 10) then
      OKtest := Oktest + 1;
    end if;
    assert v_smar_ptr2 /= null;
    if (v_smar_ptr2 /= null) then
      OKtest := Oktest + 1;
    end if;
    
    assert v_smar_ptr1.all = v_smar_ptr3.all;
    if (v_smar_ptr1.all = v_smar_ptr3.all) then
      OKtest := Oktest + 1;
    end if;
    assert v_smar_ptr1(1) /= v_smar_ptr4(2);
    if (v_smar_ptr1(1) /= v_smar_ptr4(2)) then
      OKtest := Oktest + 1;
    end if;
    assert v_smar_ptr1(1) = v_smar_ptr4(1);
    if (v_smar_ptr1(1) = v_smar_ptr4(1)) then
      OKtest := Oktest + 1;
    end if;
    assert (v_smar_ptr1(1).bt & v_smar_ptr4(1).bt) = "11";
    if ((v_smar_ptr1(1).bt & v_smar_ptr4(1).bt) = "11") then
      OKtest := Oktest + 1;
    end if;
    assert (v_smar_ptr1(1).i - v_smar_ptr4(1).i) = 0;
    if ((v_smar_ptr1(1).i - v_smar_ptr4(1).i) = 0) then
      OKtest := Oktest + 1;
    end if;

    deallocate(v_smar_ptr1);
    deallocate(v_smar_ptr2);
    deallocate(v_smar_ptr4);
    
    assert v_two_d_ptr1.all = v_two_d_ptr3.all;
    if (v_two_d_ptr1.all = v_two_d_ptr3.all) then
      OKtest := Oktest + 1;
    end if;
    assert v_two_d_ptr1.all /= v_two_d_ptr4.all;
    if (v_two_d_ptr1.all /= v_two_d_ptr4.all) then
      OKtest := Oktest + 1;
    end if;
    assert v_two_d_ptr2 = null;
    if (v_two_d_ptr2 = null) then
      OKtest := Oktest + 1;
    end if;
    assert v_two_d_ptr3(blue, 2) = '1';
    if (v_two_d_ptr3(blue, 2) = '1') then
      OKtest := Oktest + 1;
    end if;
    
    v_two_d_ptr2 := new two_d'(red   => "111",
                               green => "000",
                               blue  => "101");
    
    assert v_two_d_ptr2.all = v_two_d_ptr4.all;
    if (v_two_d_ptr2.all = v_two_d_ptr4.all) then
      OKtest := Oktest + 1;
    end if;
    assert v_two_d_ptr2 /= null;
    if (v_two_d_ptr2 /= null) then
      OKtest := Oktest + 1;
    end if;
    
    assert (v_two_d_ptr1(red, 1) & v_two_d_ptr4(blue, 2)) = "10";
    if ((v_two_d_ptr1(red, 1) & v_two_d_ptr4(blue, 2)) = "10") then
      OKtest := Oktest + 1;
    end if;
    assert (v_two_d_ptr1(red, 1) /= v_two_d_ptr4(blue,2));
    if ((v_two_d_ptr1(red, 1) /= v_two_d_ptr4(blue,2))) then
      OKtest := Oktest + 1;
    end if;
    
    deallocate(v_two_d_ptr1);
    deallocate(v_two_d_ptr2);
    deallocate(v_two_d_ptr4);

    assert NOT(OKtest = 41) 
      report "***PASSED TEST: c03s03b00x00p03n04i00530" 
      severity NOTE;
    assert (OKtest = 41) 
      report "***FAILED TEST: c03s03b00x00p03n04i00530 - Composite type using as base for access type test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s03b00x00p03n04i00530arch;
