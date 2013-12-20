
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
-- $Id: tc1386.vhd,v 1.2 2001-10-26 16:29:40 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s05b00x00p03n02i01386ent IS
END c08s05b00x00p03n02i01386ent;

ARCHITECTURE c08s05b00x00p03n02i01386arch OF c08s05b00x00p03n02i01386ent IS

BEGIN
  TESTING : PROCESS   

    variable radix : natural := 10;
    variable v1 : natural;

    type r_array_index_type is range 1 to 3;
    type r_array_type is array (r_array_index_type) of natural;
    variable r_array : r_array_type;

    procedure set_radix ( constant radix : natural
                          ) is
    begin
      TESTING.radix := radix;             -- test selected name as target
    end set_radix;

  BEGIN
    v1 := 8;   --test simple name as target
    assert v1 = 8
      report "Simple name as target failed."
      severity note ;

    set_radix (v1);
    assert radix = v1
      report "Selected name as target failed."
      severity note ;

    r_array ( 3 to 3 ) := (3 => 10);          -- test slice name as target
    assert r_array ( 3 ) = 10
      report "Slice name as target failed."
      severity note ;

    r_array ( 2 ) := 8;               -- test indexed name as target
    assert r_array ( 2 ) = 8
      report "Indexed name as target failed."
      severity note ;

    assert NOT(v1=8 and r_array(3)=10 and r_array(2)=8) 
      report "***PASSED TEST: c08s05b00x00p03n02i01386" 
      severity NOTE;
    assert (v1=8 and r_array(3)=10 and r_array(2)=8) 
      report "***FAILED TEST: c08s05b00x00p03n02i01386 - The name of thetarget of the variable assignment statement must denote a variable"
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s05b00x00p03n02i01386arch;
