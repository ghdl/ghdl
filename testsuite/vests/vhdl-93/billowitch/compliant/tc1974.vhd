
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
-- $Id: tc1974.vhd,v 1.2 2001-10-26 16:29:44 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b01x00p02n02i01974ent IS
END c07s02b01x00p02n02i01974ent;

ARCHITECTURE c07s02b01x00p02n02i01974arch OF c07s02b01x00p02n02i01974ent IS

BEGIN
  TESTING: PROCESS
    constant L : BIT_VECTOR(1 to 4) := "0101";
    constant R : BIT_VECTOR(1 to 4) := "0011";

    constant N  : BIT_VECTOR(1 TO 4) := not L;
    constant A  : BIT_VECTOR(1 TO 4) := L and R;
    constant O  : BIT_VECTOR(1 TO 4) := L or R;
    constant NA : BIT_VECTOR(1 TO 4) := L nand R;
    constant NO : BIT_VECTOR(1 TO 4) := L nor R;
    constant X  : BIT_VECTOR(1 TO 4) := L xor R;
  BEGIN

    assert N  = "1010"      report "FAIL: NOT";
    assert A  = "0001"      report "FAIL: AND";
    assert O  = "0111"      report "FAIL: OR";
    assert NA = "1110"      report "FAIL: NAND";
    assert NO = "1000"      report "FAIL: NOR";
    assert X  = "0110"      report "FAIL: XOR";

    assert N = not L        report "FAIL: NOT (composite check)";
    assert A  = (L and R)   report "FAIL: AND (composite check)";
    assert O  = (L or R)    report "FAIL: OR (composite check)";
    assert NA = (L nand R)  report "FAIL: NAND (composite check)";
    assert NO = (L nor R)   report "FAIL: NOR (composite check)";
    assert X  = (L xor R)   report "FAIL: XOR (composite check)";
    
    for i in 1 to 4 loop
      assert N(i)  = not L(i)             report "FAIL: NOT";
      assert A(i)  = (L(i) and R(i))      report "FAIL: AND";
      assert O(i)  = (L(i) or R(i))       report "FAIL: OR";
      assert NA(i) = (L(i) nand R(i))     report "FAIL: NAND";
      assert NO(i) = (L(i) nor R(i))      report "FAIL: NOR";
      assert X(i)  = (L(i) xor R(i))      report "FAIL: XOR";
    end loop;

    assert NOT(    N  = "1010"         and 
                   A  = "0001"         and 
                   O  = "0111"         and
                   NA = "1110"         and
                   NO = "1000"         and
                   X  = "0110"         and
                   N  = not L           and 
                   A  = (L and R)      and 
                   O  = (L or R)       and 
                   NA = (L nand R)     and 
                   NO = (L nor R)      and 
                   X  = (L xor R)      )
      report "***PASSED TEST: c07s02b01x00p02n02i01974"
      severity NOTE;
    assert (    N  = "1010"         and 
                A  = "0001"         and 
                O  = "0111"         and
                NA = "1110"         and
                NO = "1000"         and
                X  = "0110"         and
                N  = not L           and 
                A  = (L and R)      and 
                O  = (L or R)       and 
                NA = (L nand R)     and 
                NO = (L nor R)      and 
                X  = (L xor R)      )
      report "***FAILED TEST: c07s02b01x00p02n02i01974 - BIT_VECTOR type truth table test failed." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b01x00p02n02i01974arch;
