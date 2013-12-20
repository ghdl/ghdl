
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
-- $Id: tc1944.vhd,v 1.2 2001-10-26 16:29:44 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b01x00p01n04i01944ent IS
END c07s02b01x00p01n04i01944ent;

ARCHITECTURE c07s02b01x00p01n04i01944arch OF c07s02b01x00p01n04i01944ent IS

BEGIN
  TESTING: PROCESS
    variable A : bit_vector (1 to 32);
    variable B : bit_vector (32 downto 1);
    constant AA : bit_vector (1 to 32) := x"0000ffff";
    variable C : bit_vector (15 downto 0);
    variable D, DD : bit_vector (0 to 15);
    variable E : bit_vector (0 to 47);
    variable F : bit_vector (47 downto 0);
    alias FF : bit_vector (47 downto 0) is F;
  BEGIN
    
    A := x"0000ffff";
    B := x"00ff00ff";

    C := x"00ff";
    D := x"0f0f";

    E := x"000000ffffff";
    F := x"000fff000fff";

    assert NOT(    (A and B ) = x"000000ff"   and
                   (A or  B ) = x"00ffffff"   and
                   (A xor B ) = x"00ffff00"   and
                   (A nand B) = x"ffffff00"   and
                   (A nor B ) = x"ff000000"   and
                   (not A   ) = x"ffff0000"   and
                   (AA and B ) = x"000000ff"   and
                   (AA or  B ) = x"00ffffff"   and
                   (AA xor B ) = x"00ffff00"   and
                   (AA nand B) = x"ffffff00"   and
                   (AA nor B ) = x"ff000000"   and
                   (not AA   ) = x"ffff0000"   and
                   (C and D ) = x"000f"   and
                   (C or  D ) = x"0fff"   and
                   (C xor D ) = x"0ff0"   and
                   (C nand D) = x"fff0"   and
                   (C nor D ) = x"f000"   and
                   (not C   ) = x"ff00"   and
                   (E and F ) = x"000000000fff"   and
                   (E or  F ) = x"000fffffffff"   and
                   (E xor F ) = x"000ffffff000"   and
                   (E nand F) = x"fffffffff000"   and
                   (E nor F ) = x"fff000000000"   and
                   (E and FF ) = x"000000000fff"   and
                   (E or  FF ) = x"000fffffffff"   and
                   (E xor FF ) = x"000ffffff000"   and
                   (E nand FF) = x"fffffffff000"   and
                   (E nor FF ) = x"fff000000000"   and
                   (not E   ) = x"ffffff000000")
      report "***PASSED TEST: c07s02b01x00p01n04i01944" 
      severity NOTE;
    assert (    (A and B ) = x"000000ff"   and
                (A or  B ) = x"00ffffff"   and
                (A xor B ) = x"00ffff00"   and
                (A nand B) = x"ffffff00"   and
                (A nor B ) = x"ff000000"   and
                (not A   ) = x"ffff0000"   and
                (AA and B ) = x"000000ff"   and
                (AA or  B ) = x"00ffffff"   and
                (AA xor B ) = x"00ffff00"   and
                (AA nand B) = x"ffffff00"   and
                (AA nor B ) = x"ff000000"   and
                (not AA   ) = x"ffff0000"   and
                (C and D ) = x"000f"   and
                (C or  D ) = x"0fff"   and
                (C xor D ) = x"0ff0"   and
                (C nand D) = x"fff0"   and
                (C nor D ) = x"f000"   and
                (not C   ) = x"ff00"   and
                (E and F ) = x"000000000fff"   and
                (E or  F ) = x"000fffffffff"   and
                (E xor F ) = x"000ffffff000"   and
                (E nand F) = x"fffffffff000"   and
                (E nor F ) = x"fff000000000"   and
                (E and FF ) = x"000000000fff"   and
                (E or  FF ) = x"000fffffffff"   and
                (E xor FF ) = x"000ffffff000"   and
                (E nand FF) = x"fffffffff000"   and
                (E nor FF ) = x"fff000000000"   and
                (not E   ) = x"ffffff000000")
      report "***FAILED TEST: c07s02b01x00p01n04i01944 - One dimensional array type logical operation failed." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b01x00p01n04i01944arch;
