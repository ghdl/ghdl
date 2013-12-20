
-- Copyright (C) 1996 Morgan Kaufmann Publishers, Inc

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
-- $Id: ch_10_bvat-b.vhd,v 1.3 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library bv_utilities;

use std.textio.all, bv_utilities.bv_arithmetic.all;

architecture bench of bv_test is
  
begin

  process is

            variable L : line;
          variable byte : bit_vector(0 to 7);
          variable word : bit_vector(1 to 32);
          variable half_byte : bit_vector(1 to 4);
          variable overflow, div_by_zero, result : boolean;

  begin
    wait for 1 ns;

    ----------------------------------------------------------------
    ----------------------------------------------------------------
    -- test bit_vector to numeric conversions
    ----------------------------------------------------------------
    ----------------------------------------------------------------

    write(L, string'("Testing bv_to_natural:"));
    writeline(output, L);

    write(L, string'("  bv_to_natural(X""02"") = "));
    write(L, bv_to_natural(X"02"));
    writeline(output, L);
    assert bv_to_natural(X"02") = 2;

    write(L, string'("  bv_to_natural(X""FE"") = "));
    write(L, bv_to_natural(X"FE"));
    writeline(output, L);
    assert bv_to_natural(X"FE") = 254;

    ----------------------------------------------------------------

    write(L, string'("Testing natural_to_bv:"));
    writeline(output, L);

    write(L, string'("  natural_to_bv(2) = "));
    write(L, natural_to_bv(2, 8));
    writeline(output, L);
    assert natural_to_bv(2, 8) = X"02";

    write(L, string'("  natural_to_bv(254) = "));
    write(L, natural_to_bv(254, 8));
    writeline(output, L);
    assert natural_to_bv(254, 8) = X"FE";

    ----------------------------------------------------------------

    write(L, string'("Testing bv_to_integer:"));
    writeline(output, L);

    write(L, string'("  bv_to_integer(X""02"") = "));
    write(L, bv_to_integer(X"02"));
    writeline(output, L);
    assert bv_to_integer(X"02") = 2;

    write(L, string'("  bv_to_integer(X""FE"") = "));
    write(L, bv_to_integer(X"FE"));
    writeline(output, L);
    assert bv_to_integer(X"FE") = -2;

    ----------------------------------------------------------------

    write(L, string'("Testing integer_to_bv:"));
    writeline(output, L);

    write(L, string'("  integer_to_bv(2) = "));
    write(L, integer_to_bv(2, 8));
    writeline(output, L);
    assert integer_to_bv(2, 8) = X"02";

    write(L, string'("  integer_to_bv(-2) = "));
    write(L, integer_to_bv(-2, 8));
    writeline(output, L);
    assert integer_to_bv(-2, 8) = X"FE";


    ----------------------------------------------------------------
    ----------------------------------------------------------------
    --  Arithmetic operations
    ----------------------------------------------------------------
    ----------------------------------------------------------------

    ----------------------------------------------------------------
    -- bv_add: Signed addition with overflow detection
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing bv_add with overflow:"));
    writeline(output, L);

    write(L, string'("  2+2 = "));
    bv_add(X"02", X"02", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"04" and not overflow;

    write(L, string'("  2+(-3) = "));
    bv_add(X"02", X"FD", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"FF" and not overflow;

    write(L, string'("  64+64 = "));
    bv_add(X"40", X"40", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"80" and overflow;

    write(L, string'("  -64+(-64) = "));
    bv_add(X"C0", X"C0", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"80" and not overflow;

    ----------------------------------------------------------------
    -- "+": Signed addition without overflow detection
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing ""+"" without overflow:"));
    writeline(output, L);

    write(L, string'("  2+2 = "));
    byte := X"02" + X"02";
    write(L, byte);
    writeline(output, L);
    assert byte = X"04";

    write(L, string'("  2+(-3) = "));
    byte := X"02" + X"FD";
    write(L, byte);
    writeline(output, L);
    assert byte = X"FF";

    write(L, string'("  64+64 = "));
    byte := X"40" + X"40";
    write(L, byte);
    writeline(output, L);
    assert byte = X"80";

    write(L, string'("  -64+(-64) = "));
    byte := X"C0" + X"C0";
    write(L, byte);
    writeline(output, L);
    assert byte = X"80";

    ----------------------------------------------------------------
    -- bv_sub: Signed subtraction with overflow detection
    ----------------------------------------------------------------
    
    writeline(output, L);
    write(L, string'("Testing bv_sub with overflow:"));
    writeline(output, L);

    write(L, string'("  2-2 = "));
    bv_sub(X"02", X"02", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"00" and not overflow;

    write(L, string'("  2-(-3) = "));
    bv_sub(X"02", X"FD", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"05" and not overflow;

    write(L, string'("  64-(-64) = "));
    bv_sub(X"40", X"C0", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"80" and overflow;

    write(L, string'("  -64-64 = "));
    bv_sub(X"C0", X"40", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"80" and not overflow;

    ----------------------------------------------------------------
    -- "-": Signed subtraction without overflow detection
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing ""-"" without overflow:"));
    writeline(output, L);

    write(L, string'("  2-2 = "));
    byte := X"02" - X"02";
    write(L, byte);
    writeline(output, L);
    assert byte = X"00";

    write(L, string'("  2-(-3) = "));
    byte := X"02" - X"FD";
    write(L, byte);
    writeline(output, L);
    assert byte = X"05";

    write(L, string'("  64-(-64) = "));
    byte := X"40" - X"C0";
    write(L, byte);
    writeline(output, L);
    assert byte = X"80";

    write(L, string'("  -64-64 = "));
    byte := X"C0" - X"40";
    write(L, byte);
    writeline(output, L);
    assert byte = X"80";

    ----------------------------------------------------------------
    -- bv_addu:  Unsigned addition with overflow detection
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing bv_addu with overflow:"));
    writeline(output, L);

    write(L, string'("  2+2 = "));
    bv_addu(X"02", X"02", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"04" and not overflow;

    write(L, string'("  64+64 = "));
    bv_addu(X"40", X"40", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"80" and not overflow;

    write(L, string'("  128+128 = "));
    bv_addu(X"80", X"80", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"00" and overflow;

    ----------------------------------------------------------------
    -- bv_addu: Unsigned addition without overflow detection
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing bv_addu without overflow:"));
    writeline(output, L);

    write(L, string'("  2+2 = "));
    byte := bv_addu(X"02", X"02");
    write(L, byte);
    writeline(output, L);
    assert byte = X"04";

    write(L, string'("  64+64 = "));
    byte := bv_addu(X"40", X"40");
    write(L, byte);
    writeline(output, L);
    assert byte = X"80";

    write(L, string'("  128+128 = "));
    byte := bv_addu(X"80", X"80");
    write(L, byte);
    writeline(output, L);
    assert byte = X"00";

    ----------------------------------------------------------------
    -- bv_subu: Unsigned subtraction with overflow detection
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing bv_subu with overflow:"));
    writeline(output, L);

    write(L, string'("  3-2 = "));
    bv_subu(X"03", X"02", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"01" and not overflow;

    write(L, string'("  64-64 = "));
    bv_subu(X"40", X"40", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"00" and not overflow;

    write(L, string'("  64-128 = "));
    bv_subu(X"40", X"80", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"C0" and overflow;

    ----------------------------------------------------------------
    -- bv_subu: Unsigned subtraction without overflow detection
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing bv_subu without overflow:"));
    writeline(output, L);

    write(L, string'("  3-2 = "));
    byte := bv_subu(X"03", X"02");
    write(L, byte);
    writeline(output, L);
    assert byte = X"01";

    write(L, string'("  64-64 = "));
    byte := bv_subu(X"40", X"40");
    write(L, byte);
    writeline(output, L);
    assert byte = X"00";

    write(L, string'("  64-128 = "));
    byte := bv_subu(X"40", X"80");
    write(L, byte);
    writeline(output, L);
    assert byte = X"C0";

    ----------------------------------------------------------------
    -- bv_neg: Signed negation with overflow detection
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing bv_neg with overflow:"));
    writeline(output, L);

    write(L, string'("  -(3) = "));
    bv_neg(X"03", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"FD" and not overflow;

    write(L, string'("  -(-3) = "));
    bv_neg(X"FD", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"03" and not overflow;

    write(L, string'("  -(127) = "));
    bv_neg(X"7F", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"81" and not overflow;

    write(L, string'("  -(-128) = "));
    bv_neg(X"80", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"80" and overflow;

    ----------------------------------------------------------------
    -- "-": Signed negation without overflow detection
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing ""-"" without overflow:"));
    writeline(output, L);

    write(L, string'("  -(3) = "));
    byte := - X"03";
    write(L, byte);
    writeline(output, L);
    assert byte = X"FD";

    write(L, string'("  -(-3) = "));
    byte := - X"FD";
    write(L, byte);
    writeline(output, L);
    assert byte = X"03";

    write(L, string'("  -(127) = "));
    byte := - X"7F";
    write(L, byte);
    writeline(output, L);
    assert byte = X"81";

    write(L, string'("  -(-128) = "));
    byte := - X"80";
    write(L, byte);
    writeline(output, L);
    assert byte = X"80";

    ----------------------------------------------------------------
    -- bv_mult: Signed multiplication with overflow detection
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing bv_mult with overflow:"));
    writeline(output, L);

    write(L, string'("  5*(-3) = "));
    bv_mult(X"05", X"FD", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"F1" and not overflow;

    write(L, string'("  (-5)*(-3) = "));
    bv_mult(X"FB", X"FD", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"0F" and not overflow;

    write(L, string'("  16*8 = "));
    bv_mult(X"10", X"08", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"80" and overflow;

    write(L, string'("  16*16 = "));
    bv_mult(X"10", X"10", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"00" and overflow;

    write(L, string'("  16*(-8) = "));
    bv_mult(X"10", X"F8", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"80" and not overflow;

    write(L, string'("  16*(-16) = "));
    bv_mult(X"10", X"F0", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"00" and overflow;

    ----------------------------------------------------------------
    -- "*":  Signed multiplication without overflow detection
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing ""*"" without overflow:"));
    writeline(output, L);

    write(L, string'("  5*(-3) = "));
    byte := X"05" * X"FD";
    write(L, byte);
    writeline(output, L);
    assert byte = X"F1";

    write(L, string'("  (-5)*(-3) = "));
    byte := X"FB" * X"FD";
    write(L, byte);
    writeline(output, L);
    assert byte = X"0F";

    write(L, string'("  16*8 = "));
    byte := X"10" * X"08";
    write(L, byte);
    writeline(output, L);
    assert byte = X"80";

    write(L, string'("  16*16 = "));
    byte := X"10" * X"10";
    write(L, byte);
    writeline(output, L);
    assert byte = X"00";

    write(L, string'("  16*(-8) = "));
    byte := X"10" * X"F8";
    write(L, byte);
    writeline(output, L);
    assert byte = X"80";

    write(L, string'("  16*(-16) = "));
    byte := X"10" * X"F0";
    write(L, byte);
    writeline(output, L);
    assert byte = X"00";

    ----------------------------------------------------------------
    -- bv_multu:  Unsigned multiplication with overflow detection
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing bv_multu with overflow:"));
    writeline(output, L);

    write(L, string'("  5*7 = "));
    bv_multu(X"05", X"07", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"23" and not overflow;

    write(L, string'("  16*8 = "));
    bv_multu(X"10", X"08", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"80" and not overflow;

    write(L, string'("  16*16 = "));
    bv_multu(X"10", X"10", byte, overflow);
    write(L, byte);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"00" and overflow;

    ----------------------------------------------------------------
    --  bv_multu: Unsigned multiplication without overflow detection
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing bv_multu without overflow:"));
    writeline(output, L);

    write(L, string'("  5*7 = "));
    byte := bv_multu(X"05", X"07");
    write(L, byte);
    writeline(output, L);
    assert byte = X"23";

    write(L, string'("  16*8 = "));
    byte := bv_multu(X"10", X"08");
    write(L, byte);
    writeline(output, L);
    assert byte = X"80";

    write(L, string'("  16*16 = "));
    byte := bv_multu(X"10", X"10");
    write(L, byte);
    writeline(output, L);
    assert byte = X"00";

    ----------------------------------------------------------------
    -- bv_div: Signed division with divide by zero and overflow detection
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing bv_div with flags:"));
    writeline(output, L);

    write(L, string'("  7/2 = "));
    bv_div(X"07", X"02", byte, div_by_zero, overflow);
    write(L, byte);
    write(L, string'(", div_by_zero = "));  write(L, div_by_zero);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"03" and not div_by_zero and not overflow;

    write(L, string'("  -7/2 = "));
    bv_div(X"F9", X"02", byte, div_by_zero, overflow);
    write(L, byte);
    write(L, string'(", div_by_zero = "));  write(L, div_by_zero);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"FD" and not div_by_zero and not overflow;

    write(L, string'("  7/-2 = "));
    bv_div(X"07", X"FE", byte, div_by_zero, overflow);
    write(L, byte);
    write(L, string'(", div_by_zero = "));  write(L, div_by_zero);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"FD" and not div_by_zero and not overflow;

    write(L, string'("  -7/-2 = "));
    bv_div(X"F9", X"FE", byte, div_by_zero, overflow);
    write(L, byte);
    write(L, string'(", div_by_zero = "));  write(L, div_by_zero);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"03" and not div_by_zero and not overflow;

    write(L, string'("  -128/1 = "));
    bv_div(X"80", X"01", byte, div_by_zero, overflow);
    write(L, byte);
    write(L, string'(", div_by_zero = "));  write(L, div_by_zero);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"80" and not div_by_zero and not overflow;

    write(L, string'("  -128/-1 = "));
    bv_div(X"80", X"FF", byte, div_by_zero, overflow);
    write(L, byte);
    write(L, string'(", div_by_zero = "));  write(L, div_by_zero);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"80" and not div_by_zero and overflow;

    write(L, string'("  -16/0 = "));
    bv_div(X"F0", X"00", byte, div_by_zero, overflow);
    write(L, byte);
    write(L, string'(", div_by_zero = "));  write(L, div_by_zero);
    write(L, string'(", overflow = "));  write(L, overflow);
    writeline(output, L);
    assert byte = X"00" and div_by_zero and not overflow;

    ----------------------------------------------------------------
    -- "/": Signed division without divide by zero and overflow detection
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing ""/"" without flags:"));
    writeline(output, L);

    write(L, string'("  7/2 = "));
    byte := X"07" / X"02";
    write(L, byte);
    writeline(output, L);
    assert byte = X"03";

    write(L, string'("  -7/2 = "));
    byte := X"F9" / X"02";
    write(L, byte);
    writeline(output, L);
    assert byte = X"FD";

    write(L, string'("  7/-2 = "));
    byte := X"07" / X"FE";
    write(L, byte);
    writeline(output, L);
    assert byte = X"FD";

    write(L, string'("  -7/-2 = "));
    byte := X"F9" / X"FE";
    write(L, byte);
    writeline(output, L);
    assert byte = X"03";

    write(L, string'("  -128/1 = "));
    byte := X"80" / X"01";
    write(L, byte);
    writeline(output, L);
    assert byte = X"80";

    write(L, string'("  -128/-1 = "));
    byte := X"80" / X"FF";
    write(L, byte);
    writeline(output, L);
    assert byte = X"80";

    write(L, string'("  -16/0 = "));
    byte := X"F0" / X"00";
    write(L, byte);
    writeline(output, L);
    assert byte = X"00";

    ----------------------------------------------------------------
    -- bv_divu: Unsigned division with divide by zero detection
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing bv_divu with flag:"));
    writeline(output, L);

    write(L, string'("  7/2 = "));
    bv_divu(X"07", X"02", byte, div_by_zero);
    write(L, byte);
    write(L, string'(", div_by_zero = "));  write(L, div_by_zero);
    writeline(output, L);
    assert byte = X"03" and not div_by_zero;

    write(L, string'("  14/7 = "));
    bv_divu(X"0E", X"07", byte, div_by_zero);
    write(L, byte);
    write(L, string'(", div_by_zero = "));  write(L, div_by_zero);
    writeline(output, L);
    assert byte = X"02" and not div_by_zero;

    write(L, string'("  16/1 = "));
    bv_divu(X"10", X"01", byte, div_by_zero);
    write(L, byte);
    write(L, string'(", div_by_zero = "));  write(L, div_by_zero);
    writeline(output, L);
    assert byte = X"10" and not div_by_zero;

    write(L, string'("  16/0 = "));
    bv_divu(X"10", X"00", byte, div_by_zero);
    write(L, byte);
    write(L, string'(", div_by_zero = "));  write(L, div_by_zero);
    writeline(output, L);
    assert byte = X"10" and div_by_zero;

    write(L, string'("  16/16 = "));
    bv_divu(X"10", X"10", byte, div_by_zero);
    write(L, byte);
    write(L, string'(", div_by_zero = "));  write(L, div_by_zero);
    writeline(output, L);
    assert byte = X"01" and not div_by_zero;

    write(L, string'("  1/16 = "));
    bv_divu(X"01", X"10", byte, div_by_zero);
    write(L, byte);
    write(L, string'(", div_by_zero = "));  write(L, div_by_zero);
    writeline(output, L);
    assert byte = X"00" and not div_by_zero;

    write(L, string'("  255/1 = "));
    bv_divu(X"FF", X"01", byte, div_by_zero);
    write(L, byte);
    write(L, string'(", div_by_zero = "));  write(L, div_by_zero);
    writeline(output, L);
    assert byte = X"FF" and not div_by_zero;

    ----------------------------------------------------------------
    -- bv_divu: Unsigned division without divide by zero detection
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing bv_divu without flag:"));
    writeline(output, L);

    write(L, string'("  7/2 = "));
    byte := bv_divu(X"07", X"02");
    write(L, byte);
    writeline(output, L);
    assert byte = X"03";

    write(L, string'("  14/7 = "));
    byte := bv_divu(X"0E", X"07");
    write(L, byte);
    writeline(output, L);
    assert byte = X"02";

    write(L, string'("  16/1 = "));
    byte := bv_divu(X"10", X"01");
    write(L, byte);
    writeline(output, L);
    assert byte = X"10";

    write(L, string'("  16/0 = "));
    byte := bv_divu(X"10", X"00");
    write(L, byte);
    writeline(output, L);
    assert byte = X"00";

    write(L, string'("  16/16 = "));
    byte := bv_divu(X"10", X"10");
    write(L, byte);
    writeline(output, L);
    assert byte = X"01";

    write(L, string'("  1/16 = "));
    byte := bv_divu(X"01", X"10");
    write(L, byte);
    writeline(output, L);
    assert byte = X"00";

    write(L, string'("  255/1 = "));
    byte := bv_divu(X"FF", X"01");
    write(L, byte);
    writeline(output, L);
    assert byte = X"FF";


    ----------------------------------------------------------------
    ----------------------------------------------------------------
    --  Arithmetic comparison operators.
    ----------------------------------------------------------------
    ----------------------------------------------------------------

    ----------------------------------------------------------------
    -- bv_lt: Signed less than comparison
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing bv_lt:"));
    writeline(output, L);

    write(L, string'("  2 < 2 = "));
    result := bv_lt(X"02", X"02");
    write(L, result);
    writeline(output, L);
    assert NOT result;

    write(L, string'("  2 < 3 = "));
    result := bv_lt(X"02", X"03");
    write(L, result);
    writeline(output, L);
    assert result;

    write(L, string'("  -2 < 2 = "));
    result := bv_lt(X"FE", X"02");
    write(L, result);
    writeline(output, L);
    assert result;

    write(L, string'("  2 < -3 = "));
    result := bv_lt(X"02", X"FD");
    write(L, result);
    writeline(output, L);
    assert NOT result;

    ----------------------------------------------------------------
    -- bv_le: Signed less than or equal comparison
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing bv_le:"));
    writeline(output, L);

    write(L, string'("  2 <= 2 = "));
    result := bv_le(X"02", X"02");
    write(L, result);
    writeline(output, L);
    assert result;

    write(L, string'("  2 <= 3 = "));
    result := bv_le(X"02", X"03");
    write(L, result);
    writeline(output, L);
    assert result;

    write(L, string'("  -2 <= 2 = "));
    result := bv_le(X"FE", X"02");
    write(L, result);
    writeline(output, L);
    assert result;

    write(L, string'("  2 <= -3 = "));
    result := bv_le(X"02", X"FD");
    write(L, result);
    writeline(output, L);
    assert NOT result;

    ----------------------------------------------------------------
    -- bv_gt: Signed greater than comparison
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing bv_gt:"));
    writeline(output, L);

    write(L, string'("  2 > 2 = "));
    result := bv_gt(X"02", X"02");
    write(L, result);
    writeline(output, L);
    assert NOT result;

    write(L, string'("  3 > 2 = "));
    result := bv_gt(X"03", X"02");
    write(L, result);
    writeline(output, L);
    assert result;

    write(L, string'("  2 > -2 = "));
    result := bv_gt(X"02", X"FE");
    write(L, result);
    writeline(output, L);
    assert result;

    write(L, string'("  -3 > 2 = "));
    result := bv_gt(X"FD", X"02");
    write(L, result);
    writeline(output, L);
    assert NOT result;

    ----------------------------------------------------------------
    -- bv_ge: Signed greater than or equal comparison
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing bv_ge:"));
    writeline(output, L);

    write(L, string'("  2 >= 2 = "));
    result := bv_ge(X"02", X"02");
    write(L, result);
    writeline(output, L);
    assert result;

    write(L, string'("  3 >= 2 = "));
    result := bv_ge(X"03", X"02");
    write(L, result);
    writeline(output, L);
    assert result;

    write(L, string'("  2 >= -2 = "));
    result := bv_ge(X"02", X"FE");
    write(L, result);
    writeline(output, L);
    assert result;

    write(L, string'("  -3 >= 2 = "));
    result := bv_ge(X"FD", X"02");
    write(L, result);
    writeline(output, L);
    assert NOT result;

    ----------------------------------------------------------------
    ----------------------------------------------------------------
    --  Extension operators - convert a bit vector to a longer one
    ----------------------------------------------------------------
    ----------------------------------------------------------------

    ----------------------------------------------------------------
    -- bv_sext: Sign extension
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing bv_sext:"));
    writeline(output, L);

    write(L, string'("  sext(X""02"", 32) = "));
    word := bv_sext(X"02", 32);
    write(L, word);
    writeline(output, L);
    assert word = X"00000002";

    write(L, string'("  sext(X""FE"", 32) = "));
    word := bv_sext(X"FE", 32);
    write(L, word);
    writeline(output, L);
    assert word = X"FFFFFFFE";

    write(L, string'("  sext(X""02"", 8) = "));
    byte := bv_sext(X"02", 8);
    write(L, byte);
    writeline(output, L);
    assert byte = X"02";

    write(L, string'("  sext(X""FE"", 8) = "));
    byte := bv_sext(X"FE", 8);
    write(L, byte);
    writeline(output, L);
    assert byte = X"FE";

    write(L, string'("  sext(X""02"", 4) = "));
    half_byte := bv_sext(X"02", 4);
    write(L, half_byte);
    writeline(output, L);
    assert half_byte = X"2";

    write(L, string'("  sext(X""FE"", 4) = "));
    half_byte := bv_sext(X"FE", 4);
    write(L, half_byte);
    writeline(output, L);
    assert half_byte = X"E";

    ----------------------------------------------------------------
    -- bv_zext" Zero extension
    ----------------------------------------------------------------

    writeline(output, L);
    write(L, string'("Testing bv_zext:"));
    writeline(output, L);

    write(L, string'("  zext(X""02"", 32) = "));
    word := bv_zext(X"02", 32);
    write(L, word);
    writeline(output, L);
    assert word = X"00000002";

    write(L, string'("  zext(X""FE"", 32) = "));
    word := bv_zext(X"FE", 32);
    write(L, word);
    writeline(output, L);
    assert word = X"000000FE";

    write(L, string'("  zext(X""02"", 8) = "));
    byte := bv_zext(X"02", 8);
    write(L, byte);
    writeline(output, L);
    assert byte = X"02";

    write(L, string'("  zext(X""FE"", 8) = "));
    byte := bv_zext(X"FE", 8);
    write(L, byte);
    writeline(output, L);
    assert byte = X"FE";

    write(L, string'("  zext(X""02"", 4) = "));
    half_byte := bv_zext(X"02", 4);
    write(L, half_byte);
    writeline(output, L);
    assert half_byte = X"2";

    write(L, string'("  zext(X""FE"", 4) = "));
    half_byte := bv_zext(X"FE", 4);
    write(L, half_byte);
    writeline(output, L);
    assert half_byte = X"E";


    wait;
  end process;

end architecture bench;
