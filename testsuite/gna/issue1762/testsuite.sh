#! /bin/sh

. ../../testenv.sh

# A conversion function on a port association ("input => bv(s_result)")
# whose formal is an unconstrained port hit an over-strict assertion in
# Elab_Conversion: the output is unbounded there while the actual is
# bounded, which the code assumed could not happen.  Only the gcc and llvm
# backends use that translator, and only a build with assertions enabled
# stops on it -- with --disable-checks GHDL silently carried on with a
# violated precondition, which is why this looked fixed.
# tb.vhd therefore checks the values that reach the registers, not just
# that the design analyzes.  See issue1762.
analyze test.vhd
analyze tb.vhd

out=$(elab_simulate tb_register_bank 2>&1)
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: crashed instead of elaborating"
  exit 1
fi

# 48879 = 16#BEEF#, 4660 = 16#1234#, written through the converted port.
if ! echo "$out" | grep -q "PASS a_out=48879 b_out=4660"; then
  echo "FAIL: wrong data through the conversion-function association"
  exit 1
fi

# Same crash, but with a conversion target whose range and direction differ
# from the actual's, which the report's own design cannot distinguish.
analyze conv_range.vhdl

out=$(elab_simulate tb_conv_range 2>&1)
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: crashed on a conversion to a differently-ranged subtype"
  exit 1
fi

# 16#F001# through the conversion, read from 'LEFT to 'RIGHT.
if ! echo "$out" | grep -q "pos(left..right)=1111000000000001 len=16"; then
  echo "FAIL: wrong data or length through the differently-ranged conversion"
  exit 1
fi

clean

echo "Test successful"
