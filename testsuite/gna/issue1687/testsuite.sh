#! /bin/sh

. ../../testenv.sh

# ent.vhdl calls RC_Add_n, whose formal A is an unconstrained bit_vector,
# with a single slice as the formal designator:
#
#   result <= RC_Add_n(A(3 downto 0) => A(3 downto 0), B => B, Cin => Cin);
#
# LRM08 5.3.2.2 e) 2) gives the index range of an interface object that is
# associated by a slice name -- or by more than one association element --
# the direction of the corresponding index subtype of its base type.  For
# bit_vector that is 'natural', so the object is ascending and this
# 'downto' slice of it is an error (LRM08 8.5).  Written 'A(0 to 3) => ...'
# it is accepted.
#
# This is the same rule as in issue2765 and issue2688.  The test expected
# a failure until a93226f23 made a single slice give its own direction to
# the object, which is the behaviour that has been removed again.
analyze pkg.vhdl ent.vhdl

out=$(elab_simulate_failure rc_add_n_f 2>&1)
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: crashed instead of reporting the slice direction error"
  exit 1
fi

if ! echo "$out" | grep -q "slice direction doesn't match"; then
  echo "FAIL: expected the slice direction error"
  exit 1
fi

# The same call with the formal designator written ascending is legal.
analyze ok.vhdl
elab_simulate rc_add_n_f_ok

clean

echo "Test successful"
