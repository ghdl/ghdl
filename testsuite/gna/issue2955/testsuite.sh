#! /bin/sh

. ../../testenv.sh

# An entity port of a nested array type (signed_vector, array of
# unconstrained signed) left partially or fully unconstrained (only
# constrained via the actual at instantiation) used to crash
# analysis/elaboration with an internal ASSERTION_ERROR
# (synth-vhdl_expr.adb:635). See issue2955.
export GHDL_STD_FLAGS="--std=08"
analyze pkg.vhd dummy.vhd top.vhd

out=$(elab_simulate top --fst=waves.fst 2>&1)
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: crashed instead of completing"
  exit 1
fi

if ! echo "$out" | grep -q "simulation finished"; then
  echo "FAIL: expected the simulation to complete"
  exit 1
fi

rm -f waves.fst
clean

echo "Test successful"
