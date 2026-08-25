#! /bin/sh

. ../../testenv.sh

# A port of a nested array type whose element is unconstrained
# ("array (natural range <>) of signed", port "signed_vector(0 to N-1)")
# associated element by element -- "i_data(0) => i_data" -- failed
# elaboration with a spurious "bound check failure" on the gcc and llvm
# backends.  The individual association synthesized an actual subtype that
# was marked fully constrained while its element was still unconstrained,
# so the bounds of the element were never elaborated.
#
# repro.vhd is the reporter's own design; chk.vhd checks that the data
# really crosses the indexed association, since not crashing is not enough
# to show the element bounds are right.  See issue2955.
export GHDL_STD_FLAGS=--std=08

analyze repro.vhd

out=$(elab_simulate aaa_test_tle 2>&1)
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: crashed"
  exit 1
fi

if echo "$out" | grep -q "bound check failure"; then
  echo "FAIL: spurious bound check on the indexed formal association"
  exit 1
fi

analyze chk.vhd

out=$(elab_simulate tb_chk2955 2>&1)
echo "$out"

if ! echo "$out" | grep -q "PASS b=9876 len=16"; then
  echo "FAIL: wrong data or element length through the indexed association"
  exit 1
fi

clean

echo "Test successful"
