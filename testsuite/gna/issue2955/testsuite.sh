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
# The element subtype is taken from the individual associations, and LRM08
# 5.3.2.1 has all the elements of an array share one subtype, so they must
# all define the same one.  See issue2955.
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

#  Two associations that do not define the same element subtype: rejected
#  during analysis, since the bounds are locally static.
out=$(analyze_failure mismatch.vhd 2>&1)
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: crashed instead of reporting the mismatch"
  exit 1
fi

if ! echo "$out" | grep -q "element subtype of individual association"; then
  echo "FAIL: expected the element subtype mismatch error"
  exit 1
fi

#  The same on a generic, which is the worst-behaved shape: without the
#  check the second value is silently truncated and read back as 0, with no
#  diagnostic and a zero exit status.
out=$(analyze_failure gen_mismatch.vhd 2>&1)
echo "$out"

if ! echo "$out" | grep -q "element subtype of individual association"; then
  echo "FAIL: expected the element subtype mismatch error on a generic"
  exit 1
fi

#  The same mismatch with bounds that are not locally static cannot be seen
#  during analysis.  It must still be an ordinary elaboration error rather
#  than an internal one.
analyze dyn_mismatch.vhd

out=$(elab_simulate tb_dyn 2>&1) || true
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: internal error instead of a length mismatch diagnostic"
  exit 1
fi

if ! echo "$out" | grep -qE "length of actual doesn't match|bound check failure"; then
  echo "FAIL: expected a length mismatch diagnostic"
  exit 1
fi

#  A slice as the formal part is a single association element, and what it
#  gives is the element subtype of its own actual, not the array subtype of
#  it.  This shape is still not fully supported -- gcc and llvm end in a
#  bound check failure, as they did before -- but it must not be an internal
#  error, which is what mcode used to answer.
analyze slice.vhd

out=$(elab_simulate tb_slice 2>&1) || true
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: internal error on the slice formal association"
  exit 1
fi

#  A conversion on the formal part of an out port is legal, and what it
#  gives is the parameter subtype of the conversion, which is not what the
#  conversion node carries here -- so such an association must simply be
#  left out of the comparison rather than compared against the wrong type.
analyze conv.vhd

out=$(elab_simulate tb_conv 2>&1)
echo "$out"

if ! echo "$out" | grep -q "x8len=8 y4len=4"; then
  echo "FAIL: a conversion on the formal part is no longer accepted"
  exit 1
fi

#  Same length but opposite direction is legal and must keep working.
analyze dir.vhd

out=$(elab_simulate tb_dir 2>&1)
echo "$out"

if ! echo "$out" | grep -q "PASS s=8"; then
  echo "FAIL: the reversed element association no longer works"
  exit 1
fi

clean

echo "Test successful"
