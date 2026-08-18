#! /bin/sh

. ../../testenv.sh

analyze ent.vhd
analyze ent2.vhd

# ent's port default value (3) is out of the range (1 to max) once max is
# resolved to 2 via ent2's generic map. The error must be reported where
# the out-of-range default value actually is (ent.vhd:5), not at the
# instantiation site (ent2.vhd:5) -- see issue 562.
out=$(elab_simulate_failure ent2 2>&1)
echo "$out"

# Only the "error:" lines carry the location being tested. On gcc/llvm the
# runtime error is followed by GRT's elaboration backtrace ("from: ... at
# ent2.vhd:5"), which legitimately names ent2.vhd -- that really is where
# the instantiation is -- and must not be mistaken for the reported error
# location. That backtrace is printed only when the design was built with
# usable debug info, so it is absent from some local builds and present in
# CI; matching on it either way would make this test environment-dependent.
errors=$(echo "$out" | grep "error:")

# The exact message layout differs by backend: mcode folds the check
# statically and reports "ent.vhd:5:39:error: literal out of range" during
# elaboration, while gcc/llvm defer to a GRT runtime check that reports
# "<executable>:error: bound check failure at ent.vhd:5". Match on
# substrings only, so both styles are accepted.
if echo "$errors" | grep -q "ent2.vhd:"; then
  echo "FAIL: bound-check error wrongly attributed to ent2.vhd (the instantiation site) instead of ent.vhd (where the out-of-range port default value actually is)"
  exit 1
fi

if ! echo "$errors" | grep -q "ent.vhd:5"; then
  echo "FAIL: expected the bound-check error to be reported at ent.vhd:5"
  exit 1
fi

clean

echo "Test successful"
