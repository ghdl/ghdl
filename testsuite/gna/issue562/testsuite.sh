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

if echo "$out" | grep -q "^ent2.vhd:"; then
  echo "FAIL: bound-check error wrongly attributed to ent2.vhd (the instantiation site) instead of ent.vhd (where the out-of-range port default value actually is)"
  exit 1
fi

if ! echo "$out" | grep -q "^ent.vhd:5:"; then
  echo "FAIL: expected the bound-check error to be reported at ent.vhd:5"
  exit 1
fi

clean

echo "Test successful"
