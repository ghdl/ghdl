#! /bin/sh

. ../../testenv.sh

# A custom resolution function applied to a signal of an array-of-array
# type (mx_ivec_arr, via subtype resoba) used to crash at runtime with an
# internal CONSTRAINT_ERROR ("access check failed"). The test's own
# self-check ends the simulation via "report ... severity failure" once
# all resolved-signal assertions have passed (its "Test passed ..."
# message is the expected, successful outcome, not a real failure --
# hence run_failure below). See issue2201.
export GHDL_STD_FLAGS="--std=08"

analyze test.vhdl

out=$(elab_simulate_failure unc_subt2 2>&1)
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: simulation crashed instead of reaching the expected outcome"
  exit 1
fi

if ! echo "$out" | grep -q "Test passed \.\.\."; then
  echo "FAIL: expected the \"Test passed ...\" self-check message"
  exit 1
fi

clean

echo "Test successful"
