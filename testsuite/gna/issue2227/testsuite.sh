#! /bin/sh

. ../../testenv.sh

# The assignment target aggregate uses a discrete-range choice
# ("4 to 8 => k"), which LRM93 8.4 disallows for a target -- Check_Target
# correctly rejects it. But Fill_Array_From_Aggregate_Associated (which
# fills an array sized by the *valid* choice count) used to walk every
# choice unconditionally, overrunning that array and crashing with an
# internal CONSTRAINT_ERROR (index check failed) right after the correct
# error was already reported. See issue2227.
export GHDL_STD_FLAGS="--std=08"

out=$(analyze_failure test.vhdl 2>&1)
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: analysis crashed instead of reporting a clean error"
  exit 1
fi

if ! echo "$out" | grep -q "discrete range choice not allowed for target"; then
  echo "FAIL: expected the discrete-range-choice error"
  exit 1
fi

clean

echo "Test successful"
