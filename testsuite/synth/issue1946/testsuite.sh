#! /bin/sh

. ../../testenv.sh

# A process with a falling_edge branch containing no statements, followed
# by an elsif rising_edge branch that does have a signal assignment, used
# to crash --synth with an internal ASSERT_FAILURE
# (netlists-memories.adb:2575). See issue1946.
export GHDL_STD_FLAGS="--std=08"
out=$(synth circuit.vhd -e circuit 2>&1)
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: synth crashed"
  exit 1
fi

clean

echo "Test successful"
