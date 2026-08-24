#! /bin/sh

. ../../testenv.sh

# A memory with an async (unclocked) write port used to crash --synth
# with an internal ASSERTION_ERROR (netlists-memories.adb:2057) instead
# of a clean diagnostic. See issue2079.
export GHDL_STD_FLAGS="--std=08"

# Use the helper from testenv.sh rather than calling $GHDL directly: it
# already assembles the flag variables, which have to stay unquoted so
# they split into separate arguments.
out=$(synth ent.vhdl -e 2>&1) || true
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: synth crashed instead of reporting a clean error"
  exit 1
fi

if ! echo "$out" | grep -q "latch infered"; then
  echo "FAIL: expected a clean latch-inference error"
  exit 1
fi

clean

echo "Test successful"
