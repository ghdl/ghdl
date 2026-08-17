#! /bin/sh

. ../../testenv.sh

# A latched signal that is assigned to an output port only *indirectly*
# (through a plain concurrent signal assignment, not driven straight from
# the process) used to slip past the "latch inferred" check that a
# directly-latched output port would trigger, silently synthesizing as a
# constant 'X' driver instead of failing. See issue1501.
echo "try to synthesize (failure expected) ent.vhd -e"
if OUT=$("$GHDL" --synth $GHDL_STD_FLAGS $GHDL_FLAGS ent.vhd -e 2>&1); then
  echo "$OUT"
  echo "FAIL: expected synthesis to fail reporting the inferred latch"
  exit 1
fi
echo "$OUT"

if ! echo "$OUT" | grep -q "latch infered for net \"res\""; then
  echo "FAIL: expected a \"latch infered\" error for net \"res\""
  exit 1
fi

clean

echo "Test successful"
