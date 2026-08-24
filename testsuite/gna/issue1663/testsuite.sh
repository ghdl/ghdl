#! /bin/sh

. ../../testenv.sh

# A large 2D array signal (300x280 8-bit elements, an image buffer)
# combined with --vcd wave dumping used to crash with an internal
# CONSTRAINT_ERROR (grt-vcd.adb:465 access check failed). The original
# report read the image from a file via VUnit; this exercises the same
# large-2D-array + --vcd combination without either dependency. See
# issue1663.
export GHDL_STD_FLAGS="--std=08"
analyze tb.vhd

out=$(elab_simulate tb --vcd=wave.vcd 2>&1)
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: crashed instead of completing"
  exit 1
fi

if [ ! -s wave.vcd ]; then
  echo "FAIL: expected a non-empty wave.vcd to be generated"
  exit 1
fi

rm -f wave.vcd
clean

echo "Test successful"
