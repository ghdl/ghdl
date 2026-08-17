#! /bin/sh

. ../../testenv.sh

# -i (import) registers units in the library WITHOUT analyzing them. Trying
# to -e (elaborate) an imported-but-never-analyzed unit used to crash with
# an internal TYPES.INTERNAL_ERROR instead of a clean "has not been
# analyzed" error. See issue1538.
echo "import tb.vhd cfuncs.vhd"
"$GHDL" -i $GHDL_STD_FLAGS $GHDL_FLAGS tb.vhd cfuncs.vhd

echo "elaborate (failure expected) tb"
if OUT=$("$GHDL" -e $GHDL_STD_FLAGS $GHDL_FLAGS tb 2>&1); then
  echo "$OUT"
  echo "FAIL: expected elaborating an imported-but-unanalyzed unit to fail"
  exit 1
fi
echo "$OUT"

if echo "$OUT" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: elaboration crashed instead of reporting a clean error"
  exit 1
fi

if ! echo "$OUT" | grep -q "has not been analyzed"; then
  echo "FAIL: expected a clean \"has not been analyzed\" error"
  exit 1
fi

clean

echo "Test successful"
