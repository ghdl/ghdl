#! /bin/sh

. ../../testenv.sh

# Chained 'element'element on a nested (array-of-array) type used to crash
# with an internal ASSERT_FAILURE ("no field Element_Subtype") after
# already reporting a bogus "prefix must denote an object or a type"
# error. See issue2226.
export GHDL_STD_FLAGS="--std=08"

analyze test.vhdl

out=$(elab_simulate test 2>&1)
echo "$out"

if ! echo "$out" | grep -q "Array(1)(0) value is: 'Z'"; then
  echo "FAIL: expected initial value report"
  exit 1
fi

if ! echo "$out" | grep -q "Array(1)(0) value is: '1'"; then
  echo "FAIL: expected updated value report"
  exit 1
fi

clean

echo "Test successful"
