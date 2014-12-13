#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="--std=02 -fpsl"
analyze psl_test_named_statement.vhd
elab_simulate psl_test_named_statement --stop-time=200ns 2>&1 | tee psl.out

if ! grep -q "psl assertion error" psl.out; then
  echo "missing psl error"
  exit 1
fi

clean
rm -f psl.out

echo "Test successful"
