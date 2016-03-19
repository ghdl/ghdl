#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="--std=08 -fpsl"

analyze test1.vhdl
elab_simulate psl_test_endpoint --psl-report=psl.out

grep -q '"cover-pass": 3' psl.out
rm psl.out

clean

echo "Test successful"
