#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS="--std=08 -Werror=sensitivity"
analyze test_rec.vhdl

analyze_failure test.vhdl

clean

echo "Test successful"
