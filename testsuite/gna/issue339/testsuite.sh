#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze test_pkg.vhdl test_bench.vhdl
elab_simulate test_bench --stop-time=700ns

clean

echo "Test successful"
