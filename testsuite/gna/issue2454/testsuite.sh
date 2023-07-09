#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze bug_test.vhdl
elab_simulate bug_test --stop-time=100ns

clean

echo "Test successful"
