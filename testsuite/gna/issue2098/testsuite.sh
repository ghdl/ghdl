#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

# Original test, incorrect
analyze_failure -Werror=elaboration test-orig.vhdl

# Modified v1
analyze -Werror=elaboration test2.vhdl
elab_simulate test2

clean

# Better modification
analyze test.vhdl
elab_simulate test

clean

echo "Test successful"
