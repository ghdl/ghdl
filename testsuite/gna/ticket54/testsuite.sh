#! /bin/sh

. ../../testenv.sh

analyze test.vhdl
elab_simulate_failure test --assert-level=error
clean

echo "Test successful"
