#! /bin/sh

. ../../testenv.sh

analyze top_phystest_simple.vhdl
elab_simulate_failure top_physicaltest_simple

analyze tb2.vhdl
elab_simulate_failure tb2

clean

echo "Test successful"
