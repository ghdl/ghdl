#! /bin/sh

. ../../testenv.sh

analyze_failure bug.vhdl

analyze bug2.vhdl
elab_simulate_failure bug

clean

echo "Test successful"
