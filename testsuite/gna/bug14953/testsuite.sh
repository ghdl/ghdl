#! /bin/sh

. ../../testenv.sh

analyze bug.vhdl
elab_simulate_failure bug

analyze bug2.vhdl
elab_simulate_failure bug

clean

echo "Test successful"
