#! /bin/sh

. ../../testenv.sh

analyze bug.vhdl
elab_simulate_failure bug

clean

echo "Test successful"
