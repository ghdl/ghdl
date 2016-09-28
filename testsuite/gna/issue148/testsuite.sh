#! /bin/sh

. ../../testenv.sh

analyze bug_sim.vhdl
elab_failure bug_sim
clean

echo "Test successful"
