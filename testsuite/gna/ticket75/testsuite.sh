#! /bin/sh

. ../../testenv.sh

analyze bug.vhdl
elab_simulate_failure ent
clean

echo "Test successful"
