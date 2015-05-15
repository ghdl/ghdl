#! /bin/sh

. ../../testenv.sh

analyze bug.vhdl
elab_simulate_failure ent

analyze bug1.vhdl
elab_simulate_failure ent1

analyze bug2.vhdl
elab_simulate ent2

clean

echo "Test successful"
