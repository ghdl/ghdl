#! /bin/sh

. ../../testenv.sh

analyze bug.vhdl
analyze repro.vhdl
elab_simulate ent

analyze_failure bug2.vhdl
clean

echo "Test successful"
