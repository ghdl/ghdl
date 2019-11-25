#! /bin/sh

. ../../testenv.sh

analyze ex1_entity.vhdl ex1_top.vhdl
elab_simulate_failure ex1_top

clean

echo "Test successful"
