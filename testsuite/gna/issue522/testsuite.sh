#! /bin/sh

. ../../testenv.sh

analyze shifter.vhdl
analyze shifter_tb.vhdl
elab_simulate_failure shifter_tb

clean

echo "Test successful"
