#! /bin/sh

. ../../testenv.sh

analyze range_tb1.vhdl
elab_simulate_failure range_tb1

analyze range_tb.vhdl
elab_simulate_failure range_tb

clean

echo "Test successful"
