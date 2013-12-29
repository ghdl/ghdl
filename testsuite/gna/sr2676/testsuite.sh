#! /bin/sh

. ../../testenv.sh

analyze reset_types.vhdl reset.vhdl reset-rtl.vhdl reset-test.vhdl
elab_simulate reset_testbench --stop-time=1us

clean

echo "Test successful"
