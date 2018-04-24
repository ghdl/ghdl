#! /bin/sh

. ../../testenv.sh

analyze dut.vhdl
elab_simulate_failure tb

clean

echo "Test successful"
