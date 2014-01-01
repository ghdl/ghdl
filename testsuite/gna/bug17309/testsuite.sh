#! /bin/sh

. ../../testenv.sh

analyze polyamplib.vhdl master_testbench3.vhdl
elab_simulate master_testbench3 --stop-time=1ms

clean

echo "Test successful"
