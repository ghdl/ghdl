#! /bin/sh

. ../../testenv.sh

analyze cells.vhdl

analyze testbench_15993.vhdl
elab_simulate add_tb

analyze testbench.vhdl
elab_simulate add_tb

clean

echo "Test successful"
