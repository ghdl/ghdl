#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb_last_value_bug.vhdl
elab_simulate tb_last_value_bug --stop-time=50ns

analyze tb2.vhdl
elab_simulate tb2

clean

export GHDL_STD_FLAGS=--std=87

analyze tb2.vhdl
elab_simulate_failure tb2

clean


echo "Test successful"
