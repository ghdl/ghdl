#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb_last_value_bug.vhdl
elab_simulate tb_last_value_bug --stop-time=50ns

clean

echo "Test successful"
