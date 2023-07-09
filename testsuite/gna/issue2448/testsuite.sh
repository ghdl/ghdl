#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze debouncer.vhdl debouncer_no_vunit_not_ok_tb.vhdl
elab_simulate debouncer_no_vunit_not_ok_tb

clean

echo "Test successful"
