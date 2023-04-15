#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze shift_register.vhdl
analyze shift_register_tb.vhdl
elab_simulate shift_register_tb

clean

echo "Test successful"
