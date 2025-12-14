#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze rams_init_file.vhdl rams_init_file_tb.vhdl
elab_simulate_failure rams_init_file_tb

clean

echo "Test successful"
