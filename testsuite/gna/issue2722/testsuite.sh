#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb.vhdl
elab_simulate ghdl_bug_2_tb

clean

echo "Test successful"
