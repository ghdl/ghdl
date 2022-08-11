#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze abc.vhdl
analyze abc_tb.vhdl
elab_simulate abc_tb

analyze abc_tb1.vhdl
elab_simulate abc_tb1

analyze abc_tb2.vhdl
elab_simulate abc_tb2

analyze abc_tb3.vhdl
elab_simulate abc_tb3

clean

echo "Test successful"
