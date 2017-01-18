#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze minimum_tb.vhdl
elab_simulate minimum_tb

analyze minimum_tb2.vhdl
elab_simulate minimum_tb2

clean

echo "Test successful"
