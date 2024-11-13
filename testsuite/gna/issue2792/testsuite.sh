#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze mem_tb.vhdl
elab_simulate mem_tb

analyze mem_tb2.vhdl
elab_simulate mem_tb2

clean

echo "Test successful"
