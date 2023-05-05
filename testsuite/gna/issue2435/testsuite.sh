#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze my_pkg.vhdl my_tb.vhdl
elab_simulate my_tb

clean

echo "Test successful"
