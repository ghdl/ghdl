#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze top_tb.vhdl
elab_simulate top_tb

clean

echo "Test successful"
