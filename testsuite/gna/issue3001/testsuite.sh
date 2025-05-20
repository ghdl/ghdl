#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tt0_tb.vhdl
elab_simulate tt0_tb

clean

echo "Test successful"
