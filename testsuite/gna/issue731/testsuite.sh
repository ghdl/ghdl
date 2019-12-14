#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze adder.vhdl tbadder.vhdl
elab_simulate tbadder

clean

echo "Test successful"
