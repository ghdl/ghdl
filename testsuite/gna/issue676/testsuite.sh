#! /bin/sh

#exit 0
. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze adder.vhdl
elab_simulate adder

clean

echo "Test successful"
