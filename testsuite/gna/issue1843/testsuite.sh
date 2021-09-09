#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze counter.vhdl
elab_simulate counter

clean

echo "Test successful"
