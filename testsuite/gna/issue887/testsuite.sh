#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze test2.vhdl
elab_simulate tb2

analyze test.vhdl
elab_simulate tb

clean

echo "Test successful"
