#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze test.vhdl
elab_simulate test

analyze test1.vhdl
elab_simulate test1

clean

echo "Test successful"
