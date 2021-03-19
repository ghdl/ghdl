#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze test2.vhdl
elab_simulate test

clean

echo "Test successful"
