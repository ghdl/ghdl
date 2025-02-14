#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze test_realmin.vhdl
elab_simulate test_realmin

clean

echo "Test successful"
